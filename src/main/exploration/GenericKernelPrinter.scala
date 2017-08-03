package exploration

import java.io.{File, IOException}
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger
import exploration.ExpressionFilter.Status.Success
import ir.ast.{Expr, FunCall, Lambda, Param}
import ir.{ArrayType, Type, TypeChecker}
import lift.arithmetic._
import opencl.executor.Eval
import opencl.generator.{NDRange, OpenCLGenerator}
import opencl.ir.pattern._
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._
import rewriting.utils.Utils

import scala.collection.immutable.Map
import scala.io.Source
import scala.sys.process._
import scala.util.Random

/**
  * This main currently runs a parameter space exploration over the
  * serialized low level expressions.
  */
object GenericKernelPrinter {

  private val logger = Logger(this.getClass)

  private var topFolder = ""

  private val parser = new ArgotParser("ParameterRewrite")

  parser.flag[Boolean](List("h", "help"),
    "Show this message.") {
    (sValue, _) =>
      parser.usage()
      sValue
  }

  private val input = parser.parameter[String]("input",
    "Input file containing the lambda to use for rewriting",
    optional = false) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage("Input file \"" + s + "\" does not exist")
      s
  }

  private[exploration] val defaultSpeedup = 1.0f
  private[exploration] val defaultConfigsToAchieveSpeedup = 100
  private[exploration] val defaultTimeoutInSeconds = 60
  private[exploration] val defaultMaxConfigs = 500
  private[exploration] val defaultMaxLocalSize = 512
  private[exploration] val defaultVendor = "Nvidia"
  private[exploration] val defaultDeviceType = "GPU"
  private[exploration] val defaultDeviceId = 0

  protected[exploration] val speedup = parser.option[Float](List("speedup"), "s",
    s"Specifies Speedup to achieve within 'configsToAchieveSpeedup'-many configs (default: $defaultSpeedup)")

  protected[exploration] val configsToAchieveSpeedup = parser.option[Int](List("configsToAchieveSpeedup"), "c",
    s"Specifies how many configs to try to achieve 'speedup' (default: $defaultConfigsToAchieveSpeedup)")

  protected[exploration] val timeoutInSeconds = parser.option[Int](List("timeout"), "t",
    s"Abort tuning after t seconds (default: $defaultTimeoutInSeconds)")

  protected[exploration] val maxConfigs = parser.option[Int](List("maxConfigs"), "m",
    s"Abort tuning after trying m configs (default: $defaultMaxConfigs)")

  protected[exploration] val maxLocalSize = parser.option[Int](List("maxLocalSize"), "l",
    s"Specifies max local size per dimension for device - see clinfo (default: $defaultMaxLocalSize)")

  protected[exploration] val vendor = parser.option[String](List("vendor"), "v",
    s"Specifies which OpenCL platform to use (matches substring) (default: $defaultVendor)")

  protected[exploration] val deviceType = parser.option[String](List("deviceType"), "d",
    s"Specifies which OpenCL device type to use (default: $defaultDeviceType)")

  protected[exploration] val deviceId = parser.option[Int](List("deviceId"), "i",
    s"Specifies ID of the OpenCL device to use (default: $defaultDeviceId)")

  private val settingsFile = parser.option[String](List("f", "file"), "name",
    "The settings file to use."
  ) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage(s"Settings file $file doesn't exist.")
      s
  }

  private[exploration] var settings = Settings()

  private var lambdaFilename = ""

  def main(args: Array[String]): Unit = {

    try {

      parser.parse(args)
      settings = ParseSettings(settingsFile.value)
      val config = settings.genericKernelPrinterSettings

      logger.info(s"Arguments: ${args.mkString(" ")}")
      val inputArgument = input.value.get

      topFolder = Paths.get(inputArgument).toString

      lambdaFilename = topFolder + "Scala/lambdaFile"


      logger.info(s"Arguments: ${args.mkString(" ")}")
      logger.info(s"Settings:\n$settings")

      // list all the high level expression
      val all_files = Source.fromFile(s"$topFolder/index").getLines().toList
      val highLevelCount = all_files.size

      val parentFolder = Paths.get(topFolder).toAbsolutePath.getParent

      var expr_counter = 0
      all_files.foreach(filename => {

        val fullFilename = parentFolder + "/" + filename

        if (Files.exists(Paths.get(fullFilename))) {
          val high_level_hash = filename.split("/").last
          expr_counter = expr_counter + 1
          println(s"High-level expression : $expr_counter / $highLevelCount")

          try {

            val high_level_expr_orig = readLambdaFromFile(fullFilename)

            val vars = high_level_expr_orig.getVarsInParams()

            val combinations = settings.inputCombinations

            val st =
              if (combinations.isDefined &&
                combinations.get.head.length == vars.length)
                (vars: Seq[ArithExpr], combinations.get.head).zipped.toMap
              else
                createValueMap(high_level_expr_orig)

            val sizesForFilter = st.values.toSeq

            val high_level_expr = replaceInputTypes(high_level_expr_orig, st)

            TypeChecker(high_level_expr)

            val loweredIndex = s"${topFolder}Lower/$high_level_hash/index"
            if (Files.exists(Paths.get(loweredIndex))) {

              val lowLevelExprList = Source.fromFile(loweredIndex).getLines().toList

              val lowLevelCounter = new AtomicInteger()
              val lowLevelCount = lowLevelExprList.size
              val propagationCounter = new AtomicInteger()
              val kernelCounter = new AtomicInteger()
              println(s"Found $lowLevelCount low level expressions")

              //val parList = if (config.sequential) lowLevelExprList else lowLevelExprList.par
              // todo add sequential command line arg
              val parList = lowLevelExprList.par

              parList.foreach(low_level_filename => {

                try {

                  val low_level_hash = low_level_filename.split("/").last
                  val fullLowLevelFilename = parentFolder + "/" + low_level_filename
                  val low_level_str = readFromFile(fullLowLevelFilename)
                  val low_level_factory = Eval.getMethod(low_level_str)

                  lowLevelCounter.incrementAndGet()

                  def countSubstring(str: String, substr: String) = substr.r.findAllMatchIn(str).length

                  // how many tuning parameters are there?
                  val tpCount = countSubstring(low_level_str, "variables") - vars.length - 1
                  val tuningParameters = Seq.fill(tpCount)(TuningParameter())

                  val expr = low_level_factory(vars ++ tuningParameters)
                  TypeChecker(expr)


                  val kernel = opencl.executor.Compile(expr)

                  def replaceOCLFunctions(kernel: String, fun: String, tuningParameter: String): String = {
                    kernel.replaceAll(
                      fun + """\(0\)""",
                      s"""${tuningParameter}_0""").replaceAll(
                      fun + """\(1\)""",
                      s"""${tuningParameter}_1""").replaceAll(
                      fun + """\(2\)""",
                      s"""${tuningParameter}_2""")
                  }

                  def introduceOCLTuningParameters(kernel: String): String = {
                    val global_size = replaceOCLFunctions(kernel, "get_global_size", "GLOBAL_SIZE")
                    val local_size = replaceOCLFunctions(global_size, "get_local_size", "LOCAL_SIZE")
                    replaceOCLFunctions(local_size, "get_num_groups", "NUM_GROUPS")
                  }

                  val genericKernel = introduceOCLTuningParameters(kernel)
                  //if(low_level_hash == "974323ee359506c482e957a975b7837f54f1e0f25b23b2d0b1fa1b061aacfc6a") {
                  //  println(genericKernel)
                  //}

                  val sb = new java.lang.StringBuilder

                  // todo (@bastian) currently only uses the first input combination
                  // add atf vars
                  assert(vars.length == combinations.get.head.length)
                  val varsWithSizes = vars.zip(combinations.get.head)
                  varsWithSizes.foreach(x => {
                    sb.append(s"""#atf::var<int> ${x._1.toString} = ${x._2.toString}\n""")
                  })
                  sb.append("\n")

                  // add search technique and abort condition
                  val searchTechnique = "atf::open_tuner"
                  sb.append(s"""#atf::search_technique \"$searchTechnique\"\n""")

                  val abortCondition =
                    s"atf::cond::speedup(${config.speedup},${config.configsToAchieveSpeedup}) || " +
                    s"atf::cond::duration<::std::chrono::seconds>(${config.timeoutInSeconds}) || " +
                    s"atf::cond::evaluations(${config.maxConfigs})"
                  sb.append(s"""#atf::abort_condition \"$abortCondition\"\n""")
                  sb.append("\n")

                  // add global size directives
                  // find out which map loops over which input dimension
                  def globalSizeInputVarMappedOver(v: Var, d: Int): (String, ArithExpr) = {
                    val inputVarsMappedOver = v.range.max.varList.intersect(vars.toSet)
                    assert(inputVarsMappedOver.size == 1)
                    (s"GLOBAL_SIZE_$d", inputVarsMappedOver.head)
                  }

                  var globalSizeMaxValues: Set[(String, ArithExpr)] = Set()
                  Expr.visit(expr.body, {
                    case FunCall(f@MapGlb(dim, _), _) => {
                      globalSizeMaxValues += globalSizeInputVarMappedOver(f.loopVar, dim)
                    }

                    case FunCall(f@MapWrg(dim, _), _) => {
                      globalSizeMaxValues += globalSizeInputVarMappedOver(f.loopVar, dim)
                    }

                    case FunCall(f@MapAtomWrg(dim, _, _), _) => {
                      globalSizeMaxValues += globalSizeInputVarMappedOver(f.loopVar, dim)
                    }

                    case _ =>
                  }, (_) => Unit)

                  globalSizeMaxValues.foreach(x => {
                    //todo maybe add divides constraint for global sizes
                    sb.append(s"""#atf::tp name \"${x._1}\" type \"int\" range \"atf::interval<int>(1,${x._2})"\n""")
                  })

                  // add local size directives
                  val usedDimensions = globalSizeMaxValues.size
                  assert(usedDimensions <= 3)

                  val localSizeDirectives = Seq.tabulate[String](usedDimensions)(
                    i => s"""#atf::tp name \"LOCAL_SIZE_$i\" \\\n type \"int\" \\\n range \"atf::interval<int>(1,${config.maxLocalSize})" \\\n constraint \"atf::divides(GLOBAL_SIZE_$i)\"\n""")
                  localSizeDirectives.foreach(x => sb.append(x))

                  // add tuning parameter directives
                  val allTuningParams = ParameterSearch.getTunableSplitsAndSlides(expr).filter(_._1.isInstanceOf[TuningParameter])
                  val elemT = Type.getValueType(expr.params.head.t)

                  allTuningParams.foreach(x => {
                    val tpName = x._1.toString
                    val divides = x._2
                    sb.append(s"""#atf::tp name \"$tpName\" \\\n type \"$elemT\" \\\n range \"atf::interval<$elemT>(1,$divides)" \\\n constraint \"atf::divides($divides)\"\n""")
                  })
                  sb.append("\n")

                  // add vendor directives
                  sb.append(s"""#atf::ocl::device_info vendor \"${config.vendor}\" type \"${config.deviceType}\" id ${config.deviceId}\n""")
                  sb.append("\n")

                  // add kernel input directives
                  expr.params.foreach(x => {
                    val size = Type.getElementCount(x.t)
                    val elemT = Type.getValueType(x.t)
                    sb.append(s"""#atf::ocl::input \"atf::buffer<$elemT>($size)\"\n""")
                  })
                  // output
                  val outputType = Type.getValueType(high_level_expr.body.t)
                  //val outputLengths2 = Type.getElementCount(high_level_expr_orig.body.t)
                  sb.append(s"""#atf::ocl::input \"atf::buffer<$outputType>(${Type.getElementCount(expr.body.t)})\"\n""")
                  vars.map(x => x.toString).sorted.foreach(x =>
                    sb.append(s"""#atf::ocl::input \"atf::scalar<int>($x)\"\n""")
                  )

                  // add kernel name and gs/ls directives
                  sb.append("#atf::ocl::kernel_name \"KERNEL\"\n")
                  //val usedGlobalSizes = globalSizeMaxValues.map(x => x._1).toList.sorted
                  val gsDirective = Seq.tabulate[String](usedDimensions)(i => s"GLOBAL_SIZE_$i")
                  val lsDirective = Seq.tabulate[String](usedDimensions)(i => s"LOCAL_SIZE_$i")
                  sb.append(s"""#atf::ocl::ls \"${lsDirective.mkString(", ")}\"\n""")
                  sb.append(s"""#atf::ocl::gs \"${gsDirective.mkString(", ")}\"\n""")
                  sb.append("\n")

                  // add NUM_GROUPS macro
                  if (genericKernel.contains("NUM_GROUPS")) {
                    val num_groups = Seq.tabulate[String](usedDimensions)(i =>
                      s"#define NUM_GROUPS_$i (GLOBAL_SIZE_$i / LOCAL_SIZE_$i)"
                    )
                    sb.append(num_groups.mkString("\n"))
                  }

                  val kernelWithDirectives = sb.toString + "\n" + genericKernel
                  //println(kernelWithDirectives)

                  // generate folder structure
                  s"mkdir -p ${topFolder}Cl/".!

                  val finalKernel =
                    s"""
                       |// High-level hash: $high_level_hash
                       |// Low-level hash: $low_level_hash
                       |
                       |$kernelWithDirectives
                       |""".stripMargin

                  //println(finalKernel)
                  val path = s"${topFolder}Cl"
                  val filename = low_level_hash + ".cl"
                  Utils.dumpToFile(finalKernel, filename, path)

                } catch {
                  case t: Throwable =>
                    // Failed reading file or similar.
                    //logger.warn(t.toString)
                    println(s"FAILED: ${t.toString}")
                } //&& substitutionCount < 800000
              })
              println(s"\nGenerated $kernelCounter kernels")
            }
          } catch {
            case t: Throwable => {
              println(t.toString)
              logger.warn(t.toString)
            }
          }
        }
      })
    } catch {
      case io: IOException =>
        logger.error(io.toString)
      case e: ArgotUsageException =>
        println(e.message)
    }
  }

  def readFromFile(filename: String) =
    Source.fromFile(filename).mkString

  def readLambdaFromFile(filename: String) =
    Eval(readFromFile(filename))

  def createValueMap(lambda: Lambda, sizes: Seq[ArithExpr] = Seq()): Map[ArithExpr, ArithExpr] = {
    val vars = lambda.getVarsInParams()

    val actualSizes: Seq[ArithExpr] =
      if (sizes.isEmpty) Seq.fill(vars.length)(settings.searchParameters.defaultInputSize)
      else sizes

    (vars, actualSizes).zipped.toMap
  }

  def replaceInputTypes(lambda: Lambda, st: Map[ArithExpr, ArithExpr]): Lambda = {
    val tunable_nodes = Utils.findTunableNodes(lambda).reverse
    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))
    Utils.quickAndDirtySubstitution(st, tunable_nodes, lambda)
  }
}
