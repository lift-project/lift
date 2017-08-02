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

  private[exploration] val defaultExploreNDRange = false
  private[exploration] val defaultSampleNDRange = -1
  private[exploration] val defaultDisableNDRangeInjection = false
  private[exploration] val defaultSequential = false
  private[exploration] val defaultGenerateScala = false

  protected[exploration] val exploreNDRange = parser.flag[Boolean](List("e", "exploreNDRange"),
    s"Explore global and local sizes instead of inferring (default: $defaultExploreNDRange)")

  protected[exploration] val sampleNDRange = parser.option[Int](List("sampleNDRange"), "n",
    s"Randomly sample n combinations of global and local sizes (requires 'explore') (default: $defaultSampleNDRange)")

  protected[exploration] val disableNDRangeInjection = parser.flag[Boolean](List("disableNDRangeInjection"),
    s"Don't inject NDRanges while compiling the OpenCL Kernel (default: $defaultDisableNDRangeInjection)")

  protected[exploration] val sequential = parser.flag[Boolean](List("s", "seq", "sequential"),
    s"Don't execute in parallel (default: $defaultSequential)")

  protected[exploration] val generateScala = parser.flag[Boolean](List("generate-scala"),
    s"Generate lambdas in Scala as well as in OpenCL (default: $defaultGenerateScala)")


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
      val config = settings.parameterRewriteSettings
      if (!config.exploreNDRange && config.sampleNDRange > 0)
        throw new RuntimeException("'sample' is defined without enabling 'explore'")

      logger.info(s"Arguments: ${args.mkString(" ")}")
      val inputArgument = input.value.get

      topFolder = Paths.get(inputArgument).toString

      lambdaFilename = topFolder + "Scala/lambdaFile"

      if (config.generateScala) {
        val f = new File(lambdaFilename)
        if (f.exists()) {
          f.delete()
        } else {
          s"mkdir -p ${topFolder}Scala".!
        }
      }


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

              val parList = if (config.sequential) lowLevelExprList else lowLevelExprList.par

              parList.foreach(low_level_filename => {

                try {

                  val low_level_hash = low_level_filename.split("/").last
                  val fullLowLevelFilename = parentFolder + "/" + low_level_filename
                  val low_level_str = readFromFile(fullLowLevelFilename)
                  val low_level_factory = Eval.getMethod(low_level_str)

                  lowLevelCounter.incrementAndGet()

                  def countSubstring( str:String, substr:String ) = substr.r.findAllMatchIn(str).length
                  // how many tuning parameters are there?
                  val tpCount = countSubstring(low_level_str, "variables") - vars.length - 1
                  val tuningParameters = Seq.fill(tpCount)(TuningParameter())

                  val expr = low_level_factory(vars ++ tuningParameters)
                      TypeChecker(expr)


                  val kernel = opencl.executor.Compile(expr)

                  def replaceOCLFunctions(kernel: String, fun: String, tuningParameter: String): String = {
                    kernel.replaceAll(
                      fun + """\(0\)""", s"""${tuningParameter}_0""").replaceAll(
                      fun + """\(1\)""", s"""${tuningParameter}_1""").replaceAll(
                      fun + """\(2\)""", s"""${tuningParameter}_2""")
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
                  // todo read from config and/or config file
                  val searchTechnique = "atf::open_tuner"
                  val abortCondition = "atf::cond::speedup(1,100)"
                  sb.append(s"""#atf::search_technique \"$searchTechnique\"\n""")
                  sb.append(s"""#atf::abort_condition \"$abortCondition\"\n""")
                  sb.append("\n")

                  // add global size directives
                  // find out which map loops over which input dimension
                  def globalSizeInputVarMappedOver(v: Var, d: Int) : (String, ArithExpr) = {
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
                  // todo get max local size param
                  val maxLocalSize = 512
                  val usedDimensions = globalSizeMaxValues.size
                  assert(usedDimensions <= 3)

                  val localSizeDirectives = Seq.tabulate[String](usedDimensions)(
                    i => s"""#atf::tp name \"LOCAL_SIZE_$i\" \\\n type \"int\" \\\n range \"atf::interval<int>(1,$maxLocalSize)" \\\n constraint \"atf::divides(GLOBAL_SIZE_$i)\"\n""")
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
                  // todo get them from command line and/or config file
                  val vendor = "Intel"
                  val deviceType = "GPU"
                  val deviceId = 0
                  sb.append(s"""#atf::ocl::device_info vendor \"$vendor\" type \"$deviceType\" id $deviceId\n""")
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

                  val kernelWithDirectives = sb.toString + "\n" + genericKernel
                  println(kernelWithDirectives)

                  /*
                  // add input buffer directive //todo change it to buffer directive
                  sb.setLength(0)
                  expr.params.foreach(x => {
                    val elemT = Type.getValueType(x.t)
                    sb.append(s"#atf::var<$elemT> ${x.mem.variable} = 1234")
                  })
                  val kernelWithATFVars = sb.toString + "\n" + kernelWithVars
                  */

                } catch {
                  case t: Throwable =>
                    // Failed reading file or similar.
                    //logger.warn(t.toString)
                    println(s"failed: ${t.toString}")
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

  def saveScala(expressions: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))], hashes: Seq[Option[String]]): Unit = {
    val filename = lambdaFilename
    val file = scala.tools.nsc.io.File(filename)

    (expressions, hashes).zipped.foreach((f, hash) => {

      try {
        val stringRep = "{ " + Utils.dumpLambdaToString(f._1).replace("\n", "; ") + "}"

        val sha256 = hash.get

        synchronized {
          file.appendAll("(\"" + sha256 + "\",  " + s"Array($stringRep)) ,\n")
        }
      } catch {
        case t: Throwable =>
          logger.warn(t.toString)
      }
    })
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
