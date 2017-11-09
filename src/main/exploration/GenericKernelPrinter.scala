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
  * This main generates a tunable kernel from a
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

  private[exploration] val defaultEnableSpeedupCondition = true
  private[exploration] val defaultSpeedup = 1.0f
  private[exploration] val defaultConfigsToAchieveSpeedup = 100
  private[exploration] val defaultEnableTimeoutCondition = true
  private[exploration] val defaultTimeoutInSeconds = 60
  private[exploration] val defaultEnableMaxConfigsCondition = false
  private[exploration] val defaultMaxConfigs = 500
  private[exploration] val defaultEnableNotBetterThanCondition = false
  private[exploration] val defaultNotBetterThanInNs = 60000000 // 60s
  private[exploration] val defaultNotBetterWithinConfigs = 500
  private[exploration] val defaultVendor = "Nvidia"
  private[exploration] val defaultDeviceType = "GPU"
  private[exploration] val defaultDeviceId = 0

  protected[exploration] val enableSpeedupCondition: FlagOption[Boolean] = parser.flag[Boolean](List("es", "enableSpeedupCondition"),
    s"Use atf abort condition 'speedup' (default: $defaultEnableSpeedupCondition)")

  protected[exploration] val speedup = parser.option[Float](List("speedup"), "s",
    s"Specifies Speedup to achieve within 'configsToAchieveSpeedup'-many configs (default: $defaultSpeedup)")

  protected[exploration] val configsToAchieveSpeedup = parser.option[Int](List("configsToAchieveSpeedup"), "c",
    s"Specifies how many configs to try to achieve 'speedup' (default: $defaultConfigsToAchieveSpeedup)")

  protected[exploration] val enableTimeoutCondition: FlagOption[Boolean] = parser.flag[Boolean](List("et", "enableTimeoutCondition"),
    s"Use atf abort condition 'timeout' (default: $defaultEnableTimeoutCondition)")

  protected[exploration] val timeoutInSeconds = parser.option[Int](List("timeout"), "t",
    s"Abort tuning after t seconds (default: $defaultTimeoutInSeconds)")

  protected[exploration] val enableMaxConfigsCondition: FlagOption[Boolean] = parser.flag[Boolean](List("em", "enableMaxConfigsCondition"),
    s"Use atf abort condition 'max_configs' (default: $defaultEnableMaxConfigsCondition)")

  protected[exploration] val maxConfigs = parser.option[Int](List("maxConfigs"), "m",
    s"Abort tuning after trying m configs (default: $defaultMaxConfigs)")

  protected[exploration] val enableNotBetterThanCondition: FlagOption[Boolean] = parser.flag[Boolean](List("em", "enableNotBetterThanCondition"),
    s"Use atf abort condition 'abort_when_not_better' (default: $defaultEnableNotBetterThanCondition)")

  protected[exploration] val notBetterThanInNs = parser.option[Int](List("notBetterThan"), "nbt",
    s"Sets the bar for 'abort_when_not_better' condition (default: $defaultNotBetterThanInNs)")

  protected[exploration] val notBetterWithinConfigs = parser.option[Int](List("notBetterWithinConfigs"), "nbwc",
    s"How many configs to try to get better than 'notBetterThan' (default: $defaultNotBetterWithinConfigs)")

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

      if((config.enableSpeedupCondition ||
        config.enableTimeoutCondition ||
        config.enableMaxConfigsCondition ||
        config.enableNotBetterThanCondition) == false)
        scala.sys.error("No abort-condition defined")

      logger.info(s"Arguments: ${args.mkString(" ")}")
      logger.info(s"Settings:\n$settings")

      val inputArgument = input.value.get
      topFolder = Paths.get(inputArgument).toString

      // find out available tuning time for each kernel: timeOutInSeconds / #liftKernels
      val highLevelFiles = Source.fromFile(s"$topFolder/index").getLines().toList
      val highLevelCount = highLevelFiles.size
      val totalLowLevelCount = highLevelFiles.map(filename => {
        val high_level_hash = filename.split("/").last
        val loweredIndex = s"${topFolder}Lower/$high_level_hash/index"

        if (Files.exists(Paths.get(loweredIndex)))
          Source.fromFile(loweredIndex).getLines().toList.size
        else
          0
      }).sum

      if (config.enableTimeoutCondition) {
        val message = "Seconds to tune a single lift kernel: " + config.timeoutInSeconds / totalLowLevelCount
        logger.info(message)
        //println(message)
      }

      val parentFolder = Paths.get(topFolder).toAbsolutePath.getParent
      var expr_counter = 0
      highLevelFiles.foreach(filename => {

        val fullFilename = parentFolder + "/" + filename

        if (Files.exists(Paths.get(fullFilename))) {
          val high_level_hash = filename.split("/").last
          expr_counter = expr_counter + 1
          println(s"High-level expression : $expr_counter / $highLevelCount")

          try {
            val high_level_expr_orig = readLambdaFromFile(fullFilename)
            val inputVars = high_level_expr_orig.getVarsInParams()

            val combinations = settings.inputCombinations
            /*
            val high_level_expr = if (combinations.isDefined &&
              combinations.get.head.length == inputVars.length)
              replaceInputTypes(high_level_expr_orig, (inputVars: Seq[ArithExpr], combinations.get.head).zipped.toMap)
            else
              high_level_expr_orig
              */
            val high_level_expr = high_level_expr_orig

            TypeChecker(high_level_expr)

            val loweredIndex = s"${topFolder}Lower/$high_level_hash/index"
            if (Files.exists(Paths.get(loweredIndex))) {

              val lowLevelExprList = Source.fromFile(loweredIndex).getLines().toList
              val lowLevelExpressionCounter = new AtomicInteger()
              val lowLevelCount = lowLevelExprList.size
              val kernelCounter = new AtomicInteger()
              println(s"Found $lowLevelCount low level expressions")

              lowLevelExprList.par.foreach(lowLevelFilename => {

                try {
                  val lowLevelHash = lowLevelFilename.split("/").last
                  val fullLowLevelFilename = parentFolder + "/" + lowLevelFilename
                  val lowLevelString = readFromFile(fullLowLevelFilename)
                  val lowLevelFactory = Eval.getMethod(lowLevelString)
                  lowLevelExpressionCounter.incrementAndGet()

                  /*
                      How many tuning parameters are in the low-level expression?
                      example lowLevelString:

                      (variables: Seq[ArithExpr]) => {
                        val v_M_1 = variables(0)
                        val v__0 = variables(1)
                        val v__1 = variables(2)
                        ... (expression down here)

                      Tuning parameters are all 'variables' besides the input inputVars, in this case
                      v__0 and v__1. We subtract inputVars.length and 1 for the first occurrence of 'variables'
                   */
                  def countSubstring(str: String, substr: String) = substr.r.findAllMatchIn(str).length

                  val tuningParameterCount = countSubstring(lowLevelString, "variables") - inputVars.length - 1
                  val tuningParameters = Seq.fill(tuningParameterCount)(TuningParameter())

                  val expr = lowLevelFactory(inputVars ++ tuningParameters)
                  TypeChecker(expr)

                  val kernel = opencl.executor.Compile(expr)

                  // replace opencl function calls with tuning parameters
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

                  val tunableKernel = introduceOCLTuningParameters(kernel)
                  val kernelWithATFMacros = addATFMacros(tunableKernel, high_level_expr_orig, expr, settings, totalLowLevelCount)

                  // generate folder structure
                  s"mkdir -p ${topFolder}Cl/".!

                  val finalKernel =
                    s"""
                       |// High-level hash: $high_level_hash
                       |// Low-level hash: $lowLevelHash
                       |
                       |$kernelWithATFMacros
                       |""".stripMargin

                  //println(finalKernel)
                  val path = s"${topFolder}Cl"
                  val filename = lowLevelHash + ".cl"
                  Utils.dumpToFile(finalKernel, filename, path)
                  kernelCounter.incrementAndGet()

                } catch {
                  case t: Throwable =>
                    // Failed reading file or similar.
                    logger.warn(t.toString)
                    println(s"FAILED: ${t.toString}")
                }
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

  def replaceInputTypes(lambda: Lambda, st: Map[ArithExpr, ArithExpr]): Lambda = {
    val tunable_nodes = Utils.findTunableNodes(lambda).reverse
    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))
    Utils.quickAndDirtySubstitution(st, tunable_nodes, lambda)
  }

  def addATFMacros(kernel: String,
                   highLevelExpr: Lambda,
                   lowLevelExpr: Lambda,
                   settings: Settings,
                   totalLowLevelCount: Int): String = {
    val inputVars = highLevelExpr.getVarsInParams()
    val config = settings.genericKernelPrinterSettings
    val combinations = settings.inputCombinations

    val atfMacros = new java.lang.StringBuilder

    // add atf input directives
    // todo (@bastian) currently only uses the first input combination from config
    if (inputVars.nonEmpty) {
      assert(inputVars.length == combinations.get.head.length)
      val varsWithSizes = inputVars.zip(combinations.get.head)
      varsWithSizes.foreach(x => {
        atfMacros.append(s"""#atf::var<int> ${x._1.toString} = ${x._2.toString}\n""")
      })
      atfMacros.append("\n")
    }

    // add search technique
    val searchTechnique = "atf::open_tuner"
    atfMacros.append(s"""#atf::search_technique \"$searchTechnique\"\n""")

    // add abort-condition
    var abortCondition = ""
    if (config.enableSpeedupCondition)
      abortCondition += s"atf::cond::speedup(${config.speedup},${config.configsToAchieveSpeedup}) || "
    if (config.enableTimeoutCondition)
      abortCondition += s"atf::cond::duration<::std::chrono::seconds>(${config.timeoutInSeconds / totalLowLevelCount}) || "
    if (config.enableMaxConfigsCondition)
      abortCondition += s"atf::cond::evaluations(${config.maxConfigs}) || "
    if (config.enableNotBetterThanCondition)
      abortCondition += s"atf::cond::abort_when_not_better(${config.notBetterThanInNs}, ${config.notBetterWithinConfigs}) || "

    abortCondition = abortCondition.dropRight(4) // drop last ' || '
    atfMacros.append(s"""#atf::abort_condition \"$abortCondition\"\n""")
    atfMacros.append("\n")

    // add tuning parameter macros
    val allTuningParams = ParameterSearch.getTunableSplitsAndSlides(lowLevelExpr).filter(_._1.isInstanceOf[TuningParameter]).distinct
    allTuningParams.foreach(tp => {
      // tp is a tuple (TuningParameter, ArithExpr), where tp._2 specifies implicitly that the TP needs
      // to divide the ArithExpr since it is contained in a Split or a Slide
      val tpName = tp._1.toString
      val divides = tp._2
      atfMacros.append(s"""#atf::tp name \"$tpName\" type \"int\" range \"atf::interval<int>(1,$divides)" constraint \"atf::divides($divides)\"\n""")
    })
    atfMacros.append("\n")

    // add global-size tuning parameter for replaced opencl functions
    // global-/local-size tps are used in the loops generated for MapGlb and MapLcl
    // find out which map loops over which input dimension to define tps correctly
    def globalSizeInputVarMappedOver(v: Var, d: Int): (String, ArithExpr) = {
      val inputVarsMappedOver = v.range.max.varList.toSet.intersect(inputVars.toSet)
      // usually we try to set the max global_size as high as the number of elements of the input
      // in that dimension therefore we use the var as max
      if (inputVarsMappedOver.size == 1)
        (s"GLOBAL_SIZE_$d", inputVarsMappedOver.head)
      else
      // if there is no var (e.g., a fixed-input size array is given) the max number of threads we
      // start is the size of the array in the according dimension taken from the config
        (s"GLOBAL_SIZE_$d", combinations.get.head(d)) // todo can we actually use this above too?
    }

    var globalSizeMaxValues: Set[(String, ArithExpr)] = Set()
    Expr.visit(lowLevelExpr.body, {
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
      // global sizes are constrained to be powers of two
      atfMacros.append(s"""#atf::tp name \"${x._1}\" type \"int\" range \"atf::interval<int>(1,${x._2})" constraint "[&](auto ${x._1}){ return (${x._1} & (${x._1} - 1)) == 0; }"\n""")
    })

    // add local size directives
    val usedDimensions = globalSizeMaxValues.size
    assert(usedDimensions <= 3)

    val localSizeMacros = Seq.tabulate[String](usedDimensions)(
      i => s"""#atf::tp name \"LOCAL_SIZE_$i\" type \"int\" range \"atf::interval<int>(1,${settings.searchParameters.maxLocalSize})" constraint \"atf::divides(GLOBAL_SIZE_$i)\"\n"""
    )
    localSizeMacros.foreach(x => atfMacros.append(x))

    // add vendor macros
    atfMacros.append(s"""#atf::ocl::device_info vendor \"${config.vendor}\" type \"${config.deviceType}\" id ${config.deviceId}\n""")
    atfMacros.append("\n")

    // add kernel input macros
    lowLevelExpr.params.foreach(x => {
      val size = Type.getElementCount(x.t)
      val elemT = Type.getValueType(x.t)
      atfMacros.append(s"""#atf::ocl::input \"atf::buffer<$elemT>($size)\"\n""")
    })
    // output
    //val outputType = Type.getValueType(high_level_expr.body.t)
    val outputType = Type.getValueType(highLevelExpr.body.t)
    atfMacros.append(s"""#atf::ocl::input \"atf::buffer<$outputType>(${Type.getElementCount(lowLevelExpr.body.t)})\"\n""")
    inputVars.map(x => x.toString).sorted.foreach(x =>
      atfMacros.append(s"""#atf::ocl::input \"atf::scalar<int>($x)\"\n""")
    )

    // add kernel name and gs/ls macros
    atfMacros.append("#atf::ocl::kernel_name \"KERNEL\"\n")
    val gsMacros = Seq.tabulate[String](usedDimensions)(i => s"GLOBAL_SIZE_$i")
    val lsMacros = Seq.tabulate[String](usedDimensions)(i => s"LOCAL_SIZE_$i")
    atfMacros.append(s"""#atf::ocl::ls \"${lsMacros.mkString(", ")}\"\n""")
    atfMacros.append(s"""#atf::ocl::gs \"${gsMacros.mkString(", ")}\"\n""")
    atfMacros.append("\n")

    // add NUM_GROUPS macro
    if (kernel.contains("NUM_GROUPS")) {
      val num_groups = Seq.tabulate[String](usedDimensions)(i =>
        s"#define NUM_GROUPS_$i (GLOBAL_SIZE_$i / LOCAL_SIZE_$i)"
      )
      atfMacros.append(num_groups.mkString("\n"))
    }

    val kernelWithATFMacros = atfMacros.toString + "\n" + kernel
    //println(kernelWithATFMacros)
    kernelWithATFMacros
  }
}
