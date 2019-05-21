package exploration

import java.io.{File, IOException}
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger
import exploration.ExpressionFilter.Status.Success
import ir.ast.{Expr, FunCall, Lambda}
import ir.{Type, TypeChecker}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.executor.Eval
import opencl.generator.NDRange
import opencl.ir.pattern._
import rewriting.InferNDRange
import rewriting.utils.{DumpToFile, Utils}
import _root_.utils.CommandLineParser
import scopt.OParser

import scala.collection.immutable.Map
import scala.io.Source
import scala.sys.process._
import scala.util.Random

/**
  * This main currently runs a parameter space exploration over the
  * serialized low level expressions.
  */
object SplitSlideRewrite {

  private val logger = Logger(this.getClass)

  private var topFolder = ""

  import ParameterRewriteSettings._

  case class Config(input: File = null,
                    exploreNDRange: Boolean = defaultExploreNDRange,
                    sampleNDRange: Int = defaultSampleNDRange,
                    disableNDRangeInjection: Boolean = defaultDisableNDRangeInjection,
                    sequential: Boolean = defaultSequential,
                    generateScala: Boolean = defaultGenerateScala,
                    settingsFile: File = null)

  val builder = OParser.builder[Config]
  var cmdArgs: Option[Config] = None
  val parser = {
    import builder._
    OParser.sequence(
      programName("SplitSlideRewrite"),
      opt[File]("input").text("Input files to read").required()
        .validate(f => if (f.exists) success else failure("File \"" + f.getName + "\" does not exist"))
        .action((arg, c) => c.copy(input = arg)),

      opt[Unit](keyExploreNDRange).text(s"Explore global and local sizes instead of inferring " +
        s"(default: $defaultExploreNDRange)")
        .action((_, c) => c.copy(exploreNDRange = true)),

      opt[Int](keySampleNDRange).text(s"Randomly sample n combinations of global and local sizes (requires 'explore') " +
        s"(default: $defaultSampleNDRange)")
        .action((arg, c) => c.copy(sampleNDRange = arg)),

      opt[Unit](keyDisableNDRangeInjection).text(s"Don't inject NDRanges while compiling the OpenCL Kernel " +
        s"(default: $defaultDisableNDRangeInjection)")
        .action((_, c) => c.copy(disableNDRangeInjection = true)),

      opt[Unit]('s', keySequential).text(s"Don't execute in parallel (default: $defaultSequential")
        .action((_, c) => c.copy(sequential = true)),

      opt[Unit]('s', keyGenerateScala).text(s"Generate lambdas in Scala as well as in OpenCL (default: $defaultGenerateScala)")
        .action((_, c) => c.copy(generateScala = true)),

      opt[File]('f', "file").text("The settings file to use.").required()
        .validate(f => if (f.exists) success else failure("File \"" + f.getName + "\" does not exist"))
        .action((arg, c) => c.copy(settingsFile = arg)),

      help("help").text("Show this message.")
    )
  }

  private[exploration] var settings = Settings()

  private var lambdaFilename = ""

  def main(args: Array[String]): Unit = {

    try {

      cmdArgs = Some(CommandLineParser(parser, args, Config()))

      settings = ParseSettings(Some(cmdArgs.get.settingsFile.getName))

      val config = settings.parameterRewriteSettings
      if (!config.exploreNDRange && config.sampleNDRange > 0)
        throw new RuntimeException("'sample' is defined without enabling 'explore'")

      logger.info(s"Arguments: ${args.mkString(" ")}")
      val inputArgument = cmdArgs.get.input.getName

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

            val all_substitution_tables: Seq[Map[Var, ArithExpr]] = SplitSlideSearch(high_level_expr)
            val substitutionCount = all_substitution_tables.size
            println(s"Found $substitutionCount valid parameter sets")

            val loweredIndex = s"${topFolder}Lower/$high_level_hash/index"
            if (Files.exists(Paths.get(loweredIndex))
              && substitutionCount < 800000) {

              val lowLevelExprList = Source.fromFile(loweredIndex).getLines().toList

              val lowLevelCounter = new AtomicInteger()
              val lowLevelCount = lowLevelExprList.size
              val propagationCounter = new AtomicInteger()
              val kernelCounter = new AtomicInteger()
              val propagationCount = lowLevelCount * substitutionCount
              println(s"Found $lowLevelCount low level expressions")

              val parList = if (config.sequential) lowLevelExprList else lowLevelExprList.par

              parList.foreach(low_level_filename => {

                val low_level_hash = low_level_filename.split("/").last
                val fullLowLevelFilename = parentFolder + "/" + low_level_filename

                try {

                  val low_level_str = readFromFile(fullLowLevelFilename)
                  val low_level_factory = Eval.getMethod(low_level_str)

                  lowLevelCounter.incrementAndGet()
                  val potential_expressions: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))] =
                    all_substitution_tables.flatMap(st => {

                      print(s"\rLow-Level expression: ${lowLevelCounter.get()}/$lowLevelCount | Propagation ${propagationCounter.incrementAndGet()}/$propagationCount")
                      val params = st.toSeq.sortBy(_._1.id).map(_._2)
                      try {
                        val expr = low_level_factory(sizesForFilter ++ params)
                        TypeChecker(expr)

                        val rangeList = if (config.exploreNDRange)
                          computeValidNDRanges(expr)
                        else
                          Seq(InferNDRange(expr))

                        val filtered: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))] =
                          rangeList.flatMap { ranges =>
                            // don't filter if we're not injecting the NDRanges
                            if (config.disableNDRangeInjection || ExpressionFilter(expr, ranges, settings.searchParameters) == Success)
                              Some((low_level_factory(vars ++ params), params, ranges))
                            else
                              None
                          }

                        if (config.exploreNDRange) logger.debug(filtered.length + " NDRanges after filtering")
                        val sampled = if (config.sampleNDRange > 0 && filtered.nonEmpty) {
                          Random.setSeed(0L) //always use the same seed
                          Random.shuffle(filtered).take(config.sampleNDRange)
                        } else
                          filtered

                        val sampleStrings: Seq[String] = sampled.map(x => low_level_hash + "_" + x._2.mkString("_") +
                          "_" + x._3._1.toString.replace(",", "_") + "_" + x._3._2.toString.replace(",", "_"))
                        if (config.sampleNDRange > 0) logger.debug("\nSampled NDRanges:\n\t" + sampleStrings.mkString(" \n "))
                        Some(sampled)

                      } catch {
                        case _: ir.TypeException => None

                        //noinspection SideEffectsInMonadicTransformation
                        case x: Throwable =>
                          logger.warn("Failed parameter propagation", x)
                          logger.warn(low_level_hash)
                          logger.warn(params.mkString("; "))
                          logger.warn(low_level_str)
                          logger.warn(settings.searchParameters.inputSize.toString)
                          None
                      }
                    }).flatten

                  kernelCounter.addAndGet(potential_expressions.size)

                  val hashes = SaveOpenCL(topFolder, low_level_hash,
                    high_level_hash, settings, potential_expressions)

                  if (config.generateScala)
                    saveScala(potential_expressions, hashes)

                } catch {
                  case t: Throwable =>
                    // Failed reading file or similar.
                    logger.warn(s"Caught at low level hash: $low_level_hash", t)
                }
              })
              println(s"\nGenerated $kernelCounter kernels")
            }
          } catch {
            case t: Throwable =>
              logger.warn(s"Caught for high-level hash: $high_level_hash", t)
          }
        }
      })
    } catch {
      case io: IOException =>
        logger.error("IOException", io)
    }
  }

  def saveScala(expressions: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))], hashes: Seq[scala.Option[String]]): Unit = {
    val filename = lambdaFilename
    val file = scala.tools.nsc.io.File(filename)

    (expressions, hashes).zipped.foreach((f, hash) => {

      try {
        val stringRep = "{ " + DumpToFile.dumpLambdaToString(f._1).replace("\n", "; ") + "}"

        val sha256 = hash.get

        synchronized {
          file.appendAll("(\"" + sha256 + "\",  " + s"Array($stringRep)) ,\n")
        }
      } catch {
        case t: Throwable =>
          logger.warn(s"Saving to scala failed, $hash", t)
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
      if (sizes.isEmpty) Seq.fill(vars.length)(settings.searchParameters.inputSize)
      else sizes

    (vars, actualSizes).zipped.toMap
  }

  def replaceInputTypes(lambda: Lambda, st: Map[ArithExpr, ArithExpr]): Lambda = {
    val tunable_nodes = Utils.findTunableNodes(lambda).reverse
    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))
    Utils.quickAndDirtySubstitution(st, tunable_nodes, lambda)
  }

  private def computeValidNDRanges(expr: Lambda): Seq[(NDRange, NDRange)] = {
    var usedDimensions: Set[Int] = Set()
    Expr.visit(expr.body, {
      case FunCall(MapGlb(dim, _), _) =>
        usedDimensions += dim

      case FunCall(MapLcl(dim, _), _) =>
        usedDimensions += dim

      case FunCall(MapWrg(dim, _), _) =>
        usedDimensions += dim

      case FunCall(MapAtomLcl(dim, _, _), _) =>
        usedDimensions += dim

      case FunCall(MapAtomWrg(dim, _, _), _) =>
        usedDimensions += dim

      case _ =>
    }, (_) => Unit)
    val nDRangeDim = usedDimensions.max + 1

    logger.debug(s"computing ${nDRangeDim}D NDRanges")

    // hardcoded highest power of two = 8192
    val pow2 = Seq.tabulate(14)(x => scala.math.pow(2, x).toInt)
    val localGlobalCombinations: Seq[(ArithExpr, ArithExpr)] = (for {
      local <- pow2
      global <- pow2
      if local <= global
    } yield (local, global)).map { case (l, g) => (Cst(l), Cst(g)) }

    nDRangeDim match {
      case 1 => for {
        x <- localGlobalCombinations
        if ExpressionFilter(x._1, x._2, settings.searchParameters) == Success
      } yield (NDRange(x._1, 1, 1), NDRange(x._2, 1, 1))

      case 2 => for {
        x: (ArithExpr, ArithExpr) <- localGlobalCombinations
        y: (ArithExpr, ArithExpr) <- localGlobalCombinations
        if ExpressionFilter(x._1, y._1, x._2, y._2, settings.searchParameters) == Success
      } yield (NDRange(x._1, y._1, 1), NDRange(x._2, y._2, 1))

      case 3 => for {
        x <- localGlobalCombinations
        y <- localGlobalCombinations
        z <- localGlobalCombinations
        if ExpressionFilter(x._1, y._1, z._1, x._2, y._2, z._2, settings.searchParameters) == Success
      } yield (NDRange(x._1, y._1, z._1), NDRange(x._2, y._2, z._2))

      case _ => throw new RuntimeException("Could not pre-compute NDRanges for exploration")
    }
  }
}
