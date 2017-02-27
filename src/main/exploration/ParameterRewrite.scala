package exploration

import java.io.{File, IOException}
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import ir.{ArrayType, Type, TypeChecker}
import lift.arithmetic.{ArithExpr, Cst}
import opencl.executor.Eval
import opencl.generator.OpenCLGenerator._
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._
import play.api.libs.json.Reads._
import play.api.libs.json._
import rewriting.InferNDRange
import rewriting.utils.Utils

import scala.collection.immutable.Map
import scala.io.Source
import scala.sys.process._

/**
  * This main currently runs a parameter space exploration over the
  * serialized low level expressions.
  */
object ParameterRewrite {

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

  private val sequential = parser.flag[Boolean](List("s", "seq", "sequential"),
    "Don't execute in parallel.")

  private val explore = parser.flag[Boolean](List("e", "explore"),
    "Additionally explore global and local sizes")

  private val generateScala = parser.flag[Boolean](List("generate-scala"),
    "Generate lambdas in Scala as well as in OpenCL")

  private val settingsFile = parser.option[String](List("f", "file"), "name",
    "Store the created lambdas into this folder."
  ) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage(s"Settings file $file doesn't exist.")
      s
  }

  private var lambdaFilename = ""

  def main(args: Array[String]): Unit = {

    try {

      parser.parse(args)

      val inputArgument = input.value.get

      topFolder = Paths.get(inputArgument).toString

      lambdaFilename = topFolder + "Scala/lambdaFile"

      if (generateScala.value.isDefined) {
        val f = new File(lambdaFilename)
        if (f.exists()) {
          f.delete()
        } else {
          s"mkdir -p ${topFolder}Scala".!
        }
      }

      val settings = ParseSettings(settingsFile.value)

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

            val vars = high_level_expr_orig.params.flatMap(_.t.varList)
              .sortBy(_.name).distinct

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

            val all_substitution_tables = ParameterSearch(high_level_expr)
            val substitutionCount = all_substitution_tables.size
            println(s"Found $substitutionCount valid parameter sets")

            val loweredIndex = s"${topFolder}Lower/$high_level_hash/index"
            if (Files.exists(Paths.get(loweredIndex))
              && substitutionCount < 800000) {

              val low_level_expr_list = Source.fromFile(loweredIndex).getLines().toList

              val low_level_counter = new AtomicInteger()
              val lowLevelCount = low_level_expr_list.size
              println(s"Found $lowLevelCount low level expressions")

              val parList = if (sequential.value.isDefined) low_level_expr_list else low_level_expr_list.par

              parList.foreach(low_level_filename => {

                try {

                  val low_level_hash = low_level_filename.split("/").last
                  val fullLowLevelFilename = parentFolder + "/" + low_level_filename
                  val low_level_str = readFromFile(fullLowLevelFilename)
                  val low_level_factory = Eval.getMethod(low_level_str)

                  println(s"Low-level expression ${low_level_counter.incrementAndGet()} / $lowLevelCount")

                  println("Propagating parameters...")
                  val potential_expressions: Seq[(Lambda, Seq[ArithExpr]/*, (NDRange, NDRange)*/)] = all_substitution_tables.flatMap(st => {

                    val params = st.toSeq.sortBy(_._1.toString.substring(3).toInt).map(_._2)
                    try {
                      val expr = low_level_factory(sizesForFilter ++ params)
                      TypeChecker(expr)

                      val rangeList = getAllNDRanges(expr, explore.value.isDefined)
                      println(s"[DEBUG] ${rangeList.length} different global localsize combinations")

                      if (ExpressionFilter(expr, rangeList.head) == ExpressionFilter.Status.Success)
                        Some((low_level_factory(vars ++ params), params))
                      else
                        None
                    } catch {
                      case _: ir.TypeException => None

                      //noinspection SideEffectsInMonadicTransformation
                      case x: Throwable =>
                        logger.warn("Failed parameter propagation", x)
                        logger.warn(low_level_hash)
                        logger.warn(params.mkString("; "))
                        logger.warn(low_level_str)
                        logger.warn(SearchParameters.matrix_size.toString)
                        None
                    }
                  })

                  println(s"Found ${potential_expressions.size} / $substitutionCount filtered expressions")

                  val hashes = SaveOpenCL(topFolder, low_level_hash,
                    high_level_hash, settings, potential_expressions)

                  if (generateScala.value.isDefined)
                    saveScala(potential_expressions, hashes)

                } catch {
                  case t: Throwable =>
                    // Failed reading file or similar.
                    logger.warn(t.toString)
                }
              })
            }
          } catch {
            case t: Throwable =>
              logger.warn(t.toString)
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

  def saveScala(expressions: Seq[(Lambda, Seq[ArithExpr])], hashes: Seq[Option[String]]): Unit = {

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
    val vars = getVars(lambda)

    val actualSizes: Seq[ArithExpr] =
      if (sizes.isEmpty) Seq.fill(vars.length)(SearchParameters.matrix_size) else sizes

    (vars, actualSizes).zipped.toMap
  }

  def getVars(lambda: Lambda) =
    lambda.params.flatMap(_.t.varList).sortBy(_.name).distinct

  def replaceInputTypes(lambda: Lambda, st: Map[ArithExpr, ArithExpr]): Lambda = {
    val tunable_nodes = Utils.findTunableNodes(lambda).reverse
    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))
    Utils.quickAndDirtySubstitution(st, tunable_nodes, lambda)
  }

  private def getAllNDRanges(expr: Lambda, explorationEnabled: Boolean): Seq[(NDRange, NDRange)] = {
    if (explorationEnabled) {
      // assumes first param of lambda is input array and determines its dimensionality
      val firstParam = expr.params.head
      val inputDim = firstParam.t match {
        case ArrayType(ArrayType(ArrayType(dt, m), n), o) => 3 //or higher but we don't care
        case ArrayType(ArrayType(dt, m), n) => 2
        case ArrayType(dt, m) => 1
        case _ => -1
      }

      val pow2 = Seq.tabulate(14)(x => scala.math.pow(2,x).toInt)
      val localGlobalCombinations: Seq[(ArithExpr, ArithExpr)] = (for {
        local <- pow2
        global <- pow2
        if local <= global
      } yield (local, global)).map{ case (l,g) => (Cst(l), Cst(g))}

      inputDim match {
        case 1 => localGlobalCombinations.map{ case (l, g) => ( Array(l, 1, 1): NDRange, Array(g, 1, 1): NDRange ) }


        case 2 => for {
          x: (ArithExpr, ArithExpr) <- localGlobalCombinations
          y: (ArithExpr, ArithExpr) <- localGlobalCombinations
        } yield (Array(x._1, y._1, 1): NDRange, Array(x._2, y._2, 1):NDRange)

        case 3 => for {
          x <- localGlobalCombinations
          y <- localGlobalCombinations
          z <- localGlobalCombinations
        } yield (Array(x._1, y._1, z._1): NDRange, Array(x._2, y._2, z._2): NDRange)

          // could not explore - return to default
        case _ => Seq(InferNDRange(expr))
      }
    } else
      Seq(InferNDRange(expr))
  }
}

case class Settings(inputCombinations: Option[Seq[Seq[ArithExpr]]])

object ParseSettings {

  private val logger = Logger(this.getClass)

  private implicit val arithExprReads: Reads[ArithExpr] =
    JsPath.read[Long].map(Cst)

  private implicit val settingsReads: Reads[Settings] =
    (JsPath \ "input_combinations").readNullable[Seq[Seq[ArithExpr]]].map(Settings)

  def apply(optionFilename: Option[String]): Settings =
    optionFilename match {
      case Some(filename) =>

        val settingsString = ParameterRewrite.readFromFile(filename)
        val json = Json.parse(settingsString)
        val validated = json.validate[Settings]

        validated match {
          case JsSuccess(settings, _) =>
            checkSettings(settings)
            settings
          case e: JsError =>
            logger.error("Failed parsing settings " +
              e.recoverTotal(e => JsError.toFlatJson(e)))
            sys.exit(1)
        }

      case None =>
        Settings(None)
    }

  private def checkSettings(settings: Settings): Unit = {

    // Check all combinations are the same size
    if (settings.inputCombinations.isDefined) {
      val sizes = settings.inputCombinations.get

      if (sizes.map(_.length).distinct.length != 1) {
        logger.error("Sizes read from settings contain different numbers of parameters")
        sys.exit(1)
      }
    }
  }

}
