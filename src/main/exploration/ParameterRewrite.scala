package exploration

import java.io.{File, IOException}
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger

import apart.arithmetic.{ArithExpr, Var}
import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import ir.{Type, TypeChecker}
import opencl.executor.Eval
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._
import rewriting.utils.Utils

import scala.collection.immutable.Map
import scala.io.Source
import scala.sys.process._

/**
  * This main currently runs a parameter space exploration over the serialized low level expressions.
  */
object ParameterRewrite {

  private val logger = Logger(this.getClass)

  private var topFolder = ""

  private val parser = new ArgotParser("ParameterRewrite")

  parser.flag[Boolean](List("h", "help"),
    "Show this message.") {
    (sValue, opt) =>
      parser.usage()
      sValue
  }

  private val input = parser.parameter[String]("input",
    "Input file containing the lambda to use for rewriting",
    optional = false) {
    (s, opt) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage("Input file \"" + s + "\" does not exist")
      s
  }

  private val sequential = parser.flag[Boolean](List("s", "seq", "sequential"), "Don't execute in parallel.")

  private val generateScala = parser.flag[Boolean](List("generate-scala"),
    "Generate lambdas in Scala as well as in OpenCL")

  private var lambdaFilename = ""


  def main(args: Array[String]) {

    try {

      parser.parse(args)

      topFolder = input.value.get

      lambdaFilename = topFolder + "Scala/lambdaFile"

      if (generateScala.value.isDefined) {
        val f = new File(lambdaFilename)
        if (f.exists()) {
          f.delete()
        } else {
          s"mkdir -p ${topFolder}Scala".!
        }
      }

      // list all the high level expression
      val all_files = Source.fromFile(s"$topFolder/index").getLines().toList
      val highLevelCount = all_files.size

      var expr_counter = 0
      all_files.foreach(filename => {

        if (Files.exists(Paths.get(filename))) {
          val high_level_hash = filename.split("/").last
          expr_counter = expr_counter + 1
          println(s"High-level expression : $expr_counter / $highLevelCount")

          try {

            val high_level_expr_orig = readLambdaFromFile(filename)

            val st = createValueMap(high_level_expr_orig)
            val sizesForFilter = st.values.toSeq
            val vars = Seq.fill(sizesForFilter.length)(Var(""))

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
                  val low_level_str = readFromFile(low_level_filename)
                  val low_level_factory = Eval.getMethod(low_level_str)

                  println(s"Low-level expression ${low_level_counter.incrementAndGet()} / $lowLevelCount")

                  println("Propagating parameters...")
                  val potential_expressions = all_substitution_tables.map(st => {
                    val params = st.map(a => a).toSeq.sortBy(_._1.toString.substring(3).toInt).map(_._2)
                    try {
                      val expr = low_level_factory(sizesForFilter ++ params)
                      TypeChecker(expr)
                      if (ExpressionFilter(expr) == ExpressionFilter.Status.Success)
                        Some((low_level_factory(vars ++ params), params))
                      else
                        None
                    } catch {
                      case x: ir.TypeException => None
                      case x: Throwable =>
                        logger.warn("Failed parameter propagation", x)
                        logger.warn(low_level_hash)
                        logger.warn(params.mkString("; "))
                        logger.warn(low_level_str)
                        logger.warn(SearchParameters.matrix_size.toString)
                        None
                    }
                  }).collect { case Some(x) => x }

                  println(s"Found ${potential_expressions.size} / $substitutionCount filtered expressions")

                  val hashes = SaveOpenCL(topFolder, low_level_hash,
                    high_level_hash, potential_expressions)

                  if (generateScala.value.isDefined)
                    saveScala(potential_expressions, hashes)

                } catch {
                  case t: Throwable =>
                    // TODO: Log all errors to a file, so they could be reproduced in case of bugs
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

  def saveScala(expressions: List[(Lambda, Seq[ArithExpr])], hashes: Seq[Option[String]]): Unit = {

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

  def createValueMap(lambda: Lambda): Map[ArithExpr, ArithExpr] = {
    val vars = lambda.params.flatMap(_.t.varList).distinct

    vars.foldLeft(Map[ArithExpr, ArithExpr]())((st, v) =>
      st.updated(v, SearchParameters.matrix_size))
  }

  def replaceInputTypes(lambda: Lambda, st: Map[ArithExpr, ArithExpr]): Lambda = {
    val tunable_nodes = Utils.findTunableNodes(lambda).reverse
    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))
    Utils.quickAndDirtySubstitution(st, tunable_nodes, lambda)
  }

}
