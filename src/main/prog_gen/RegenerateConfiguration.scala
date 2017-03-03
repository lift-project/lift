package prog_gen

import java.io.File

import com.typesafe.scalalogging.Logger
import exploration.ParameterRewrite
import ir.TypeChecker
import ir.ast.Lambda
import org.clapper.argot.{ArgotParser, ArgotUsageException}
import rewriting.utils.Utils

object RegenerateConfiguration {

  private val logger = Logger(this.getClass)

  private val parser = new ArgotParser("GeneratePrograms")

  private val inputFolder = parser.parameter[String]("input",
    "Input file containing the lambda to use for rewriting",
    optional = false) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage("Input file \"" + s + "\" does not exist")
      s
  }

  def main(args: Array[String]): Unit = {
    try {
      parser.parse(args)

      logger.info(s"Arguments: ${args.mkString(" ")}")


      val topFolder = new File(inputFolder.value.get)

      val programPaths = topFolder.listFiles().flatMap(firstSubDir =>
        firstSubDir.listFiles().flatMap(secondSubDir =>
          secondSubDir.listFiles().filter(_.isFile)))

      val concretePrograms = programPaths.par.map(program =>
        try {
        Some(ParameterRewrite.readLambdaFromFile(program.getAbsolutePath))
        } catch {
          case _: Throwable => None
        }).collect({ case Some(lambda) => lambda })

      logger.info(s"Read ${concretePrograms.length} programs...")

      saveConfigurations(concretePrograms.toArray.toSeq)

    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  private def saveConfigurations(concretePrograms: Seq[Lambda]) = {
    val configurationDirectory = "newConfiguration"

    concretePrograms.foreach(lambda => try {
      TypeChecker(lambda)

      val vars = lambda.getVarsInParams()

      val sizes = GeneratePrograms.getInputSizeCombinations(vars.length)

      val lambdaString = Utils.dumpLambdaToString(lambda)

      val hash = Utils.Sha256Hash(lambdaString)
      val hashPrefix = hash(0) + "/" + hash(1)
      val thisLambdaConf = s"$configurationDirectory/$hashPrefix/$hash"

      GeneratePrograms.generateConfigurations(sizes, hash, thisLambdaConf, lambda)

    } catch {
      case t: Throwable =>
        logger.warn(t.toString)
    })
  }

}
