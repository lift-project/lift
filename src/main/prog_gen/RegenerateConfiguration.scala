package prog_gen

import java.io.File

import com.typesafe.scalalogging.Logger
import exploration.SplitSlideRewrite
import ir.TypeChecker
import ir.ast.Lambda
import rewriting.utils.{DumpToFile, Utils}
import scopt.OParser
import utils.CommandLineParser

object RegenerateConfiguration {

  private val logger = Logger(this.getClass)

  case class Config(input: File = null)
  val builder = OParser.builder[Config]
  var cmdArgs: Option[Config] = None
  val parser = {
    import builder._
    OParser.sequence(
      programName("RegenerateConfiguration"),
      opt[File]("input").text("Input files to read").required()
        .validate(f => if (f.exists) success else failure("File \"" + f.getName + "\" does not exist"))
        .action((arg, c) => c.copy(input = arg)),

      help("help").text("Show this message.")
    )
  }

  def main(args: Array[String]): Unit = {
    cmdArgs = Some(CommandLineParser(parser, args, Config()))

    logger.info(s"Arguments: ${args.mkString(" ")}")

    val topFolder = cmdArgs.get.input

    val programPaths = topFolder.listFiles().flatMap(firstSubDir =>
      firstSubDir.listFiles().flatMap(secondSubDir =>
        secondSubDir.listFiles().filter(_.isFile)))

    val concretePrograms = programPaths.par.map(program =>
      try {
        Some(SplitSlideRewrite.readLambdaFromFile(program.getAbsolutePath))
      } catch {
        case _: Throwable => None
      }).collect({ case Some(lambda) => lambda })

    logger.info(s"Read ${concretePrograms.length} programs...")

    saveConfigurations(concretePrograms.toArray.toSeq)
  }

  private def saveConfigurations(concretePrograms: Seq[Lambda]) = {
    val configurationDirectory = "newConfiguration"

    concretePrograms.foreach(lambda => try {
      TypeChecker(lambda)

      val vars = lambda.getVarsInParams()

      val sizes = GeneratePrograms.getInputSizeCombinations(vars.length)

      val lambdaString = DumpToFile.dumpLambdaToString(lambda)

      val hash = DumpToFile.Sha256Hash(lambdaString)
      val hashPrefix = hash(0) + "/" + hash(1)
      val thisLambdaConf = s"$configurationDirectory/$hashPrefix/$hash"

      GeneratePrograms.generateConfigurations(sizes, hash, thisLambdaConf, lambda)

    } catch {
      case t: Throwable =>
        logger.warn(t.toString)
    })
  }

}
