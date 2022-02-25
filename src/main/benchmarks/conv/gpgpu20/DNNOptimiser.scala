package benchmarks.conv.gpgpu20

import benchmarks.conv.gpgpu20.settings.{ParseSettings, Settings}
import org.apache.commons.cli.{CommandLine, DefaultParser, Option, Options, ParseException}

object DNNOptimiser {
  /* List of options */
  val options = new Options()

  options.addOption(Option.builder("path_to_settings").
    longOpt("path_to_settings").
    argName("path_to_settings").
    `type`(classOf[String]).
    hasArg().
    required(true).
    build())

  def parseCommandLine(args: Array[String]): CommandLine = {
    try {
      val parser = new DefaultParser()
      parser.parse(options, args)
    } catch {
      case x: ParseException =>
        throw new ParseException("Option parsing failed. Reason: " + x.getMessage)
    }
  }


  def init(args: Array[String]): (CommandLine, Settings) = {
    val cmdArgs = parseCommandLine(args)
    val settings = ParseSettings(cmdArgs.getOptionValue("path_to_settings"))

    (cmdArgs, settings)
  }


}
