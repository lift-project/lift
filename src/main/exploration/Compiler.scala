package exploration

import java.io.File

import org.apache.commons.cli._

/**
 * Compiler driver for CLI transformations.
 */
object Compiler {
  /** List of options. */
  val options = new Options()

  /** Parsed command line to access the option values. */
  var opts: CommandLine = null

  /** Base directory to store the files. */
  var baseDir: String = null

  /**
   * Help routine, print help and exit.
   */
  def help() = {
    val formatter = new HelpFormatter();
    formatter.printHelp( "liftc", options )
    System.exit(0)
  }

  /**
   * Error routine, print the message on stderr and abort.
   * @param str Error message.
   */
  def error(str: String) = {
    System.err.println("[ERROR]: " + str)
    System.err.println("[ERROR]: Use '-h' for help.")
    System.exit(-1)
  }

  /**
   * Verbose info.
   */
  def info(str: String) = {
    if (opts.hasOption("verbose")) {
      this.synchronized {
        println("[INFO]" + str)
      }
    }
  }

  /**
   * Generate high level expressions from an input file.
   */
  def generateHighLevel() = {
    info("Generating high level expressions")

    val filename = opts.getOptionValue("input")
    if (filename == null || filename.isEmpty)
      error("missing input filename.")

    val inputFile = new File(filename)
    if (!inputFile.exists())
      error("input file '" + filename + "' does not exist.")

    // TODO(tlutz): logic to generate the high level expressions
  }

  /**
   * Generate low level expressions from a set of high level expressions.
   */
  def generateLowLevel() = {
    info("Generating low level expressions")

    // TODO(tlutz): logic to generate the high level expressions
  }

  /**
   * Generate opencl code from a set of low level expressions.
   */
  def generateOpenCL() = {
    info("Generating OpenCL code")

    // TODO(tlutz): logic to generate the OpenCL code
  }

  def main(args: Array[String]) {
    // *** general options
    // help switch
    options.addOption("h", "help", false, "Print help and exit.")
    // verbose switch
    options.addOption("v", "verbose", false, "Verbose mode.")
    // Base directory. The compiler will append "-HighLevel" "-LowLevel" "-OpenCL" for the generated files
    options.addOption("d", "directory", true, "Set root directory name.")

    // *** High level options
    options.addOption("H", "generate_high_level", false, "Generate only high level expressions.")
    options.addOption("i", "input", true, "Expression input file.")
    options.addOption("max_depth", true, "Maximum map depth in the expression.")
    options.addOption("max_dist", true, "Maximum distance between user functions.")
    options.addOption("max_rules", true, "Maximum number of rules to apply.")

    // *** Low level options
    options.addOption("L", "generate_low_level", false, "Generate only low level expressions.")
    options.addOption("xml", true, "Load an XML file for filtering parameters.")
    options.addOption("vectorize", false, "Enable vectorization.")
    options.addOption("disable_global", false, "Disable global mapping strategies.")
    options.addOption("disable_local", false, "Disable local mapping strategies.")

    // *** Codegen options
    options.addOption("C", "generate_opencl", false, "Generate only OpenCL code from low level expressions.")

    val parser = new DefaultParser()
    try {
      // parse the command line arguments
      opts = parser.parse( options, args )

      if (opts == null || opts.hasOption("help") || opts.getArgs.nonEmpty || opts == null)
        help()

      // Set base directory
      baseDir = opts.getOptionValue("directory")
      if (baseDir == null || baseDir.length == 0)
        error("Invalid base directory")
      info("Using base directory '" + baseDir + "'")

      if (opts.hasOption("generate_high_level"))
        generateHighLevel()
      else if (opts.hasOption("generate_low_level"))
        generateLowLevel()
      else if (opts.hasOption("generate_opencl"))
        generateOpenCL()
      else {
        generateHighLevel()
        generateLowLevel()
        generateOpenCL()
      }
    }
    catch {
      case x: ParseException =>
        error( "Option parsing failed. Reason: " + x.getMessage )
    }
  }
}
