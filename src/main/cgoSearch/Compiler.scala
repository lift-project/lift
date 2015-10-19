package cgoSearch

import org.apache.commons.cli._

/**
 * Compiler driver for CLI transformations
 */
object Compiler {
  val options = new Options()

  def help() = {
    val formatter = new HelpFormatter();
    formatter.printHelp( "liftc", options )
    System.exit(0)
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
    options.addOption("H", "generate-high-level", false, "Generate only high level expressions.")
    options.addOption("i", "input", true, "Expression input file.")
    options.addOption("max-depth", true, "Maximum map depth in the expression.")
    options.addOption("max-dist", true, "Maximum distance between user functions.")
    options.addOption("max-rules", true, "Maximum number of rules to apply.")

    // *** Low level options
    options.addOption("L", "generate-low-level", false, "Generate only low level expressions.")
    options.addOption("xml", true, "Load an XML file for filtering parameters.")
    options.addOption("vectorize", false, "Enable vectorization.")
    options.addOption("disable-global", false, "Disable global mapping strategies.")
    options.addOption("disable-local", false, "Disable local mapping strategies.")

    // *** Codegen options
    options.addOption("C", "generate-opencl", false, "Generate only OpenCL code from low level expressions.")

    val parser = new DefaultParser()
    try {
      // parse the command line arguments
      val opts = parser.parse( options, args )

      if (opts.hasOption("help") || opts.getArgs.nonEmpty || opts = null)
        help()
    }
    catch {
      case x: ParseException =>
        println( "Parsing failed.  Reason: " + x.getMessage )
    }
  }
}
