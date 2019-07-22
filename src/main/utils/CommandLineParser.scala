package utils

import scopt.OParser

object CommandLineParser {
  def apply[C](parser: OParser[Unit, C], args: Array[String], config: C): C = {
    OParser.parse(parser, args, config) match {
      case Some(parsedArgs) => parsedArgs.asInstanceOf[C]
      case _ => throw new IllegalArgumentException("Wrong command line arguments passed")
    }
  }
}
