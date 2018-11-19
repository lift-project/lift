package host.printer


import java.io.PrintWriter

import core.generator.GenericAST.Block

import core.generator.PrettyPrinter._


object CASTPrinter {

  //val boilerplate_code = "boilerplate_before_outline_functions\n\n"

  def apply(mb: Block, path: String, file: String): String ={

    val host_code: String = layoutS(mb.print())

    new PrintWriter(path + "/" + file) { write(host_code); close}

    host_code

  }

}
