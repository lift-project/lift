package cbackends.common.printer

import java.io.PrintWriter

import cbackends.common.common_cast.CbackendCAST.SourceFile
import core.generator.GenericAST.Block
import core.generator.PrettyPrinter._


object CASTPrinter {


  def apply(mb: Block, path: String, file: String): String ={

    val host_code: String = layoutS(mb.print())

    new PrintWriter(path + "/" + file) { write(host_code); close}

    host_code

  }


  def apply(files: List[SourceFile]) : Unit = {

    files.foreach( file => CASTPrinter(file.cast, file.path, file.file) )

  }

}
