package backends.spatial.host

import backends.common.Compiler
import _root_.ir.ast.Lambda

object HostCompiler extends Compiler {

  def compile(lambda: Lambda, path: String, files: List[String], func_name: String): Unit = {
    // TODO
  }
}
