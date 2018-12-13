package cbackends.sdh

import cbackends.common.CBackendsCompilerTrait
import ir.ast.Lambda

object SDHCompiler extends CBackendsCompilerTrait{

  override def typeCheck(lambda: Lambda): Unit = {
    println("overrided typecheck by sdh called")
  }

}
