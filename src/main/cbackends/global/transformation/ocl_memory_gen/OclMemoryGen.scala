package cbackends.global.transformation.ocl_memory_gen

import cbackends.host.host_ir.OclFunc
import ir.ast.{Expr, FunCall, Lambda, Value}

object OclMemoryGen {



  /*
  def generate(expr: Expr): Unit = {
    case fc@ FunCall( ocl:OclFunc, _* ) =>
      //val ocl_f = ocl.f
      //Utility apply to ocl_f
      //store its result to ocl.memories
      //ocl.memories =

    case FunCall( _, args@_* ) =>
      args.foreach(generate _)
    case _:Value =>
  }
*/

  def apply(lambda: Lambda): Unit = {


    //generate(lambda.body)

  }

}
