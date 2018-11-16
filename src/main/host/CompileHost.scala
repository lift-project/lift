package host

import host.loop_var_inference.LoopVarInference
import host.memory_management.{InferHostMemoryAddressSpace, MemoryAllocator}
import ir.{TypeChecker, UndefType}
import ir.ast.{Expr, IRNode, Lambda}

object CompileHost {

  def apply(lambda: Lambda, path: String) ={

    val final_lambda = lambda.asInstanceOf[Lambda]

    final_lambda.params.foreach(p => assert(p.t != UndefType))
    TypeChecker(final_lambda)

    lambda.visit( pre = {node:IRNode =>
      node match {
        case e:Expr => assert( e.t != UndefType )
        case _ =>
      }
    })

    InferHostMemoryAddressSpace(final_lambda)
    LoopVarInference(final_lambda)
    val hostMemoryDeclaredInSignature = MemoryAllocator(final_lambda)
    //View(final_lambda)


    println("All done!")
  }


}


