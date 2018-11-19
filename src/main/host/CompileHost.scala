package host

import host.loop_var_inference.LoopVarInference
import host.memory_management.{InferHostMemoryAddressSpace, MemoryAllocator}
import host.printer.CASTPrinter
import host.view.View
import ir.{TypeChecker, UndefType}
import ir.ast.{Expr, IRNode, Lambda}
import lowering.LowerIR2HostCAST
import sys.process._

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
    View(final_lambda)

    //val (CAST, all_signature_cvars) = LowerIR2HostCAST(final_lambda, hostMemoryDeclaredInSignature)
    val tuple = LowerIR2HostCAST(final_lambda, hostMemoryDeclaredInSignature)
    val CAST = tuple._1
    val all_signature_cvars = tuple._2

    //create the directory if not exist
    s"mkdir $path" !

    val sched_code = CASTPrinter(CAST,path+"/libmap.cpp")


    println("All done!")
  }


}


