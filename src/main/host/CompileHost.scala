package host

import host.loop_var_inference.LoopVarInference
import host.memory_management.{InferHostMemoryAddressSpace, MemoryAllocator}
import host.printer.CASTPrinter
import host.view.Views
import ir.{TypeChecker, UndefType}
import ir.ast.{Expr, IRNode, Lambda}
import lowering.LowerIR2HostCAST

import sys.process._
import scala.language.postfixOps

object CompileHost {

  def apply(lambda: Lambda, path: String, file: String) ={

    val final_lambda = lambda.asInstanceOf[Lambda]

    final_lambda.params.foreach(p => assert(p.t != UndefType))
    TypeChecker(final_lambda)

    lambda visit {
       //node: IRNode =>
        //node match
        {
          case e: Expr => assert(e.t != UndefType)
          case _ =>
        }
    }

    InferHostMemoryAddressSpace(final_lambda)
    LoopVarInference(final_lambda)
    val hostMemoryDeclaredInSignature = MemoryAllocator(final_lambda)
    Views(final_lambda)

    //val (CAST, all_signature_cvars) = LowerIR2HostCAST(final_lambda, hostMemoryDeclaredInSignature)
    val tuple = LowerIR2HostCAST(final_lambda, hostMemoryDeclaredInSignature)
    val CAST = tuple._1
    val all_signature_cvars = tuple._2

    //create the directory if not exist
    s"mkdir -p $path" !

    val host_code = CASTPrinter(CAST, path, file)


    println("Compilation done!")
  }


}


