package host

import ir.{TypeChecker, UndefType}
import ir.ast.{Expr, IRNode, Lambda}

object CompileHost {

  def apply(lambda: Lambda, path: String) ={

    val final_lambda = lambda.asInstanceOf[Lambda]

    final_lambda.params.foreach(p => assert(p.t != UndefType))
    TypeChecker(final_lambda)

    lambda.visit( pre = {node:IRNode =>
      node match {
        case e:Expr => assert( e.view != UndefType )
        case _ =>
      }
    })


    println("All done!")
  }


}


