package cbackends.global.transformation.funcall2closure

import ir.ast.{Expr, FunCall, Lambda, Param}
import jdk.internal.org.objectweb.asm.tree.analysis.Value

import scala.collection.mutable

object FunCall2Closure {

  def apply(fc: FunCall) : Lambda = {

    val all_params = mutable.Set.empty[Param]
    val bounded_params = mutable.Set.empty[Param]

    fc visitBy {
      case p : Param if !p.isInstanceOf[Value] => all_params += p
      case l : Lambda => bounded_params ++= l.params
    }

    val unbounded_params = all_params -- bounded_params
    val new_params = unbounded_params.map(p => Param(p.t))
    val replace_plan = unbounded_params zip new_params
    val replaced_expr = replace_plan.isEmpty match {
      case true => fc
      case false => (fc /:[Expr] replace_plan) { (acc, pair) => Expr.replace(acc, pair._1, pair._2) }
    }

    Lambda( new_params.toArray, replaced_expr )



  }

}
