package cbackends.global.transformation.funcall2closure

import ir.ast.{FunCall, Lambda, Param}
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



  }

}
