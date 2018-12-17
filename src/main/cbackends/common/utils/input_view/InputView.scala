package cbackends.common.utils.input_view

import ir.ast.{Expr, Lambda, Value}
import ir.view.{NoView, ViewMem}

object InputView {

  def pre_check(lambda: Lambda) : Unit = {
    lambda visitBy {
      case e: Expr => assert(e.view == NoView)
      case _ =>
    }
  }

  def post_check(lambda: Lambda) : Unit = {

    lambda visitBy {
      case e:Expr if !e.isInstanceOf[Value] => assert( e.view != NoView )
      case _ =>
    }

  }

  def init_params(lambda: Lambda) : Unit = {

    lambda.params.foreach( p => p.view = ViewMem(p.mem.variable, p.t) )

  }

}