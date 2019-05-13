package cbackends.common.utils.input_view

import ir.ast.{Expr, Lambda, Value}
import ir.view.{NoView}
import lift.arithmetic.Cst
import cbackends.common.utils.common_view.GenerateViewForRawInOut.generateViewForRawInOut2

object InputView {

  def pre_check(lambda: Lambda) : Unit = {

    lambda visitBy {
      case e: Expr => e.view = NoView
      case _ =>
    }


    lambda visitBy {
      case e: Expr => assert(e.view == NoView)
      case _ =>
    }
  }

  def post_check(lambda: Lambda) : Unit = {

    lambda visitBy {
      case e:Expr if !e.isInstanceOf[Value] =>
        assert( e.view != NoView )
      case _ =>
    }

  }


  def init_params(lambda: Lambda) : Unit = {

    //lambda.params.foreach( p => p.view = ViewMem(p.mem.variable, p.t) )

    lambda.params.foreach( p => p.view = generateViewForRawInOut2(p, p.t, Cst(1)) )

  }

}
