package cbackends.common.utils.input_view

import ir.{ArrayType, ArrayTypeWS, Type}
import ir.ast.{Expr, Lambda, Param, Value}
import ir.view.{NoView, View, ViewMem}
import lift.arithmetic.{ArithExpr, Cst}

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

  def generateInputViewForParam(p: Param, t: Type, size: ArithExpr) : View  = {

    val typ = t.asInstanceOf[ArrayType]
    val ArrayTypeWS(_,s) = typ
    typ.elemT match {
      case et:ArrayType =>
        val ArrayTypeWS(_, n) = et
        generateInputViewForParam(p, et, size * s).split(n)
      case _ => ViewMem(p.mem.variable, ArrayTypeWS(typ.elemT, size * s) )
    }

  }

  def init_params(lambda: Lambda) : Unit = {

    //lambda.params.foreach( p => p.view = ViewMem(p.mem.variable, p.t) )

    lambda.params.foreach( p => p.view = generateInputViewForParam(p, p.t, Cst(1)) )

  }

}
