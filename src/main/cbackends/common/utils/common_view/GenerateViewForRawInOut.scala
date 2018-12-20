package cbackends.common.utils.common_view

import ir.{ArrayType, ArrayTypeWS, Type}
import ir.ast.{Expr, Param}
import ir.view.{View, ViewMem}
import lift.arithmetic.ArithExpr

object GenerateViewForRawInOut {

  def generateViewForRawInOut(p: Expr, t: Type, size: ArithExpr) : View  = {

    val typ = t.asInstanceOf[ArrayType]
    val ArrayTypeWS(_,s) = typ
    typ.elemT match {
      case et:ArrayType =>
        val ArrayTypeWS(_, n) = et
        generateViewForRawInOut(p, et, size * s).split(n)
      case _ => ViewMem(p.mem.variable, ArrayTypeWS(typ.elemT, size * s) )
    }

  }

}
