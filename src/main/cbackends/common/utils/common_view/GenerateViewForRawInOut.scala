package cbackends.common.utils.common_view

import ir._
import ir.ast.{Expr, Param}
import ir.view._
import lift.arithmetic.{ArithExpr, Cst}

object GenerateViewForRawInOut {

  def generateViewForRawInOut(p: Expr, t: Type, size: ArithExpr) : View  = {


    /*
    val typ = t.asInstanceOf[ArrayType]
    val ArrayTypeWS(_,s) = typ
    typ.elemT match {
      case et:ArrayType =>
        val ArrayTypeWS(_, n) = et
        generateViewForRawInOut(p, et, size * s).split(n)
      case _ =>
        ViewMem(p.mem.variable, ArrayTypeWSWC(typ.elemT, size * s) )
    }
        */

    t match {
      case typ@ArrayTypeWS(elemT,s) =>
        elemT match {
          case et:ArrayType =>
            val ArrayTypeWS(_, n) = et
            generateViewForRawInOut(p, et, size * s).split(n)

          case tt@TupleType(elemsT@_*) =>
            val tuple_views = elemsT.map( generateViewForRawInOut(p, _, Cst(1)) )
            ViewMemWithInnerView(p.mem.variable,
              ViewTuple(
                tuple_views,
                tt
              ),
              ArrayTypeWSWC(typ.elemT, size * s) )

          case _ =>
            ViewMem(p.mem.variable, ArrayTypeWSWC(typ.elemT, size * s) )

        }

      case st:ScalarType =>
        ViewMemScalar(st)

    }



}

}
