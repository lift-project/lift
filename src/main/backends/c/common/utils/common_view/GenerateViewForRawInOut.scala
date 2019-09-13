package backends.c.common.utils.common_view

import ir._
import ir.ast.{Expr, Param}
import ir.view._
import lift.arithmetic.{ArithExpr, Cst}

object GenerateViewForRawInOut {

  def generateViewForRawInOut(p: Expr, t: Type, size: ArithExpr, id : Int = 0, outputViewConstruct : Boolean = false) : View  = {


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
            outputViewConstruct match {
              case false =>
                val ids = (0 until elemsT.length)
                val tuple_views = (ids zip elemsT).map{
                  case (id, elemT) =>
                    generateViewForRawInOut(p, elemT, Cst(1), id)
                }
                ViewMemWithInnerView(p.mem.variable,
                  ViewTuple(
                    tuple_views,
                    tt
                  ),
                  ArrayTypeWSWC(typ.elemT, size * s) )

              case true => ViewMem(p.mem.variable, ArrayTypeWSWC(typ.elemT, size * s) )

            }

          case _ =>
            ViewMem(p.mem.variable, ArrayTypeWSWC(typ.elemT, size * s) )

        }

      case st:ScalarType =>
        ViewMemScalar(Cst(id),st)

    }



}

  def generateViewForRawInOut2(p: Expr, t: Type, size: ArithExpr) : View  = {

    t match {
      case typ@ArrayTypeWS(elemT,s) =>
        elemT match {
          case et:ArrayType =>
            val ArrayTypeWS(_, n) = et
            generateViewForRawInOut2(p, et, size * s).split(n)

          case _ =>
            ViewMem(p.mem.variable, ArrayTypeWSWC(typ.elemT, size * s) )

        }


    }

  }

}
