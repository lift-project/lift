package analysis

import analysis.ParMapIterations.ParKind.ParKind
import ir.ast.{AbstractMap, Expr, FunCall, Lambda}
import ir.{ArrayType, Size}
import lift.arithmetic.ArithExpr
import opencl.ir.pattern.{MapGlb, MapLcl, MapWrg}

import scala.collection.immutable.ListMap

/**
 * Traverses a lambda and returns the number of elements of the argument to Map(Lcl|Wrg|Glb)
 * */
object ParMapIterations {
  object ParKind extends Enumeration {
    type ParKind = Value
    val Local, Workgroup, Global = Value
  }

  case class ParMapDescriptor(mapKind: ParKind, dim: Int)

  def apply(lambda: Lambda): ListMap[ParMapDescriptor, List[ArithExpr]] = {

    Expr.visitWithStateDepthFirst(ListMap[ParMapDescriptor, List[ArithExpr]]())(lambda.body, {

      case (FunCall(m: AbstractMap, mapArg), mapIterations) =>
        assert(mapArg.t.isInstanceOf[ArrayType with Size])

        val argSize = mapArg.t.asInstanceOf[ArrayType with Size].size

        (m match {
          case ml: MapLcl => Some(ParMapDescriptor(ParKind.Local, ml.dim))
          case mw: MapWrg => Some(ParMapDescriptor(ParKind.Workgroup, mw.dim))
          case mg: MapGlb => Some(ParMapDescriptor(ParKind.Global, mg.dim))
          case _ => None
        }) match {
          case None =>
            mapIterations
          case Some(parMapKind) =>
            mapIterations + (parMapKind -> (mapIterations.getOrElse(parMapKind, List()) :+ argSize))
        }
      case (_, mapIterations) => mapIterations
    })
  }
}
