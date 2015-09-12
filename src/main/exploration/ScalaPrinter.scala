package exploration

import apart.arithmetic.Cst
import ir._
import ir.ast._
import opencl.ir.pattern._

object ScalaPrinter {
  def apply(expr: Expr): String = {
    expr match {
      case funCall: FunCall => s"FunCall(${apply(funCall.f)}, ${funCall.args.map(apply).mkString(", ")})"
      case value: Value => s"Value(${value.value}, ${apply(value.t)})"
      case param: Param => "p_" + param.hashCode()
      case _ => expr.toString
    }
  }

  def apply(funDecl: FunDecl): String = {
    funDecl match {
      case lambda: Lambda => s"fun((${lambda.params.map(apply).mkString(", ")}) => ${apply(lambda.body)})"
      case map: Map => s"Map(${apply(map.f)})"
      case mapSeq: MapSeq => s"MapSeq(${apply(mapSeq.f)})"
      case mapWrg: MapWrg => s"MapWrg(${mapWrg.dim})(${apply(mapWrg.f)})"
      case mapLcl: MapLcl => s"MapLcl(${mapLcl.dim})(${apply(mapLcl.f)})"
      case mapGlb: MapGlb => s"MapGlb(${mapGlb.dim})(${apply(mapGlb.f)})"
      case reduceSeq: ReduceSeq => s"ReduceSeq(${apply(reduceSeq.f)})"
      case reduce: Reduce => s"Reduce(${apply(reduce.f)})"
      case reduce: PartRed => s"PartRed(${apply(reduce.f)})"
      case toGlobal: toGlobal => s"toGlobal(${apply(toGlobal.f)})"
      case toLocal: toLocal => s"toLocal(${apply(toLocal.f)})"
      case toPrivate: toPrivate => s"toPrivate(${apply(toPrivate.f)})"
      case _ => funDecl.toString
    }
  }

  def apply(t: Type): String = {
    t match {
      case ScalarType("float", Cst(4)) => "Float"
      case ScalarType("int", Cst(4)) => "Int"
      case ArrayType(elemT, len) => s"ArrayType(${apply(elemT)}, $len)"
    }
  }
}
