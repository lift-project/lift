package exploration.utils

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
      case opencl.ir.Float => "Float"
      case opencl.ir.Int => "Int"
      case opencl.ir.Double => "Double"
      case TupleType(tt@_*) => s"TupleType(${tt.map(apply).mkString(", ")})"
      case VectorType(elemT, len) => s"VectorType(${apply(elemT)}, $len)"
      case ArrayType(elemT, len) => s"ArrayType(${apply(elemT)}, $len)"
    }
  }

  def apply(uf: UserFun): String = {
    val name = "\"" + uf.name + "\""
    val paramNames = uf.paramNames.map("\"" + _ + "\"").mkString(", ")
    val inTs = uf.inTs.map(apply).mkString(", ")
    val outT = apply(uf.outT)

    val body = "\"\"\"" + uf.body.split("\n").map("|" + _).mkString("\n") + "\"\"\".stripMargin"

    s"val ${uf.name} = UserFun($name, Array($paramNames), $body, Seq($inTs), $outT)"
  }
}
