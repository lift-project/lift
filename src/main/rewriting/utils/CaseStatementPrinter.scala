package rewriting.utils

import ir.ast._
import opencl.ir.pattern.{MapSeq, ReduceSeq, SlideSeqPlus, toGlobal}

object CaseStatementPrinter {

  def apply(expr: Expr): String = {
    expr match {
      case funCall: FunCall => s"FunCall(${apply(funCall.f)}, ${funCall.args.map(apply).mkString(", ")})"
      case param: Param => "_"
      case _ => expr.toString
    }
  }

  def apply(funDecl: FunDecl): String = {
    funDecl match {
      case lambda: Lambda => s"Lambda(_, ${apply(lambda.body)})"
      case map: Map => s"Map(${apply(map.f)})"
      case mapSeq: MapSeq => s"MapSeq(_)"
      case split: Split => s"Split(_)"
      case reduce: Reduce => s"Reduce(${apply(reduce.f)})"
      case reduceSeq: ReduceSeq => s"ReduceSeq(_)"
      case scanplus: SlideSeqPlus => s"ScanPlus(_)"
      case reduce: PartRed => s"PartRed(${apply(reduce.f)})"
      case toGlobal: toGlobal => s"toGlobal(_)"
      case uf: UserFun => "_"
      case _ => funDecl.toString
    }
  }
}
