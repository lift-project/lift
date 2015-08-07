package exploration

import ir.ast._

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
      case split: Split => s"Split(_)"
      case reduce: Reduce => s"Reduce(${apply(reduce.f)})"
      case reduce: PartRed => s"PartRed(${apply(reduce.f)})"
      case uf: UserFun => "_"
      case _ => funDecl.toString
    }
  }
}
