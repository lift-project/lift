package exploration

import ir.ast._


object NumberPrinter {
  def apply(lambda: Lambda): String = (new NumberPrinter())(lambda)
}

private class NumberPrinter() {

  private var idMap = scala.collection.Map[Expr, Int]()

  def apply(lambda: Lambda): String = {
    Expr.visitWithState(0)(lambda.body, (e, count) => {
      idMap += (e -> count)
      count + 1
    })

    constructString(lambda)
  }

  private def constructString(expr: Expr): String = {
    val prefix = idMap(expr) + "_"

    val s = expr match {
      case funCall: FunCall => s"FunCall(${constructString(funCall.f)}, ${funCall.args.map(constructString).mkString(", ")})"
      case _ => expr.toString
    }

    prefix + s
  }

  private def constructString(funDecl: FunDecl): String = {
    funDecl match {
      case lambda: Lambda => s"Lambda(${lambda.params.map(constructString).mkString(", ")}, \n${constructString(lambda.body).split("\n").map("  " + _ + "\n").mkString })"
      case fp: FPattern => s"${fp.getClass.getSimpleName}(${constructString(fp.f)})"
      case _ => funDecl.toString
    }
  }
}
