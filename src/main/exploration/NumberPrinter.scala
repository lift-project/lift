package exploration

import ir.ast._

object NumberExpression {
  def depthFirst(lambda: Lambda): collection.Map[Expr, Int] = {
    depthFirst(lambda.body)
  }

  def depthFirst(expr: Expr): collection.Map[Expr, Int] = {
    number(expr, Expr.visitWithStateDepthFirst(0))
  }

  def breadthFirst(lambda: Lambda): collection.Map[Expr, Int] = {
    breadthFirst(lambda.body)
  }

  def breadthFirst(expr: Expr): collection.Map[Expr, Int] = {
    number(expr, (a, b) =>
      Expr.visitWithState(0)(a, b, visitArgs = true))
  }

  private def number(expr: Expr, visitFunction: ((Expr, ((Expr, Int) => Int)) => Int)) = {
    var idMap = collection.Map[Expr, Int]()

    visitFunction(expr, (e, count) => {
      idMap += (e -> count)
      count + 1
    })

    idMap
  }
}

object NumberPrinter {
  def apply(lambda: Lambda): String =
    (new NumberPrinter(NumberExpression.breadthFirst(lambda)))(lambda)

  def apply(expr: Expr): String =
    (new NumberPrinter(NumberExpression.breadthFirst(expr)))(expr)
}

object DepthFirstNumberPrinter {
  def apply(lambda: Lambda): String =
    (new NumberPrinter(NumberExpression.depthFirst(lambda)))(lambda)

  def apply(expr: Expr): String =
    (new NumberPrinter(NumberExpression.depthFirst(expr)))(expr)
}

private class NumberPrinter(idMap: scala.collection.Map[Expr, Int]) {

  private[exploration] def apply(expr: Expr): String = {
    val prefix = if (idMap.isDefinedAt(expr)) idMap(expr) + "_" else ""

    val s = expr match {
      case funCall: FunCall => s"FunCall(${apply(funCall.f)}, ${funCall.args.map(apply).mkString(", ")})"
      case _ => expr.toString
    }

    prefix + s
  }

  private[exploration] def apply(funDecl: FunDecl): String = {
    funDecl match {
      case lambda: Lambda => s"Lambda(${lambda.params.map(apply).mkString(", ")}, \n${apply(lambda.body).split("\n").map("  " + _ + "\n").mkString })"
      case fp: FPattern => s"${fp.getClass.getSimpleName}(${apply(fp.f)})"
      case _ => funDecl.toString
    }
  }
}
