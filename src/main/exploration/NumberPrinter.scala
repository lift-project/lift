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

  def byDepth(lambda: Lambda): collection.Map[Expr, Int] = byDepth(lambda.body)

  def byDepth(expr: Expr): collection.Map[Expr, Int] = (new NumberByDepth)(expr)

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

class NumberByDepth {

  var idMap = collection.Map[Expr, Int]()
  var currentDepth = 0

  def apply(lambda: Lambda): collection.Map[Expr, Int] =
    apply(lambda.body)

  def apply(expr: Expr): collection.Map[Expr, Int] = {
    number(expr)
    idMap
  }

  private def number(expr: Expr): Unit = {
    idMap += (expr -> currentDepth)
    expr match {
      case call: FunCall =>
        number(call.f)
        call.args.foreach(number)
      case _ =>
    }
  }

  private def number(funDecl: FunDecl): Unit = {
    funDecl match {
      case map: AbstractMap =>
        currentDepth += 1
        number(map.f)
        currentDepth -= 1
      case reduce: AbstractPartRed =>
        currentDepth += 1
        number(reduce.f)
        currentDepth -= 1
      case lambda: Lambda => number(lambda.body)
      case fp: FPattern => number(fp.f)
      case _ =>
    }
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

class NumberPrinter(idMap: scala.collection.Map[Expr, Int]) {

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
