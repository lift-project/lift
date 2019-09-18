package backends.spatial.accel

import core.generator.GenericAST.{AstNode, ExpressionT, MutableBlockT, Pipe, StatementT, VarRefT}
import core.generator.PrettyPrinter._

object SpatialAccelAST {

  trait CounterT extends ExpressionT {
    val min: ExpressionT
    val max: ExpressionT
    val stride: ExpressionT
    val factor: ExpressionT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (min.visit(_)(visitFun)) |>
        (max.visit(_)(visitFun)) |>
        (stride.visit(_)(visitFun)) |>
        (factor.visit(_)(visitFun))
    }

    override def print(): Doc = {
      min.print <> text("until") <> max.print <> text("by") <>
        stride.print <> text("par") <> factor.print
    }
  }

  case class Counter(min: ExpressionT,
                     max: ExpressionT,
                     stride: ExpressionT,
                     factor: ExpressionT) extends CounterT {
    def _visitAndRebuild(pre: (AstNode) => AstNode,  post: (AstNode) => AstNode) : AstNode = {
      Counter(min.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        max.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        stride.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        factor.visitAndRebuild(pre, post).asInstanceOf[ExpressionT])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      min.visitBy(pre, post)
      max.visitBy(pre, post)
      stride.visitBy(pre, post)
      factor.visitBy(pre, post)
    }
  }

  trait ReduceT extends StatementT {
    val accum: AstNode
    val counter: List[CounterT]
    val mapFun: MutableBlockT
    val reduceFun: MutableBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        // Visit internal expressions of a for loop
        (accum.visit(_)(visitFun)) |>
        (counter.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        }) |>
        (mapFun.visit(_)(visitFun)) |>
        (reduceFun.visit(_)(visitFun))
    }

    override def print(): Doc = {
      text("Reduce(") <> accum.print <> text(")") <>
        text("(") <> counter.map(_.print()).reduce(_ <> text(",") <> _) <> text(")") <>
        mapFun.print <> reduceFun.print
    }
  }

  case class Reduce(accum: AstNode,
                    counter: List[CounterT],
                    mapFun: MutableBlockT,
                    reduceFun: MutableBlockT) extends ReduceT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      Reduce(accum.visitAndRebuild(pre, post),
        counter.map(_.visitAndRebuild(pre, post).asInstanceOf[CounterT]),
        mapFun.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT],
        reduceFun.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {
      accum.visitBy(pre, post)
      counter.foreach(_.visitBy(pre, post))
      mapFun.visitBy(pre, post)
      reduceFun.visitBy(pre, post)
    }
  }
}
