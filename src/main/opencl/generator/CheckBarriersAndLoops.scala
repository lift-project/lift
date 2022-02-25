package opencl.generator

import ir.ast.{Expr, FunCall, Lambda}
import opencl.ir.pattern.{Barrier, MapLcl}

object CheckBarriersAndLoops {

  def apply(lambda: Lambda): Unit =
    apply(lambda.body)

  def apply(expr: Expr): Unit =
    Expr.visit(expr, _ => Unit, {
      case FunCall(m: MapLcl, _) =>

        // TODO: extend ArithExpr equality check to return false
        // If bounds are not equal, or if they are not evaluable
        if ((m.iterationCount.min !== m.iterationCount.max) ||
          !m.iterationCount.min.isEvaluableGivenEnoughExtraData ||
          !m.iterationCount.max.isEvaluableGivenEnoughExtraData) {
        //if (m.iterationCount == Cst(1) / ?) {
          if (PerformBarrierElimination() &&
            m.f.body.contains({ case FunCall(nested@MapLcl(_, _), _) if nested.emitBarrier => })) {
            throw new IllegalKernel("Kernel contains a barrier that might not be taken by all threads inside\n" + m)
          }
          if (PerformBarrierInsertion() &&
            m.f.body.contains({ case FunCall(Barrier(_, _), _) => })) {
            throw new IllegalKernel("Kernel contains a barrier that might not be taken by all threads inside\n" + m)
          }
        }

      case _ =>
    })

}
