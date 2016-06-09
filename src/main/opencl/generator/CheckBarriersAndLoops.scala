package opencl.generator

import ir.ast.{Expr, FunCall, Lambda}
import opencl.ir.pattern.MapLcl

object CheckBarriersAndLoops {

  def apply(lambda: Lambda): Unit =
    apply(lambda.body)

  def apply(expr: Expr): Unit =
    Expr.visit(expr, _ => Unit, {
      case FunCall(m: MapLcl, _) =>

        if (m.iterationCount.min != m.iterationCount.max) {
        //if (m.iterationCount == Cst(1) / ?) {
          if (m.f.body.contains({ case FunCall(nested@MapLcl(_, _), _) if nested.emitBarrier => })) {
            throw new IllegalKernel("Kernel contains a barrier not taken by all threads inside\n" + m)
          }
        }

      case _ =>
    })

}
