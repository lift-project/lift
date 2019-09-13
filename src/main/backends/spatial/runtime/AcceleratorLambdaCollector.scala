package backends.spatial.runtime

import backends.spatial.host.ir.AccelFun
import ir.ast.{Expr, FunCall, Lambda}

object AcceleratorLambdaCollector {
  def apply(lambda: Lambda): List[Lambda] = {
    Expr.visitWithState(List[Lambda]())(
      expr = lambda.body,
      visitFun = (expr: Expr, accelLambdas: List[Lambda]) =>
        expr match {
          case fc @ FunCall(af: AccelFun, _*) =>  accelLambdas :+ af.f
          case _ =>                               accelLambdas
        },
      visitArgs = true
    )
  }
}
