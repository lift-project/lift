package backends.spatial.accel.ir.pattern

import ir.ast.{Expr, Lambda, Lambda1, Lambda2, fun}
import lift.arithmetic.{ArithExpr, Cst}

case class SpPipeFold(override val fMap: Lambda1,
                      override val fReduce: Lambda2,
                      override val chunkSize: ArithExpr,
                      override val stride: ArithExpr,
                      override val factor: ArithExpr)
  extends SpFold(fMap, fReduce,chunkSize, stride, factor)

object SpPipeFold {
  def apply(chunkSize: ArithExpr,
            stride: ArithExpr = Cst(1),
            factor: ArithExpr = Cst(1),
            fMap: Lambda, fReduce: Lambda2,
            init: Expr): Lambda1 =
    fun((x) => SpPipeFold(fMap, fReduce, chunkSize, stride, factor)(init, x))
}

