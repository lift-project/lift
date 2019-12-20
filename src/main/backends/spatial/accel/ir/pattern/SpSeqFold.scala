package backends.spatial.accel.ir.pattern

import ir.ast.{Expr, Lambda, Lambda1, Lambda2, Pattern, fun}
import lift.arithmetic.{ArithExpr, Cst}

case class SpSeqFold(override val fMap: Lambda1,
                     override val fReduce: Lambda2,
                     override val chunkSize: ArithExpr,
                     override val stride: ArithExpr)
  extends SpFold(fMap, fReduce,chunkSize, stride, Cst(1)) with Sequential {
  override def copy(f1: Lambda, f2: Lambda): Pattern = SpSeqFold(f1, f2, chunkSize, stride)
}

object SpSeqFold {
  def apply(chunkSize: ArithExpr,
            stride: ArithExpr = Cst(1),
            fMap: Lambda, fReduce: Lambda2,
            init: Expr): Lambda1 =
    fun((x) => SpSeqFold(fMap, fReduce, chunkSize, stride)(init, x))
}