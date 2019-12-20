package backends.spatial.accel.ir.pattern

import ir.ast.{Expr, Lambda, Lambda1, Lambda2, Pattern, fun}
import lift.arithmetic.{ArithExpr, Cst}

case class SpSeqMemFold(override val fMap: Lambda1,
                        override val fReduce: Lambda2,
                        override val chunkSize: ArithExpr,
                        override val stride: ArithExpr)
  extends SpMemFold(fMap, fReduce, chunkSize, stride, Cst(1)) with Sequential {
  override def copy(f1: Lambda, f2: Lambda): Pattern = SpSeqMemFold(f1, f2, chunkSize, stride)
}

object SpSeqMemFold {
  def apply(chunkSize: ArithExpr,
            stride: ArithExpr = Cst(1),
            fMap: Lambda, fReduce: Lambda2,
            init: Expr): Lambda1 =
    fun((x) => SpSeqMemFold(fMap, fReduce, chunkSize, stride)(init, x))
}

