package backends.spatial.accel.ir.pattern

import ir.ast.{Lambda, Lambda1, Pattern}
import lift.arithmetic.{ArithExpr, Cst}

case class SpSeqForeach(override val chunkSize: ArithExpr,
                         override val stride: ArithExpr = Cst(1),
                         override val f: Lambda1)
  extends AbstractSpForeach(chunkSize, stride, Cst(1), f) with Sequential {
  override def copy(f: Lambda): Pattern = SpSeqForeach(chunkSize, stride, f)
}