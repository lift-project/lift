package backends.spatial.accel.ir.pattern

import ir.ast.{Lambda, Lambda1, Pattern}
import lift.arithmetic.{ArithExpr, Cst}

case class SpPipeForeach(override val chunkSize: ArithExpr,
                         override val stride: ArithExpr = Cst(1),
                         override val factor: ArithExpr = Cst(1),
                         override val f: Lambda1)
  extends AbstractSpForeach(chunkSize, stride, factor, f) with Piped {
  override def copy(f: Lambda): Pattern = SpPipeForeach(chunkSize, stride, factor, f)
}