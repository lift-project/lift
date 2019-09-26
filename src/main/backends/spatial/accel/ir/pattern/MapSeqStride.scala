package backends.spatial.accel.ir.pattern

import ir.ast.{AbstractMap, IRNode, Lambda, Lambda1, Pattern}
import lift.arithmetic.{ArithExpr, PosVar}

case class MapSeqStride(override val f: Lambda1, s: ArithExpr) extends AbstractMap(f, "MapSeq",
  PosVar("i")) {
  override def copy(f: Lambda): Pattern = MapSeqStride(f, s)
  var shouldUnroll = false

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)
}

object MapSeqStride {
  def apply(f: Lambda1, s: ArithExpr) = new MapSeqStride(f, s)
}