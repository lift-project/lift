package backends.spatial.accel.ir.pattern

import ir.ast.{AbstractMap, IRNode, Lambda, Lambda1, Pattern}
import lift.arithmetic.{ArithExpr, PosVar}

case class MapParStride(override val f: Lambda1, s: ArithExpr, p: ArithExpr) extends AbstractMap(f, "MapSeq",
  PosVar("i")) {
  override def copy(f: Lambda): Pattern = MapParStride(f, s, p)
  var shouldUnroll = false

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)
}

object MapParStride {
  def apply(f: Lambda1, s: ArithExpr, p: ArithExpr) = new MapParStride(f, s, p)
}