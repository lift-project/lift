package backends.spatial.accel.ir.pattern

import ir.ast.{AbstractMap, IRNode, Lambda, Lambda1, Pattern}
import lift.arithmetic.PosVar

case class MapSeq(override val f: Lambda1) extends AbstractMap(f, "MapSeq",
  PosVar("i")) {
  override def copy(f: Lambda): Pattern = MapSeq(f)
  var shouldUnroll = false

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)
}

object MapSeq {
  def apply(f: Lambda1) = new MapSeq(f)
}
