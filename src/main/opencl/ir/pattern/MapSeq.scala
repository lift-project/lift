package opencl.ir.pattern

import lift.arithmetic.PosVar
import ir.ast._

case class MapSeq(override val f: Lambda1) extends AbstractMap(f, "MapSeq",
  PosVar("i")) {
  override def copy(f: Lambda): Pattern = MapSeq(f)
  var shouldUnroll = false

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)
}

class MapSeqUnroll(override val f: Lambda1) extends MapSeq(f) {
  override def copy(f: Lambda): Pattern = MapSeqUnroll(f)
  shouldUnroll = true
}

object MapSeqUnroll {
  def apply(f: Lambda1) = new MapSeqUnroll(f)
}

