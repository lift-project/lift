package opencl.ir.pattern

import lift.arithmetic.PosVar
import ir.ast._

case class MapSeq(override val f: Lambda1) extends AbstractMap(f, "MapSeq",
  PosVar("i")) {
  override def copy(f: Lambda): Pattern = MapSeq(f)
  var shouldUnroll = false
}

class MapSeqUnroll(override val f: Lambda1) extends MapSeq(f) {
  shouldUnroll = true
}

object MapSeqUnroll {
  def apply(f: Lambda1) = new MapSeqUnroll(f)
}

