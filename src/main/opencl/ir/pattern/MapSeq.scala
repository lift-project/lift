package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast._

case class MapSeq(override val f: Lambda1) extends AbstractMap(f, "MapSeq",
                                                               Var("i")) {
  override def copy(f: Lambda): Pattern = MapSeq(f)
}

class MapSeqUnroll(override val f: Lambda1) extends MapSeq(f)

object MapSeqUnroll {
  def apply(f: Lambda1) = new MapSeqUnroll(f)
}

