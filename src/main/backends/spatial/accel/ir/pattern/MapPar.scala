package backends.spatial.accel.ir.pattern

import ir.ast.{AbstractMap, Lambda, Lambda1, Pattern}
import lift.arithmetic.PosVar

case class MapPar(override val f: Lambda1)
  extends AbstractMap(f, "MapPar", PosVar("p_id")) {
  override def copy(f: Lambda): Pattern = MapPar(f)
  var shouldUnroll = false
}

object MapPar {
  def apply(f: Lambda1) = new MapPar(f)
}
