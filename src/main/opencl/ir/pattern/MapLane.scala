package opencl.ir.pattern

import ir.ast._
import lift.arithmetic.PosVar

case class MapLane(override val f: Lambda1) extends AbstractMap(f, "MapLane",
  PosVar("lane_id")) {
  override def copy(f: Lambda): Pattern = MapLane(f)
}
