package opencl.ir.pattern

import apart.arithmetic.{PosVar, Var}
import ir.ast._

case class MapLane(override val f: Lambda1) extends AbstractMap(f, "MapLane",
  PosVar("lane_id")) {
  override def copy(f: Lambda): Pattern = MapLane(f)
}
