package opencl.ir.pattern

import lift.arithmetic.{PosVar, Var}
import ir.ast._

case class MapLane(override val f: Lambda1) extends AbstractMap(f, "MapLane",
  PosVar("lane_id")) with ParallelPattern  {
  override def copy(f: Lambda): Pattern = MapLane(f)
}
