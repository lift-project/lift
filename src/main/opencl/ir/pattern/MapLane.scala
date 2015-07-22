package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast._

case class MapLane(override val f: Lambda1) extends AbstractMap(f, "MapLane",
                                                                Var("lane_id"))
                                                    with isGenerable {
  override def copy(f: Lambda): Pattern = MapLane(f)
}
