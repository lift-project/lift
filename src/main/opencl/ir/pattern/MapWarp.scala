package opencl.ir.pattern

import ir.ast._
import lift.arithmetic.PosVar

case class MapWarp(override val f: Lambda1) extends AbstractMap(f, "MapWarp",
  PosVar("warp_id")) {
  override def copy(f: Lambda): Pattern = MapWarp(f)
}
