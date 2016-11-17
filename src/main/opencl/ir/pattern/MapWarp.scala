package opencl.ir.pattern

import apart.arithmetic.{PosVar, Var}
import ir.ast._

case class MapWarp(override val f: Lambda1) extends AbstractMap(f, "MapWarp",
  PosVar("warp_id")) {
  override def copy(f: Lambda): Pattern = MapWarp(f)
}
