package opencl.ir.pattern

import lift.arithmetic.{PosVar, Var}
import ir.ast._

case class MapWarp(override val f: Lambda1) extends AbstractMap(f, "MapWarp",
  PosVar("warp_id")) with ParallelPattern  {
  override def copy(f: Lambda): Pattern = MapWarp(f)
}
