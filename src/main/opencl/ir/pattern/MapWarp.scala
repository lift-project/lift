package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast._

case class MapWarp(override val f: Lambda1) extends AbstractMap(f, "MapWarp",
                                                                Var("warp_id"))
                                                    with isGenerable {
  override def copy(f: Lambda): Pattern = MapWarp(f)
}
