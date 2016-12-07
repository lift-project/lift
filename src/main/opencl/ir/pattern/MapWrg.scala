package opencl.ir.pattern

import lift.arithmetic.{PosVar, Var}
import ir.ast._

case class MapWrg(dim: Int, override val f: Lambda1)
  extends AbstractMap(f, "MapWrg", PosVar("wg_id")) {
  override def copy(f: Lambda): Pattern = MapWrg(dim, f)
}


object MapWrg {
  def apply(f: Lambda1) = new MapWrg(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapWrg(dim, f)
}
