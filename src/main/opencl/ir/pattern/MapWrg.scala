package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast.{isGenerable, AbstractMap, Lambda1}

case class MapWrg(dim: Int, override val f: Lambda1)
  extends AbstractMap(f, "MapWrg", Var("wg_id")) with isGenerable

object MapWrg {
  def apply(f: Lambda1) = new MapWrg(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapWrg(dim, f)
}
