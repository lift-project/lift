package opencl.ir.pattern

import arithmetic.Var
import ir.ast.{isGenerable, AbstractMap, Lambda1}

case class MapGlb(dim: Int, override val f: Lambda1)
  extends AbstractMap(f, "MapGlbl", Var("gl_id")) with isGenerable

object MapGlb {
  def apply(f: Lambda1) = new MapGlb(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapGlb(dim, f)
}
