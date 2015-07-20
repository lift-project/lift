package opencl.ir.pattern

import arithmetic.Var
import ir.ast.Lambda1
import opencl.ir.ast.GenerableMap

case class MapGlb(dim: Int, override val f: Lambda1)
  extends GenerableMap(f, "MapGlbl", Var("gl_id"))

object MapGlb {
  def apply(f: Lambda1) = new MapGlb(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapGlb(dim, f)
}
