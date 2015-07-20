package opencl.ir.pattern

import arithmetic.Var
import ir.ast.Lambda1
import opencl.ir.ast.GenerableMap

case class MapWrg(dim: Int, override val f: Lambda1)
  extends GenerableMap(f, "MapWrg", Var("wg_id"))

object MapWrg {
  def apply(f: Lambda1) = new MapWrg(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapWrg(dim, f)
}
