package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast._

/**
 *
 * @param dim
 * @param f
 */
case class MapAtomLcl(dim: Int, override val f: Lambda1, val workVar: Var)
extends AbstractMap(f, "MapAtomLcl", Var("l_id")) {
  override def copy(f: Lambda): Pattern = MapAtomLcl(dim, f, Var("work_idx"))
  var emitBarrier = true
}

object MapAtomLcl {
  def apply(f: Lambda1) = new MapAtomLcl(0, f, Var("work_idx")) // o is default

  def apply(dim: Int) = (f: Lambda1) => new MapAtomLcl(dim, f, Var("work_idx"))
}
