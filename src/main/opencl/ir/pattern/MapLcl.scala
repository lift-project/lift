package opencl.ir.pattern

import arithmetic.Var
import ir.ast.{isGenerable, AbstractMap, Lambda1}

/**
 *
 * Applicable rules:
 *  - MapLcl(f) => toGlobal(MapLcl(f))
 *  - MapLcl(f) => toLocal(MapLcl(f))
 *
 * @param dim
 * @param f
 */
case class MapLcl(dim: Int, override val f: Lambda1)
extends AbstractMap(f, "MapLcl", Var("l_id")) with isGenerable

object MapLcl {
  def apply(f: Lambda1) = new MapLcl(0, f) // o is default

  def apply(dim: Int) = (f: Lambda1) => new MapLcl(dim, f)
}