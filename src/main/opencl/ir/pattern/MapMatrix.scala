package opencl.ir.pattern

import arithmetic.Var
import ir.ast.{MapCall, Expr, Lambda1}
import opencl.ir.ast.GenerableMap

// Map over a matrix - more abstract, to please the typechecker

case class MapMatrix(dim: Int, f: Lambda1) extends GenerableMap(f) {
  override def apply(args: Expr*) : MapCall = {
    assert(args.length == 1)
    new MapCall("MapMatrix", Var("wg_id"), this, args(0))
  }

  override def $(that: Expr) : MapCall = {
    apply(that)
  }
}

object MapMatrix {
  def apply(f: Lambda1) = new MapMatrix(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapMatrix(dim, f)
}