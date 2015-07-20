package opencl.ir.pattern

import arithmetic.Var
import ir.ast.{MapCall, Expr, Lambda1}
import opencl.ir.ast.GenerableMap

case class MapWarp(f: Lambda1) extends GenerableMap(f) {
  override def apply(args: Expr*) : MapCall = {
    assert(args.length == 1)
    new MapCall("MapWarp", Var("warp_id"), this, args(0))
  }

  override def $(that: Expr) : MapCall = {
    apply(that)
  }
}