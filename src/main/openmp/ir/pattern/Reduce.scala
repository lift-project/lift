package openmp.ir.pattern

import ir.Type
import ir.ast.{Expr, Lambda, UserFun, fun}
import opencl.ir.pattern.ReduceSeq

class ReduceParImpl(val op:ReduceOp) extends ReduceSeq(op.f)

object ReduceParImpl {
  def apply(op:ReduceOp) = new ReduceParImpl(op)
}

class ReduceOp(val f:Lambda, val pragmaSymbol:String)

class :+(t:Type) extends ReduceOp(UserFun("ompAdd"+t,Array("x","y"),"return x + y", Seq(t,t),t),"+")
object :+ {
  def apply(t:Type) = new :+(t)
}

object ReducePar {
  def apply(op:ReduceOp, init:Expr) = fun((x) => ReduceParImpl(op)(init,x))
}