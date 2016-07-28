package openmp.ir.pattern

import ir.Type
import ir.ast.{Expr, Lambda, UserFun, fun}
import opencl.ir.pattern.ReduceSeq

//The implementation class for the reduce parallel. Behaves like a sequential reduction,
//but can only be constructed with a parallel reduction operation
class ReduceParImpl(val op:ReduceOp) extends ReduceSeq(op.f)

object ReduceParImpl {
  def apply(op:ReduceOp) = new ReduceParImpl(op)
}
//Represents a parallel reduction operator. The lambda is the body of the reduction,
//whil the pragma symbol is the symbol used in the OpenMP pragma
class ReduceOp(val f:Lambda, val pragmaSymbol:String)

//Some reductions: addition and subtraction
class :+(t:Type) extends ReduceOp(UserFun("ompAdd"+t,Array("x","y"),"return x + y", Seq(t,t),t),"+")
object :+ {
  def apply(t:Type) = new :+(t)
}

class :-(t:Type) extends ReduceOp(UserFun("ompAdd"+t,Array("x","y"),"return x - y", Seq(t,t),t),"-")
object :- {
  def apply(t:Type) = new :-(t)
}

//Nice constructor the reduce parallel implementation
object ReducePar {
  def apply(op:ReduceOp, init:Expr) = fun((x) => ReduceParImpl(op)(init,x))
}