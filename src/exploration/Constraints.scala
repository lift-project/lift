package exploration

import ir.FunExpr

class Constraints(val maxMapDepth: Int, val converge: Boolean, val randomOnly: Boolean = false) {

  class Wrap[T <: AnyRef](val value: T) {
    override def hashCode() = value.hashCode
    override def equals(a: Any) = a match {
      case ref: Wrap[_] => ref.value eq value
      case _ => false
    }
  }
  
  def setOnlyTerminal() : Constraints = new Constraints(maxMapDepth, converge)
  
  val fixedFuns : scala.collection.mutable.Set[Wrap[FunExpr]] = new scala.collection.mutable.HashSet() // set of functions that shouldn't be derived
  def addFixedFun(f: FunExpr) = {fixedFuns += new Wrap(f)}
  def canDerive(f: FunExpr) = !fixedFuns.contains(new Wrap(f))
}