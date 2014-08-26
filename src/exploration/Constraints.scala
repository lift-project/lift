package exploration

import ir.{Lambda, FunCall}

class Constraints(val maxMapDepth: Int, val converge: Boolean, val randomOnly: Boolean = false) {

  class Wrap[T <: AnyRef](val value: T) {
    override def hashCode() = value.hashCode
    override def equals(a: Any) = a match {
      case ref: Wrap[_] => ref.value eq value
      case _ => false
    }
  }
  
  def setOnlyTerminal() : Constraints = new Constraints(maxMapDepth, converge)
  
  val fixedFuns : scala.collection.mutable.Set[Wrap[FunCall]] = new scala.collection.mutable.HashSet() // set of function calls that shouldn't be derived any further
  def addFixedFunCall(f: FunCall) = {fixedFuns += new Wrap(f)}
  def canDerive(f: FunCall) = !fixedFuns.contains(new Wrap(f))
}