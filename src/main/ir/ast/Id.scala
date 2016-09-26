package ir.ast

import ir.{Type, UndefType}

case class Id() extends FunDecl(arity = 1) {
  /**
   * Indicating if it is possible to generate code for this function declaration.
   * Might be overwritten by a subclass or by mixing in the `isGenerable` trait.
   */
  override def isGenerable: Boolean = false

  override def checkType(argType: Type, setType: Boolean): Type = argType

}
