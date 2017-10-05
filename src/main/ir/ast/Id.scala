package ir.ast

import ir.Type

case class Id() extends FunDecl(arity = 1) {
  override def checkType(argType: Type, setType: Boolean): Type = argType
}
