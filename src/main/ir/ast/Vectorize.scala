package ir.ast

import ir.Type
import lift.arithmetic.ArithExpr

/*
// TODO: discuss if this should be a Pattern again
// TODO: (if so, this could be replaced in the very first pass before type
// TODO:  checking)
case class Vectorize(n: Expr, f: Fun) extends FPattern {
  def isGenerable() = true
  override def copy() = Vectorize(n, f)
}
*/

object Vectorize {
  class Helper(n: ArithExpr) {
    def apply(uf: UserFun): UserFun = uf.vectorize(n)

    def apply(v: Value): Value = v.vectorize(n)

    def apply(p: Param): Param = p.vectorize(n)
  }

  //@deprecated("Use function.vectorize(ArithExpr) instead")
  def apply(n: ArithExpr): Helper = new Helper(n)
}

case class VectorizeFunCall(vecSize: ArithExpr) extends FunDecl(arity = 1) {


  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = argType.vectorize(vecSize)
}
