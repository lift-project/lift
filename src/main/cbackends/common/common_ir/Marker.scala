package cbackends.common.common_ir

import ir.Type
import ir.ast.{Expr, FunCall, FunDecl, IRNode}


/**
  * Marker hello world test
  */
object Marker2 {

  def apply(arg: Expr): FunCall = FunCall(Marker2(), arg)

}

case class Marker2(enable: Boolean = false) extends FunDecl(arity = 1) {
  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = argType
}


/**
  * Marker For Naums's NN expression
  */
object Marker3 {

  def apply(arg: Expr): FunCall = FunCall(Marker3(), arg)

}

case class Marker3(name: String = "default_name", groupID: Int = 0, tunable_params : List[Int] = List(), cancelCombo: List[Int] = List())
  extends FunDecl(arity = 1) {

  assert(tunable_params.length == cancelCombo.length);

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = argType
}
