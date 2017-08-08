package ir.view




import ir.Type
import ir.ast.{AbstractMap, Expr, FunCall, IRNode}
import lift.arithmetic.ArithExpr

import scala.collection.mutable


object InferPtrOutType {

  def apply(e: Expr) : Unit = {

    val lengthStack: mutable.Stack[ArithExpr] = mutable.Stack[ArithExpr]()

    def pre(n:IRNode) : Unit = {
      n match {
        case fc: FunCall =>
          fc.f match {
            case _: AbstractMap =>
            lengthStack.push(Type.getLength(fc.t))
          }
      }
    }

    IRNode.visit(e, pre _, post = (n:IRNode) => {})



  }
/*
  private def inferPtrOutType(fc: FunCall) : Type = {
    fc.f match {
      case l: Lambda => InferPtrOutType(l.body)
      case p: Pattern =>

      case t: Tuple => fc.args


      case _: VectorizeUserFun | _: UserFun =>

      case _: Id =>
      case dunno => throw new NotImplementedError(s"InferPtrOutType.scala: $dunno")
    }
  }*/

}
