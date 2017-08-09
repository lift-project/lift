package ir.view



import ir.{ArrayType, Type}
import ir.ast.{AbstractMap, Expr, FunCall, IRNode, UserFun, VectorizeUserFun}
import lift.arithmetic.{?, ArithExpr}
import opencl.ir.{GlobalMemory, LocalMemory, PrivateMemory}
import opencl.ir.pattern._

import scala.collection.mutable


object InferPtrOutType {

  def apply(e: Expr) : Unit = {

    var wrgDepth = 0
    var lclOrGlbDepth = 0
    val glbLenStack: mutable.Stack[ArithExpr] = mutable.Stack[ArithExpr]()
    val lclLenStack: mutable.Stack[ArithExpr] = mutable.Stack[ArithExpr]()
    val prvLenStack: mutable.Stack[ArithExpr] = mutable.Stack[ArithExpr]()

    def buildArrayType(stack: mutable.Stack[ArithExpr], elemT: Type) : Type = {
      stack.foldLeft(elemT)((t, len) => if (len == ?) ArrayType(t) else ArrayType(t, len))
    }

    def prePost(n: IRNode) : (IRNode => Unit) = {
      n match {
        case fc: FunCall =>
          fc.f match {
            case _: AbstractMap =>
              fc.f match {
                case _: MapWarp | _:MapGlb | _ : MapLcl | _: MapAtomLcl =>  lclOrGlbDepth+=1
                case _: MapWrg => wrgDepth+=1
                case _ =>
              }
              glbLenStack.push(Type.getLength(fc.t))
              if (wrgDepth>0)
                lclLenStack.push(Type.getLength(fc.t))
              if (lclOrGlbDepth>0)
                prvLenStack.push(Type.getLength(fc.t))

              (n:IRNode) => {
                glbLenStack.pop
                if (wrgDepth>0)
                  lclLenStack.pop
                if (lclOrGlbDepth>0)
                  prvLenStack.pop
                fc.f match {
                  case _: MapWarp | _:MapGlb | _ : MapLcl | _: MapAtomLcl =>  lclOrGlbDepth-=1
                  case _: MapWrg => wrgDepth-=1
                  case _ =>
                }
              }

            case _: UserFun | _:VectorizeUserFun =>
              fc.outPtrType = fc.addressSpace match {
                case GlobalMemory => buildArrayType(glbLenStack,fc.t)
                case LocalMemory => buildArrayType(lclLenStack,fc.t)
                case PrivateMemory => buildArrayType(prvLenStack,fc.t)
              }
              (n:IRNode) => {}

            case _=>(n:IRNode) => {}
          }
        case _=>(n:IRNode) => {}
      }
    }

    IRNode.visitPrePost(e, prePost)
  }

}
