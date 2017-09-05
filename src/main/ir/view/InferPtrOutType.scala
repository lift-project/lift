package ir.view



import ir.{ArrayType, NoType, Type, UndefType}
import ir.ast.{AbstractMap, Expr, FunCall, IRNode, ModifyWrite, UserFun, VectorizeUserFun}
import lift.arithmetic.{?, ArithExpr}
import opencl.ir.{GlobalMemory, LocalMemory, OpenCLAddressSpace, PrivateMemory}
import opencl.ir.pattern._

import scala.collection.mutable


object InferPtrOutType {

  def apply(e: Expr, numDim: Int) : Unit = {

    var wrgDepth = 0
    var lclOrGlbDepth = 0
    val glbLenStack: mutable.Stack[ArithExpr] = mutable.Stack[ArithExpr]()
    val lclLenStack: mutable.Stack[ArithExpr] = mutable.Stack[ArithExpr]()
    val prvLenStack: mutable.Stack[ArithExpr] = mutable.Stack[ArithExpr]()
    var writePtrType = e.t

    def buildArrayType(stack: mutable.Stack[ArithExpr], elemT: Type) : Type = {
      stack.foldLeft(elemT)((t, len) => if (len == ?) ArrayType(t) else ArrayType(t, len))
    }

    def updateWritePtrType(t: Type, as: OpenCLAddressSpace) = {
      if (writePtrType == UndefType) {
        as match {
          case GlobalMemory => writePtrType = buildArrayType(glbLenStack, t)
          case LocalMemory => writePtrType = buildArrayType(lclLenStack, t)
          case PrivateMemory => writePtrType = buildArrayType(prvLenStack, t)
        }
      }
    }

    def prePost(n: IRNode) : (IRNode => Unit) = {
      n match {
        case fc: FunCall =>
          fc.f match {

            case _: ModifyWrite =>
              updateWritePtrType(fc.t, fc.addressSpace)
              (_) => {}

            case _: AbstractMap =>
              // first update the length stacks if we are at the right depth (e.g. in a single workgroup or single thread)
              glbLenStack.push(Type.getLength(fc.t))
              if (wrgDepth==numDim)
                lclLenStack.push(Type.getLength(fc.t))
              if (lclOrGlbDepth==numDim)
                prvLenStack.push(Type.getLength(fc.t))

              // then update the depth information
              fc.f match {
                case _: MapWarp | _:MapGlb | _ : MapLcl | _: MapAtomLcl =>  lclOrGlbDepth+=1
                case _: MapWrg => wrgDepth+=1
                case _ =>
              }

              (n:IRNode) => {
                // reverse everything we did before
                fc.f match {
                  case _: MapWarp | _:MapGlb | _ : MapLcl | _: MapAtomLcl =>  lclOrGlbDepth-=1
                  case _: MapWrg => wrgDepth-=1
                  case _ =>
                }
                glbLenStack.pop
                if (wrgDepth==numDim)
                  lclLenStack.pop
                if (lclOrGlbDepth==numDim)
                  prvLenStack.pop
              }

            case _: UserFun | _:VectorizeUserFun =>
              updateWritePtrType(fc.t, fc.addressSpace)
              fc.outPtrType = writePtrType
              writePtrType = UndefType
              (_) => {}

            case _=>(_) => {}
          }
        case _=>(_) => {}
      }
    }

    IRNode.visitPrePost(e, prePost)
  }

}
