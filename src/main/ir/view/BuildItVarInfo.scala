package ir.view

import ir.{ArrayType, Type}
import ir.ast.{AbstractMap, Expr, FunCall, FunDecl, IRNode}
import lift.arithmetic.Var
import opencl.ir.pattern._

import scala.collection.mutable

object BuildItVarInfo {

  def apply(e: Expr, numDim: Int) : Unit = {

    var wrgDepth = 0
    var lclOrGlbDepth = 0

    val glbStack: mutable.Stack[(Var,  (Type) => ArrayType)] = mutable.Stack()
    val lclStack: mutable.Stack[(Var,  (Type) => ArrayType)] = mutable.Stack()
    val prvStack: mutable.Stack[(Var,  (Type) => ArrayType)] = mutable.Stack()

    val glbWriteParallelStack: mutable.Stack[(Var,  (Type) => ArrayType)] = mutable.Stack()
    val lclWriteParallelStack: mutable.Stack[(Var,  (Type) => ArrayType)] = mutable.Stack()
    val prvWriteParallelStack: mutable.Stack[(Var,  (Type) => ArrayType)] = mutable.Stack()

    val glbReadParallelStack: mutable.Stack[(Var,  (Type) => ArrayType)] = mutable.Stack()
    val lclReadParallelStack: mutable.Stack[(Var,  (Type) => ArrayType)] = mutable.Stack()
    val prvReadParallelStack: mutable.Stack[(Var,  (Type) => ArrayType)] = mutable.Stack()


    def incDepthInf(f: FunDecl) = {
      f match {
        case _: MapWarp | _:MapGlb | _ : MapLcl | _: MapAtomLcl =>  lclOrGlbDepth+=1
        case _: MapWrg => wrgDepth+=1
        case _ =>
      }
    }

    def decDepthInf(f: FunDecl) = {
      f match {
        case _: MapWarp | _: MapGlb | _: MapLcl | _: MapAtomLcl => lclOrGlbDepth -= 1
        case _: MapWrg => wrgDepth -= 1
        case _ =>
      }
    }

    def pushToReadStacks(readLoopVar: Var, fc: FunCall) = {
      fc.f match {
        case _:ParallelPattern =>
          glbReadParallelStack.push((readLoopVar,getBuildArrayTypeFun(fc.t)))
          if (wrgDepth==numDim)
            lclReadParallelStack.push((readLoopVar,getBuildArrayTypeFun(fc.t)))
          if (lclOrGlbDepth==numDim)
            prvReadParallelStack.push((readLoopVar,getBuildArrayTypeFun(fc.t)))
        case _ =>
      }
    }

    def popFromReadStacks(f: FunDecl) = {
      f match {
        case _:ParallelPattern =>
          glbReadParallelStack.pop
          if (wrgDepth == numDim)
            lclReadParallelStack.pop
          if (lclOrGlbDepth == numDim)
            prvReadParallelStack.pop
        case _ =>
      }
    }

    def pushToWriteStacks(writeLoopVar: Var, fc: FunCall) = {
      glbStack.push((writeLoopVar,getBuildArrayTypeFun(fc.t)))
      if (wrgDepth==numDim)
        lclStack.push((writeLoopVar,getBuildArrayTypeFun(fc.t)))
      if (lclOrGlbDepth==numDim)
        prvStack.push((writeLoopVar,getBuildArrayTypeFun(fc.t)))

      fc.f match {
        case _:ParallelPattern =>
          glbWriteParallelStack.push((writeLoopVar,getBuildArrayTypeFun(fc.t)))
          if (wrgDepth==numDim)
            lclWriteParallelStack.push((writeLoopVar,getBuildArrayTypeFun(fc.t)))
          if (lclOrGlbDepth==numDim)
            prvWriteParallelStack.push((writeLoopVar,getBuildArrayTypeFun(fc.t)))
        case _ =>
      }
    }

    def popFromWriteStacks(f: FunDecl) = {
      glbStack.pop
      if (wrgDepth == numDim)
        lclStack.pop
      if (lclOrGlbDepth == numDim)
        prvStack.pop

      f match {
        case _:ParallelPattern =>
          glbWriteParallelStack.pop
          if (wrgDepth == numDim)
            lclWriteParallelStack.pop
          if (lclOrGlbDepth == numDim)
            prvWriteParallelStack.pop
        case _ =>
      }
    }

    def getBuildArrayTypeFun(t:Type) : (Type) => ArrayType = {
      t match {
        case at: ArrayType => at.replacedElemT
      }
    }

    def prePost(n: IRNode): (IRNode => Unit) = {

      n match {
        case fc: FunCall =>
          fc.f match {

            // patterns with loop iteration variable for output
            case _: AbstractMap | _:MapSeqSlide | _: FilterSeq =>

              // first update the length stacks if we are at the right depth (e.g. in a single workgroup or single thread)

              val writeLoopVar =
                fc.f match {
                  case am: AbstractMap => am.loopVar
                  case mss: MapSeqSlide => mss.loopVar
                  case f: FilterSeq => f.loopWrite
                }
              pushToWriteStacks(writeLoopVar, fc)

              val readLoopVar =
                fc.f match {
                  case am: AbstractMap => am.loopVar
                  case mss: MapSeqSlide => mss.loopVar
                  case f: FilterSeq => f.loopRead
                }
              pushToReadStacks(readLoopVar, fc)



              // then update the depth information
              incDepthInf(fc.f)

              fc.writeItVarsAndArrayFun = List(glbStack.toList, lclStack.toList, prvStack.toList)
              fc.writeItVarsAndArrayFunParallel = List(glbWriteParallelStack.toList, lclWriteParallelStack.toList, prvWriteParallelStack.toList)

              fc.readItVarsAndArrayFunParallel = List(glbReadParallelStack.toList, lclReadParallelStack.toList, prvReadParallelStack.toList)


              return (_:IRNode) => {
                // reverse everything we did before
                decDepthInf(fc.f)
                popFromWriteStacks(fc.f)
                popFromReadStacks(fc.f)
              }

            case _ =>
              fc.writeItVarsAndArrayFun = List(glbStack.toList, lclStack.toList, prvStack.toList)
              fc.writeItVarsAndArrayFunParallel = List(glbWriteParallelStack.toList, lclWriteParallelStack.toList, prvWriteParallelStack.toList)
              fc.readItVarsAndArrayFunParallel = List(glbReadParallelStack.toList, lclReadParallelStack.toList, prvReadParallelStack.toList)

          }

        case e:Expr =>
          e.writeItVarsAndArrayFun = List(glbStack.toList, lclStack.toList, prvStack.toList)
          e.writeItVarsAndArrayFunParallel = List(glbWriteParallelStack.toList, lclWriteParallelStack.toList, prvWriteParallelStack.toList)
          e.readItVarsAndArrayFunParallel = List(glbReadParallelStack.toList, lclReadParallelStack.toList, prvReadParallelStack.toList)

        case _ =>

      }

      (_:IRNode) =>


    }

    IRNode.visitPrePost(e, prePost)


  }

}
