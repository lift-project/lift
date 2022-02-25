package rewriting.macrorules

import core.generator.GenericAST.VarRef
import ir.Type.{getLength, getLengths}
import ir.{GenerateIR, Type, TypeChecker, UnallocatedMemory, UndefType}
import ir.ast.{AbstractMap, Expr, FunCall, Gather, Join, Lambda, Param, ReorderWithStride, fun}
import ir.view.{BuildDepthInfo, InputView, NoView, View, ViewPrinter}
import lift.arithmetic.{?, ArithExpr, Cst}
import lift.arithmetic.ArithExpr._
import opencl.generator.{NDRange, RangesAndCounts}
import opencl.ir.{InferOpenCLAddressSpace, UndefAddressSpace}
import rewriting.rules.Rule

object CoalescingRules {

  // TODO: add gather
  // TODO: restrict to innermost MapLcl / MapGlb
//  def coalesceAndScatter(checkIterations: Int) = {
//    assert(checkIterations >= 2)
//
//    Rule("Map(f) => Split(..) o Scatter(..) o Join(..) o Map(f) o " +
//      "Split(..) o Gather(..) o Join(..)", {
//      case FunCall(m: AbstractMap, arg) =>
//
//        assert(arg.t != UndefType)
//
//        def inferInputViews(expr: Expr): Unit = {
//          val lambdaWrapper = Lambda(Array(Param()), expr)
//
//          TypeChecker(expr)
//          InferOpenCLAddressSpace(lambdaWrapper)
//          RangesAndCounts(lambdaWrapper, NDRange(?,?,?), NDRange(?,?,?), Predef.Map())
//
//          BuildDepthInfo(expr)
//          InputView(expr)
//
//          assert(expr.addressSpace != UndefAddressSpace)
//          assert(expr.mem != UnallocatedMemory)
//          assert(expr.view != NoView)
//        }
//
//        inferInputViews(arg)
//
//        val argUntangling = GenerateIR.untangleZippedArrays(arg.t)
//
//        val argUntanglingWithReorderingStrides =
//          argUntangling.arrayGetters.zip(argUntangling.arrayTypes).map {
//            case (argArrGetter: Lambda, argArrType: Type) =>
//
//              val subArg = argArrGetter $ arg
//              inferInputViews(subArg)
//
//              assert(subArg.t == argArrType) // Sanity check
//
//              val genericIdx = ViewPrinter.emit(subArg.view) match {
//                case VarRef(_, _, Some(arithExpression)) => arithExpression.content
//                case _ => throw new IllegalStateException()
//              }
//              val iterationSpecificIndices = (0 until checkIterations).map(i =>
//                ArithExpr.substitute(genericIdx, Predef.Map(m.loopVar -> i)))
//
//              val distancesBetweenIterationsAreEqual =
//                if (checkIterations <= 2) true
//                else {
//                  val firstDistance = iterationSpecificIndices(1) - iterationSpecificIndices.head
//
//                  iterationSpecificIndices.zip(iterationSpecificIndices.tail).tail.forall {
//                    case (idx1, idx2) => idx2 - idx1 == firstDistance
//                  }
//                }
//
//              if (!distancesBetweenIterationsAreEqual)
//                throw new IllegalArgumentException(s"Cannot coalesce $m since in the first $checkIterations " +
//                  s"iterations, the distances between iteration accesses of ${subArg.mem} are not equal")
//
//              val distanceBetweenNIterations = iterationSpecificIndices(1) - iterationSpecificIndices.head
//
//              if (distanceBetweenNIterations == Cst(1))
//                throw new IllegalArgumentException(s"Cannot coalesce $m since in the first $checkIterations " +
//                  s"iterations, the distance between iteration accesses of ${subArg.mem} is already 1")
//
//              (argArrGetter, argArrType, distanceBetweenNIterations)
//          }
//
//        val (coalescedSubArgs, decoalescing) =
//            argUntanglingWithReorderingStrides.map {
//              case (argArrGetter: Lambda, argArrType: Type, reorderingStride: ArithExpr) =>
//
//                val subArgDims = getLengths(argArrType).length - 1
//
//                val flatten = if (subArgDims <= 1) fun(p => p)
//                else GenerateIR.applyNTimes(Join(), subArgDims)
//
//                (Gather(ReorderWithStride(reorderingStride)) o flatten o argArrGetter $ arg,
//                  ????)
//            }
//
//      argUntangling.retanglingLambda.apply()
//    })
//  }
}
