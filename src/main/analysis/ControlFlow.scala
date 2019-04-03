package analysis

import analysis.AccessCounts.SubstitutionMap
import lift.arithmetic._
import ir._
import ir.ast._
import opencl.generator._
import opencl.ir.pattern._

object ControlFlow {
 def apply(
    lambda: Lambda,
    localSize: NDRange = NDRange(?,?,?),
    globalSize: NDRange = NDRange(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new ControlFlow(lambda, localSize, globalSize, valueMap)
}

class ControlFlow(
  lambda: Lambda,
  localSize: NDRange,
  globalSize: NDRange,
  valueMap: SubstitutionMap
) extends Analyser(lambda, localSize, globalSize, valueMap) {

  private var ifStatements: ArithExpr = Cst(0)
  private var forStatements: ArithExpr = Cst(0)
  private var forBranches : ArithExpr = Cst(0)
  private var currentNesting: ArithExpr = Cst(1)

  ShouldUnroll(lambda)

  count(lambda.body)

  def getIfStatements(exact: Boolean = false) =
    getExact(ifStatements, exact)

  def getForStatements(exact: Boolean = false) =
    getExact(forStatements, exact)

  def getForBranches(exact: Boolean = false) =
    getExact(forBranches, exact)

  private def count(
    lambda: Lambda,
    loopVar: Var,
    avgIterations: ArithExpr,
    unrolled: Boolean): Unit = {

    val range = loopVar.range.asInstanceOf[RangeAdd]
    // TODO: Information needed elsewhere. See OpenCLGenerator
    // try to see if we really need a loop
    range.numVals match {
      case Cst(0) => return
      case Cst(1) =>

      // TODO: See TestInject.injectExactlyOneIterationVariable
      // TODO: M / 128 is not equal to M /^ 128 even though they print to the same C code
      case _ if range.min.min == Cst(0) &&
        ArithExpr.substituteDiv(range.stop) == ArithExpr.substituteDiv(range.step) =>

      // TODO: See TestOclFunction.numValues and issue #62
      case _ if range.start.min.min == Cst(0) && range.stop == Cst(1) =>
        ifStatements += currentNesting
      case _ =>
        (range.numVals.min, range.numVals.max) match {

          case (Cst(0),Cst(1)) =>
            // one or less iteration
            ifStatements += currentNesting

          case _  if !unrolled =>

            forBranches += currentNesting * avgIterations
            forStatements += currentNesting
          case _ =>
        }
    }

    currentNesting *= avgIterations
    count(lambda.body)
    currentNesting /^= avgIterations
  }

  private def count(expr: Expr): Unit = {
    expr match {
      case FunCall(f, args@_*) =>

        args.foreach(count)

        f match {
          case _: MapGlb | _: MapWrg =>
            val map = f.asInstanceOf[AbstractMap]
            val step = map.loopVar.range.asInstanceOf[RangeAdd].step

            val n = Type.getLength(expr.t) /^ step

            count(map.f, map.loopVar, n, unrolled = false)

          case map: MapLcl =>
            val step = map.loopVar.range.asInstanceOf[RangeAdd].step

            val n = Type.getLength(expr.t) /^ step
            val unrolled = map.shouldUnroll

            count(map.f, map.loopVar, n, unrolled)

          case mapSeq: MapSeq =>
            val n = Type.getLength(expr.t)
            count(mapSeq.f, mapSeq.loopVar, n, mapSeq.shouldUnroll)

          case reduceSeq: ReduceSeq =>
            val n = Type.getLength(args(1).t)
            count(reduceSeq.f, reduceSeq.loopVar, n, reduceSeq.shouldUnroll)

          case iterate@Iterate(n, nestedLambda, _, _, _) =>
            count(nestedLambda, iterate.indexVar, n, unrolled = false)

          case l: Lambda => count(l.body)
          case fp: FPattern => count(fp.f.body)

          case _ =>
        }
      case _ =>
    }
  }

}
