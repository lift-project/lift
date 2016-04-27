package analysis

import analysis.AccessCounts.SubstitutionMap

import apart.arithmetic.ArithExpr.{contains, substitute}
import apart.arithmetic.{?, ArithExpr, Cst, RangeAdd}
import ir._
import ir.ast._
import opencl.generator.OpenCLGenerator.NDRange
import opencl.generator._
import opencl.ir.OpenCLMemoryAllocator
import opencl.ir.pattern._
import rewriting.InferNDRange.substituteInNDRange

object BarrierCounts {
    def apply(
    lambda: Lambda,
    localSize: NDRange = Array(?,?,?),
    globalSize: NDRange = Array(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new BarrierCounts(lambda, localSize, globalSize, valueMap)

}

class BarrierCounts(
  val lambda: Lambda,
  val localSize: NDRange,
  val globalSize: NDRange,
  val valueMap: SubstitutionMap) {

  private val substLocal = substituteInNDRange(localSize, valueMap)
  private val substGlobal = substituteInNDRange(globalSize, valueMap)

  private val substitutionMap = collection.immutable.Map[ArithExpr, ArithExpr](
    new get_local_size(0) -> substLocal(0),
    new get_local_size(1) -> substLocal(1),
    new get_local_size(2) -> substLocal(2),
    new get_global_size(0) -> substGlobal(0),
    new get_global_size(1) -> substGlobal(1),
    new get_global_size(2) -> substGlobal(2),
    new get_num_groups(0) -> (substGlobal(0) / substLocal(0)),
    new get_num_groups(1) -> (substGlobal(1) / substLocal(1)),
    new get_num_groups(2) -> (substGlobal(2) / substLocal(2))
  ).filterNot(pair => contains(pair._2, ?)) ++ valueMap

  private var barrierCounts = collection.Map[MapLcl, ArithExpr]()

  private var currentNesting: ArithExpr = Cst(1)

  private lazy val totalCount =
    barrierCounts.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)

  // TODO: only do the required steps and only if not already done
  if (lambda.body.t == UndefType)
    TypeChecker(lambda)

  if (lambda.body.mem == UnallocatedMemory) {
    RangesAndCounts(lambda, localSize, globalSize, valueMap)
    OpenCLMemoryAllocator(lambda)
  }

  BarrierElimination(lambda)

  count(lambda.body)

  // TODO: different fences?

  def counts = barrierCounts

  def getTotalCount(exact: Boolean = false) =
    if (exact) substitute(totalCount, substitutionMap) else totalCount

  private def count(lambda: Lambda, arithExpr: ArithExpr): Unit = {
    currentNesting *= arithExpr
    count(lambda.body)
    currentNesting /^= arithExpr
  }

  private def getParallelMapTripCount(map: AbstractMap, t: Type) = {
    Type.getLength(t) /^ map.loopVar.range.asInstanceOf[RangeAdd].step
  }

  private def count(expr: Expr): Unit = {
    expr match {
      case FunCall(f, args@_*) =>

        args.foreach(count)

        f match {
          case mapLcl: MapLcl if mapLcl.emitBarrier =>
            barrierCounts += mapLcl -> currentNesting
            val n = getParallelMapTripCount(mapLcl, expr.t)
            count(mapLcl.f, n)

          case _: MapGlb | _:MapWrg | _: MapLcl =>
            val map = f.asInstanceOf[AbstractMap]

            val n = getParallelMapTripCount(map, expr.t)
            count(map.f, n)

          case map: AbstractMap =>
            val n = Type.getLength(expr.t)
            count(map.f, n)

          case reduce: AbstractPartRed =>
            val n = Type.getLength(args(1).t)
            count(reduce.f, n)

          case Iterate(n, nestedLambda) =>
            count(nestedLambda, n)

          case l: Lambda => count(l.body)
          case fp: FPattern => count(fp.f.body)
          case _ =>
        }

      case _ =>
    }

  }

}
