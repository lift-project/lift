package analysis

import analysis.AccessCounts.SubstitutionMap
import lift.arithmetic.{?, ArithExpr, Cst}
import ir._
import ir.ast._
import opencl.generator._
import opencl.ir.pattern._

object BarrierCounts {
    def apply(
    lambda: Lambda,
    localSize: NDRange = NDRange(?,?,?),
    globalSize: NDRange = NDRange(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new BarrierCounts(lambda, localSize, globalSize, valueMap)

}

class BarrierCounts(
  lambda: Lambda,
  localSize: NDRange,
  globalSize: NDRange,
  valueMap: SubstitutionMap
) extends Analyser(lambda, localSize, globalSize, valueMap) {

  private var barrierCounts = collection.Map[MapLcl, ArithExpr]()

  private var currentNesting: ArithExpr = Cst(1)

  private lazy val totalCount =
    barrierCounts.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)

  BarrierElimination(lambda)

  count(lambda.body)

  // TODO: different fences?

  def counts = barrierCounts

  def getTotalCount(exact: Boolean = false) =
    getExact(totalCount, exact)

  private def count(lambda: Lambda, arithExpr: ArithExpr): Unit = {
    currentNesting *= arithExpr
    count(lambda.body)
    currentNesting /^= arithExpr
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

          case _: MapGlb | _:MapWrg =>
            val map = f.asInstanceOf[AbstractMap]

            val n = getParallelMapTripCount(map, expr.t)
            count(map.f, n)

          case map: AbstractMap =>
            val n = Type.getLength(expr.t)
            count(map.f, n)

          case reduce: AbstractPartRed =>
            val n = Type.getLength(args(1).t)
            count(reduce.f, n)

          case Iterate(n, nestedLambda, _,  _, _) =>
            count(nestedLambda, n)

          case l: Lambda => count(l.body)
          case fp: FPattern => count(fp.f.body)
          case _ =>
        }

      case _ =>
    }

  }

}
