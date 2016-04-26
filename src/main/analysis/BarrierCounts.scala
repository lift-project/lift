package analysis

import apart.arithmetic.{ArithExpr, Cst, RangeAdd}
import ir._
import ir.ast._
import opencl.executor.Compile
import opencl.ir.pattern._

object BarrierCounts {
  def apply(lambda: Lambda) =
    new BarrierCounts(lambda)

}

class BarrierCounts(val lambda: Lambda) {

  // TODO: only do the required steps and only if not already done
  Compile(lambda)
  count(lambda.body)

  // TODO: different fences?

  lazy val totalCount = barrierCounts.foldLeft(Cst(0): ArithExpr)((acc, curr) => acc + curr._2)

  private var barrierCounts = collection.Map[MapLcl, ArithExpr]()

  private var currentNesting: ArithExpr = Cst(1)

  def counts = barrierCounts

  private def count(lambda: Lambda, arithExpr: ArithExpr): Unit = {
    currentNesting *= arithExpr
    count(lambda.body)
    currentNesting /= arithExpr
  }

  private def getParallelMapTripCount(map: AbstractMap, t: Type) = {
    Type.getLength(t) / map.loopVar.range.asInstanceOf[RangeAdd].step
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
