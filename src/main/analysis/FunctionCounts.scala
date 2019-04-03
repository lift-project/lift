package analysis

import analysis.AccessCounts.SubstitutionMap
import lift.arithmetic.{?, ArithExpr}
import ir._
import ir.ast._
import opencl.generator.NDRange
import opencl.ir._
import opencl.ir.pattern._

object FunctionCounts{

  type SubstitutionMap = collection.immutable.Map[ArithExpr, ArithExpr]

  def apply(
    lambda: Lambda,
    localSize: NDRange = NDRange(?,?,?),
    globalSize: NDRange = NDRange(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new FunctionCounts(lambda, localSize, globalSize, valueMap)

}
class FunctionCounts (
  lambda: Lambda,
  localSize: NDRange,
  globalSize: NDRange,
  valueMap: SubstitutionMap
) extends Analyser(lambda, localSize, globalSize, valueMap) {

  private val addMultiplyPattern: PartialFunction[Expr, Any] =
  {
    case FunCall(uf1, FunCall(uf2, _, _), _) if uf1 == add  && uf2 == mult =>
    case FunCall(uf1, _, FunCall(uf2, _, _)) if uf1 == add  && uf2 == mult =>
  }

  private val vectorisedAddMultiplyPattern: PartialFunction[Expr, Any] =
  {
    case FunCall(VectorizeUserFun(_, uf1), FunCall(VectorizeUserFun(_, uf2), _, _), _)
      if uf1 == add  && uf2 == mult =>
    case FunCall(VectorizeUserFun(_, uf1), _, FunCall(VectorizeUserFun(_, uf2), _, _))
      if uf1 == add  && uf2 == mult =>
  }

  private val functionCounts =
    collection.mutable.Map[String, ArithExpr]().withDefaultValue(0)

  // TODO: Vector length
  private val vectorisedFunctionCounts =
    collection.mutable.Map[String, ArithExpr]().withDefaultValue(0)

  private var addMultCount: ArithExpr = 0
  private var vectorisedAddMultCount: ArithExpr = 0

  private var currentNesting: ArithExpr = 1

  count(lambda.body)

  def getFunctionCount(userFun: UserFun, exact: Boolean = false): ArithExpr =
    getFunctionCount(userFun.name, exact)

  def getVectorisedCount(userFun: UserFun, exact: Boolean = false): ArithExpr =
    getVectorisedCount(userFun.name, exact)

  def getFunctionCount(userFun: String, exact: Boolean): ArithExpr =
    getExact(functionCounts(userFun), exact)

  def getVectorisedCount(userFun: String, exact: Boolean): ArithExpr =
    getExact(vectorisedFunctionCounts(userFun), exact)

  def getTotalCount(userFun: UserFun, exact: Boolean = false): ArithExpr =
    getFunctionCount(userFun, exact) + getVectorisedCount(userFun, exact)

  def getFunctions: collection.Set[String] =
    functionCounts.keySet ++ vectorisedFunctionCounts.keySet

  def getAddMultCount(exact: Boolean = false): ArithExpr =
    getExact(addMultCount, exact)

  def getVectorisedAddMultCount(exact: Boolean = false): ArithExpr =
    getExact(vectorisedAddMultCount, exact)

  private def count(lambda: Lambda, arithExpr: ArithExpr): Unit = {
    currentNesting *= arithExpr
    count(lambda.body)
    currentNesting /^= arithExpr
  }

  private def count(expr: Expr): Unit = {

    if (addMultiplyPattern.isDefinedAt(expr))
      addMultCount += currentNesting

    if (vectorisedAddMultiplyPattern.isDefinedAt(expr))
      vectorisedAddMultCount += currentNesting

    expr match {
      case FunCall(f, args@_*) =>

        args.foreach(count)

        f match {
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

          case Iterate(n, nestedLambda, _, _, _) =>
            count(nestedLambda, n)

          case l: Lambda => count(l.body)
          case fp: FPattern => count(fp.f.body)
          case uf: UserFun =>
            functionCounts(uf.name) += currentNesting
          case vuf: VectorizeUserFun =>
            vectorisedFunctionCounts(vuf.userFun.name) += currentNesting
          case _ =>
        }


      case _ =>
    }

  }
}
