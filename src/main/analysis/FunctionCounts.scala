package analysis

import analysis.AccessCounts.SubstitutionMap
import apart.arithmetic.{?, ArithExpr, Cst}
import ir._
import ir.ast.{AbstractMap, AbstractPartRed, Expr, FPattern, FunCall, Iterate, Lambda, UserFun, VectorizeUserFun}
import opencl.generator.OpenCLGenerator.NDRange
import opencl.ir.pattern.{MapGlb, MapLcl, MapWrg}

object FunctionCounts{

  type SubstitutionMap = collection.immutable.Map[ArithExpr, ArithExpr]

  def apply(
    lambda: Lambda,
    localSize: NDRange = Array(?,?,?),
    globalSize: NDRange = Array(?,?,?),
    valueMap: SubstitutionMap = collection.immutable.Map()
  ) = new FunctionCounts(lambda, localSize, globalSize, valueMap)

}
class FunctionCounts (
  lambda: Lambda,
  localSize: NDRange,
  globalSize: NDRange,
  valueMap: SubstitutionMap
) extends Analyser(lambda, localSize, globalSize, valueMap) {

  private val functionCounts =
    collection.mutable.Map[UserFun, ArithExpr]().withDefaultValue(Cst(0))

  // TODO: Vector length
  private val vectorisedFunctionCounts =
    collection.mutable.Map[UserFun, ArithExpr]().withDefaultValue(Cst(0))

  private var currentNesting: ArithExpr = Cst(1)

  count(lambda.body)

  def getFunctionCount(userFun: UserFun) = functionCounts(userFun)

  def getVectorisedCount(userFun: UserFun) = vectorisedFunctionCounts(userFun)

  def getTotalCount(userFun: UserFun) =
    getFunctionCount(userFun) + getVectorisedCount(userFun)

  def getFunctions = functionCounts.keySet ++ vectorisedFunctionCounts.keySet

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
          case uf: UserFun =>
            functionCounts(uf) += currentNesting
          case vuf: VectorizeUserFun =>
            vectorisedFunctionCounts(vuf.userFun) += currentNesting
          case _ =>
        }

      case _ =>
    }

  }
}
