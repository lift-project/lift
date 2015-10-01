package exploration

import apart.arithmetic.{Cst, ArithExpr}
import ir._
import ir.ast.{Lambda, Expr, FunCall}
import opencl.executor.Execute
import opencl.ir.pattern.{MapWrg, MapLcl, MapGlb}
import opencl.generator.OpenCLGenerator.NDRange


object InferNDRange {
  def apply(lambda: Lambda): (NDRange, NDRange) = (new InferNDRange)(lambda)

  def apply(lambda: Lambda, values: Any*): (NDRange, NDRange) = {
    val nDRanges = apply(lambda)

    val valueMap = Execute.createValueMap(lambda, values:_*)

    (substituteInNDRange(nDRanges._1, valueMap), substituteInNDRange(nDRanges._2, valueMap))
  }

  def substituteInNDRange(nDRange: NDRange, valueMap: Map[ArithExpr, ArithExpr]): NDRange =
    Array(
      ArithExpr.substitute(nDRange(0), valueMap),
      ArithExpr.substitute(nDRange(1), valueMap),
      ArithExpr.substitute(nDRange(2), valueMap)
    )
}

class InferNDRange {

  private var mapGlb = Map[Int, ArithExpr]()
  private var mapLcl = Map[Int, ArithExpr]()
  private var mapWrg = Map[Int, ArithExpr]()

  def apply(lambda: Lambda): (NDRange, NDRange) = {
    TypeChecker.check(lambda.body)

    var mapGlobals = List[(Int, ArithExpr)]()
    var mapLocals = List[(Int, ArithExpr)]()
    var mapWorkGroups = List[(Int, ArithExpr)]()

    Expr.visit(lambda.body,
    {
      case FunCall(MapGlb(dim, _), arg) => mapGlobals = (dim, Type.getLength(arg.t)) +: mapGlobals
      case FunCall(MapLcl(dim, _), arg) => mapLocals = (dim, Type.getLength(arg.t)) +: mapLocals
      case FunCall(MapWrg(dim, _), arg) => mapWorkGroups = (dim, Type.getLength(arg.t)) +: mapWorkGroups
      case _ =>
    }, (_) => Unit)

    mapGlb = getMostCommonLengthsForDimensions(mapGlobals)
    mapLcl = getMostCommonLengthsForDimensions(mapLocals)
    mapWrg = getMostCommonLengthsForDimensions(mapWorkGroups)

    val sizes0 = getSizesForDimension(0)
    val sizes1 = getSizesForDimension(1)
    val sizes2 = getSizesForDimension(2)

    (Array(sizes0._1, sizes1._1 , sizes2._1), Array(sizes0._2, sizes1._2 , sizes2._2))
  }

  private def getSizesForDimension(dim: Int): (ArithExpr, ArithExpr) = {
    if (mapGlb.isDefinedAt(dim)) {
      (Cst(32), mapGlb(dim))
    } else if (mapLcl.isDefinedAt(dim)) {
      (mapLcl(dim), mapWrg(dim) * mapLcl(dim))
    } else {
      (Cst(1), Cst(1))
    }
  }

  private def getMostCommonLengthsForDimensions(mapGlobals: List[(Int, ArithExpr)]): Map[Int, ArithExpr] = {
    mapGlobals.groupBy(_._1).mapValues(pairs => {
      val lengths = pairs.map(_._2)
      getMostCommonElement(lengths)
    })
  }

  private def getMostCommonElement(lengths: List[ArithExpr]): ArithExpr = {
    val distinct = lengths.distinct
    val idOfMostCommon = distinct.map(length => lengths.count(_ == length)).zipWithIndex.maxBy(_._1)._2
    distinct(idOfMostCommon)
  }
}
