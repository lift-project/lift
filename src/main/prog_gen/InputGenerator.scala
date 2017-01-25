package prog_gen

import ir._
import ir.ast.{Lambda, Param}
import lift.arithmetic.{ArithExpr, Cst}
import opencl.ir._

object InputGenerator {

  def apply(concreteSizes: collection.Map[ArithExpr, Cst] = collection.Map()) =
    new InputGenerator(concreteSizes)
}

class InputGenerator(val concreteSizes: collection.Map[ArithExpr, Cst]) {

  private def getFloat = util.Random.nextFloat() * 10

  def apply(t: Type): Any = {

    t match {
      case ArrayType(ArrayType(ArrayType(ArrayType(Float, len4), len3), len2), len1) =>
        val actual1 = ArithExpr.substitute(len1, concreteSizes).eval
        val actual2 = ArithExpr.substitute(len2, concreteSizes).eval
        val actual3 = ArithExpr.substitute(len3, concreteSizes).eval
        val actual4 = ArithExpr.substitute(len4, concreteSizes).eval

        Array.fill(actual1, actual2, actual3, actual4)(getFloat)

      case ArrayType(ArrayType(ArrayType(Float, len3), len2), len1) =>
        val actual1 = ArithExpr.substitute(len1, concreteSizes).eval
        val actual2 = ArithExpr.substitute(len2, concreteSizes).eval
        val actual3 = ArithExpr.substitute(len3, concreteSizes).eval

        Array.fill(actual1, actual2, actual3)(getFloat)

      case ArrayType(ArrayType(Float, len2), len1) =>
        val actual1 = ArithExpr.substitute(len1, concreteSizes).eval
        val actual2 = ArithExpr.substitute(len2, concreteSizes).eval

        Array.fill(actual1, actual2)(getFloat)

      case ArrayType(Float, len1) =>
        val actual1 = ArithExpr.substitute(len1, concreteSizes).eval

        Array.fill(actual1)(getFloat)

      case Float => getFloat
    }

  }

  def apply(param: Param): Any = apply(param.t)

  def apply(lambda: Lambda): Seq[Any] =
    lambda.params.map(apply)

}
