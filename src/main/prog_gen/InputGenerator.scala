package prog_gen

import ir._
import ir.ast.{Lambda, Param}
import lift.arithmetic.{ArithExpr, Cst}
import opencl.ir._

object InputGenerator {

  def apply(concreteSizes: collection.Map[ArithExpr , Cst] = collection.Map()) =
    new InputGenerator(concreteSizes)
}

class InputGenerator(val concreteSizes: collection.Map[ArithExpr, Cst]) {

  private def getFloat = util.Random.nextFloat() * 10

  def apply(t: Type): Any = {

    t match {
      case ArrayType(ArrayType(ArrayType(ArrayType(Float, len4), len3), len2), len1) =>
        val actual1 = evaluate(len1)
        val actual2 = evaluate(len2)
        val actual3 = evaluate(len3)
        val actual4 = evaluate(len4)

        Array.fill(actual1, actual2, actual3, actual4)(getFloat)

      case ArrayType(ArrayType(ArrayType(Float, len3), len2), len1) =>
        val actual1 = evaluate(len1)
        val actual2 = evaluate(len2)
        val actual3 = evaluate(len3)

        Array.fill(actual1, actual2, actual3)(getFloat)

      case ArrayType(ArrayType(Float, len2), len1) =>
        val actual1 = evaluate(len1)
        val actual2 = evaluate(len2)

        Array.fill(actual1, actual2)(getFloat)

      case ArrayType(Float, len1) =>
        val actual1 = evaluate(len1)

        Array.fill(actual1)(getFloat)

      case Float => getFloat

      case _ => throw new NotImplementedError()
    }

  }

  private def evaluate(len1: ArithExpr) =
    ArithExpr.substitute(len1, concreteSizes).eval

  def apply(param: Param): Any = apply(param.t)

  def apply(lambda: Lambda): Seq[Any] =
    lambda.params.map(apply)

}
