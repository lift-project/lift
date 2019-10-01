package backends.spatial.accel.ir.pattern

import ir.ast.{Expr, IRNode, Lambda, Lambda1, Lambda2, Pattern, fun}
import ir.interpreter.Interpreter.ValueMap
import ir._
import lift.arithmetic.{ArithExpr, PosVar, SimplifiedExpr, Var}

case class SpMemFold(override val fMap: Lambda,
                  override val fReduce: Lambda,
                  override val iterSize: ArithExpr,
                  override val stride: Option[ArithExpr],
                  override val factor: Option[ArithExpr])
  extends AbstractSpFold(fMap, fReduce, PosVar("i"), iterSize, stride, factor) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case TupleType(initT, at@ArrayType(elemT)) =>
        fMap.params(0).t = ArrayType(elemT, iterSize) // map input array element type

        val mapBodyType = TypeChecker.check(fMap.body, setType) // check the body

        fReduce.params(0).t = initT // initial element type
        fReduce.params(1).t = mapBodyType // reduce input array element type

        val reduceBodyType = TypeChecker.check(fReduce.body, setType) // check the body

        if (initT != mapBodyType || initT != reduceBodyType)
          throw TypeException(
            s"Illegal customising function in:\n``$this``.\n" +
              s"``($initT, $mapBodyType) -> $reduceBodyType`` does not match ``(α, α) -> α``"
          )

        ArrayTypeWSWC(initT, 1)

      case _ => throw new TypeException(argType, "TupleType(_, ArrayType(_, _))", this)
    }
  }
}

object SpMemFold {
  def apply(iterSize: ArithExpr,
            stride: ArithExpr,
            factor: ArithExpr,
            fMap: Lambda, fReduce: Lambda2,
            init: Expr): Lambda1 =
    fun((x) => SpMemFold(fMap, fReduce, iterSize, Some(stride), Some(factor))(init, x))

  def apply(iterSize: ArithExpr,
            stride: Option[ArithExpr] = None,
            factor: Option[ArithExpr] = None,
            fMap: Lambda, fReduce: Lambda2,
            init: Expr): Lambda1 =
    fun((x) => SpMemFold(fMap, fReduce, iterSize, stride, factor)(init, x))
}

