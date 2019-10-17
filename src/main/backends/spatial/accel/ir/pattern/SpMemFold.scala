package backends.spatial.accel.ir.pattern

import ir._
import ir.ast.{Expr, Lambda, Lambda1, Lambda2, fun}
import lift.arithmetic.{ArithExpr, Cst, PosVar}

case class SpMemFold(override val fMap: Lambda1,
                     override val fReduce: Lambda2,
                     override val chunkSize: ArithExpr,
                     override val stride: ArithExpr,
                     override val factor: ArithExpr)
  extends AbstractSpFold(fMap, fReduce, PosVar("i"), PosVar("j"), chunkSize, stride, factor) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case TupleType(initT, at@ArrayType(elemT)) =>
        // map input array element type
        fMap.params(0).t = ArrayType(elemT, chunkSize)

        val tiledMapBodyType = TypeChecker.check(fMap.body, setType) // check the body
        val mapBodyType = tiledMapBodyType match {
          case ArrayType(elemT) => elemT
          case t => throw new TypeException(t, "ArrayType(_, _)", this)
        }

        fFlatMapT = at.replacedElemT(mapBodyType)

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
  def apply(chunkSize: ArithExpr,
            stride: ArithExpr = Cst(1),
            factor: ArithExpr = Cst(1),
            fMap: Lambda, fReduce: Lambda2,
            init: Expr): Lambda1 =
    fun((x) => SpMemFold(fMap, fReduce, chunkSize, stride, factor)(init, x))
}

