package backends.spatial.accel.ir.pattern

import ir._
import ir.ast.{Lambda1, Lambda2}
import lift.arithmetic.{ArithExpr}

abstract class SpFold(override val fMap: Lambda1,
                      override val fReduce: Lambda2,
                      override val chunkSize: ArithExpr,
                      override val stride: ArithExpr,
                      override val factor: ArithExpr)
  extends AbstractSpFold(fMap, fReduce, chunkSize, stride, factor) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case TupleType(_: ScalarType, ArrayTypeWS(_, _)) => super.checkType(argType, setType)
      case _ => throw new TypeException(argType, "TupleType(ScalarType, ArrayType(_, _))", this)
    }
  }
}