package cbackends.common.common_ir

import ir._
import ir.ast.Pattern
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.ArithExpr


case class Slice(startIdx: ArithExpr, endIdx: ArithExpr) extends Pattern(arity = 1) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case at: ArrayType with Size with Capacity =>
        ArrayTypeWSWC(ArrayTypeWSWC(at.elemT, chunkSize, chunkSize), at.size /^ chunkSize, at.capacity /^ chunkSize)

      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }


  override def eval(valueMap: ValueMap, args: Any*): Vector[Vector[_]] = {
    assert(args.length == arity)

    args.head match {
      case v: Vector[_] => v.grouped(chunkSize.eval).toVector
    }
  }
}
