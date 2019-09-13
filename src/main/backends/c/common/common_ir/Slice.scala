package backends.c.common.common_ir

import ir._
import ir.ast.Pattern
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.ArithExpr



// implement a view construct that perform this: res = [startIdx, endIdx) of the outmost dimension
case class Slice(startIdx: ArithExpr, endIdx: ArithExpr) extends Pattern(arity = 1) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case at: ArrayType with Size with Capacity =>
        val chunkSize = endIdx - startIdx
        ArrayTypeWSWC(at.elemT, chunkSize, chunkSize)

      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }


  override def eval(valueMap: ValueMap, args: Any*): Vector[Vector[_]] = {
    assert(args.length == arity)

    args.head match {
      case v: Vector[_] =>
        val chunkSize = endIdx - startIdx
        v.grouped(chunkSize.eval).toVector
    }
  }
}
