package backends.spatial.accel.ir.pattern

import ir._
import ir.interpreter.Interpreter._
import ir.ast.Join

import scala.collection.immutable

/**
 * JoinW pattern. Performs the join on the previous write, changing the type of the target memory.
 * Code for this pattern can be generated.
 *
 * The joinW pattern has the following high-level semantics:
 *   `JoinW()( [ [x,,1,,, ..., x,,n,,], ..., [x,,m-n+1,,, ..., x,,m,,] ] ) =
 *    [x,,1,,, ..., x,,m,,]`
 *
 * The joinW pattern has the following type:
 *   `JoinW() : [ [a],,i,, ],,j,, -> [ a ],,i x j,,`
 */
case class JoinW() extends Join {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(ArrayTypeWSWC(et,is,ic), os, oc) => ArrayTypeWSWC(et, is*os, ic*oc)
      case _ => throw new TypeException(argType, "ArrayType(ArrayType(_, _), _)", this)
    }
  }



  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case v: Vector[Vector[_] @unchecked] => v.flatten
    }
  }
}
