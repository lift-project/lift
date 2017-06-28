package ir.ast

import ir.interpreter.Interpreter._
import ir._

/**
 * TransposeW pattern. Performs the transpose on the previous write.
 * Code for this pattern can be generated.
 *
 * Equivalent to Split(N) o Scatter(IndexFunction.transposeFunction(N, M)) o Join() when applied to type
 * ArrayType(ArrayType( ..., M), N) but infers N and M automatically during view generation.
 *
 * The transpose pattern has the following high-level semantics:
 * `TransposeW()([ [x,,1,,, ..., x,,n,,], [y,,1,,, ..., y,,n,,], ..., [z,,1,,, ..., z,,n,,] ]) = [ [x,,1,,, y,,1,,, ..., z,,1,,], [x,,2,,, y,,2,,, ..., z,,2,,], ..., [x,,n,,, y,,n,, ..., z,,n,,,] ]`
 *
 * The transpose pattern has the following type:
 * `TransposeW() : [ [a],,I,, ],,J,, -> [ [a],,J,, ],,I,,`
 */
case class TransposeW() extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(ArrayTypeWSWC(t, ns,nc), ms,mc) => ArrayTypeWSWC(ArrayTypeWSWC(t, ms,mc), ns,nc)
      case _ => throw new TypeException(argType, "ArrayType(ArrayType(_,_),_)", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[Vector[_]] = {
    assert(args.length == arity)
    args.head match {
      case vec: Vector[Vector[_] @unchecked] => vec.transpose
    }
  }

}
