package ir.ast

import lift.arithmetic.ArithExpr
import ir.interpreter.Interpreter._
import ir._

/**
 * Split pattern.
 * Code for this pattern can be generated.
 *
 * The split pattern has the following high-level semantics:
 *   `Split(n)( [x,,1,,, ..., x,,m,,] ) =
 *     [ [x,,1,,, ..., x,,n,,], ..., [x,,m-n+1,,, ..., x,,m,,] ]`
 *
 * The split pattern has the following type:
 *   `Split(n) : [a],,n x j,, -> [ [a],,n,, ],,j,,`
 *
 * We know the following algorithmic rewrite rules for the split pattern
 * (so far):
 *  - `Join() o Split(chunkSize) | Split(chunkSize) o Join() => id`
 *
 * @param chunkSize The size of chunks the input array should be split in.
 *                  The size of the input array must be a multiple of
 *                  `chunkSize`.
 */
case class Split(chunkSize: ArithExpr) extends Pattern(arity = 1) {

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

object SplitND {
  // Split(s0, s1, s2) => Split(s0) o Map(Split(s1)) o Map(Map(Split(s2)))
  def apply(chunkSizes: ArithExpr*): FunDecl = {
    assert(chunkSizes.nonEmpty)

    chunkSizes.reverse match {
      case Nil => throw new IllegalArgumentException()
      case last :: Nil =>
        Split(last)
      case last :: remaining =>
        SplitND(remaining.reverse: _*) o GenerateIR.wrapInMaps(Split(last), remaining.length)
    }
  }
}
