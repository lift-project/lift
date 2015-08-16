package ir.ast

import apart.arithmetic.ArithExpr
import ir.interpreter.Interpreter._
import ir.{TypeException, ArrayType, Type}

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
case class Split(chunkSize: ArithExpr) extends Pattern(arity = 1)
  with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, n) =>
        ArrayType(ArrayType(t, chunkSize), n /^ chunkSize)

      case _ => throw new TypeException(argType, "ArrayType")
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)

    def split(n: Int, a: Seq[_]): Seq[Seq[_]] = {
      val (firstChunk, rest) = a.splitAt(n)
      if (rest.isEmpty) {
        Array(firstChunk)
      } else {
        firstChunk +: split(n, rest)
      }
    }

    args.head match {
      case a: Seq[_] => split(chunkSize.eval, a)
    }
  }
}
