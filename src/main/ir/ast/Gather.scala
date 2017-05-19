package ir.ast

import ir.interpreter.Interpreter._
import ir.{ArrayType, Type, TypeException, UndefType}

/**
 * Gather pattern. Performs a reorder on the next read.
 * Code for this pattern can be generated.
 *
 * The gather pattern has the following high-level semantics:
 * `Gather(idx)( [x,,1,,, ..., x,,n,,] ) = [ y,,1,,, ..., y,,n,,], where y,,i,, = x,,idx(i),,`
 *
 * The gather pattern has the following type:
 * `Gather(IndexFunction) : [a],,I,, -> [a],,I,,`
 *
 * @param idx The function to use for reordering
 */
case class Gather(idx: IndexFunction) extends Pattern(arity = 1)
                                      with isGenerable {

  override def checkType(argType: Type, setType: Boolean): Type = {
    // Gather expects an array
    argType match {
      case ArrayType(_) => argType
      case _ => throw new TypeException(argType, "ArrayType(_)", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)

    args.head match {
      case vec: Vector[_] =>
        (0 to vec.length).map(i => vec(idx.f(i, Type.fromAny(vec)).eval)).toVector
    }
  }
}

