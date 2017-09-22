package ir.ast

import lift.arithmetic.{ArithExpr, Cst}
import ir.interpreter.Interpreter.ValueMap
import ir._

/**
 * Tail pattern.
 * Code for this pattern can be generated.
 *
 * The tail pattern has the following high-level semantics:
 * `Tail()( [x,,1,,, x,,2,,, ..., x,,n,,] ) = [x,,2,,, ..., x,,n,,]`
 *
 * The tail pattern has the following type:
 * `Tail() : [a],,I+1,, -> [a],,I,,`
 */
case class Tail() extends Pattern(arity = 1) with isGenerable {
  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    /**
     * Subtract 1 from an expression and throw an error if we can prove that
     * the result is not positive.
     */
    def minusOne(e: ArithExpr): ArithExpr = {
      if (e == Cst(1)) throw new TypeException(
        argType, "ArrayType with size/capacity > 1 (if known)", this
      )
      e - 1
    }

    argType match {
      case at: ArrayType => at match {
        case sc: Size with Capacity => ArrayTypeWSWC(at.elemT, minusOne(sc.size), minusOne(sc.capacity))
        case c: Capacity => ArrayTypeWC(at.elemT, minusOne(c.capacity))
        case s: Size => ArrayTypeWS(at.elemT, minusOne(s.size))
        case _ => at // At your own risk…
      }
      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    args.head match {
      case a: Array[_] => a.tail
    }
  }
}
