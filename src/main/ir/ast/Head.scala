package ir.ast

import ir.interpreter.Interpreter._
import ir._

/**
 * Head pattern.
 * Code for this pattern can be generated.
 *
 * The head pattern has the following high-level semantics:
 * `Head()( [x,,1,,, ..., x,,n,,] ) = x,,1,,`
 *
 * The head pattern has the following type:
 * `Head() : [a],,I,, -> a`
 */
case class Head() extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t) => ArrayTypeWSWC(t, 1)

      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }


  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    args.head match {
      case a: Array[_] => a.head
    }
  }
}
