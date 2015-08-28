package ir.ast

import ir.{TypeException, ArrayType, Type}

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
      case ArrayType(t, _) => ArrayType(t, 1)

      case _ => throw new TypeException(argType, "ArrayType")
    }
  }

}
