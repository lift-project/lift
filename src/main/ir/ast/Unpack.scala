package ir.ast

import apart.arithmetic.Cst
import ir.TypeException
import ir.ArrayType
import ir.Type

/**
 * Unpack pattern.
 * Code for this pattern can be generated.
 *
 * The unpack pattern has the following high-level semantics:
 * `Unpack()( [x,,1,,] ) = x,,1,,`
 *
 * The head pattern has the following type:
 * `Unpack() : [a],,I,, -> a`
 */
case class Unpack() extends Pattern(arity = 1) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, Cst(1)) => t

      case _ => throw new TypeException(argType, "ArrayType")
    }
  }

}




