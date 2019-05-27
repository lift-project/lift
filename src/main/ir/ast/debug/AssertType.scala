package ir.ast.debug

import ir.Type
import lift.arithmetic.simplifier.ExprSimplifier

/**
  * assertType checks if the given type is the same as the type of its parameter.
  */
case class AssertType(expectedType: Type, name: String = "") extends
  TypeOperator(actualType => {

    val expectedTypeSimplified = Type.visitAndRebuild(expectedType, ae => ExprSimplifier(ae))
    val actualTypeSimplified = Type.visitAndRebuild(actualType, ae => ExprSimplifier(ae))

    try {
      assert(expectedTypeSimplified.equals(actualTypeSimplified))
    } catch {
      case e: java.lang.AssertionError =>
        System.err.println("AssertType for \"" + name + "\" failed.\nExpected type:\n" +
          expectedTypeSimplified.toString.replace("),", "),\n") +
          "\nActual type:\n" + actualTypeSimplified.toString.replace("),", "),\n"))
        throw e
    }
  })
