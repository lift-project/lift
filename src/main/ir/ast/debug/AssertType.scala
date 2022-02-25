package ir.ast.debug

import ir.Type
import lift.arithmetic.ArithExpr
import lift.arithmetic.simplifier.ExprSimplifier

/**
  * assertType checks if the given type satisfies one of the following conditions:
  * 1. If expectedType is defined, then the given type must equal at least one of the types in expectedType list.
  * 2. If expectedType is not defined, but expected Lengths is, that the given array type dimensions must equal
  *    those specified in respective dimensions.
  */
class AssertType(val expectedTypeAsORList: Option[List[Type]],
                 val expectedLengths: Option[List[(Int, ArithExpr)]],
                 val name: String) extends
  TypeOperator(actualType => {
    expectedTypeAsORList match {
      case Some(_) =>
        var typeMatched = false
        var i = 0
        val actualTypeSimplified = Type.visitAndRebuild(actualType, ae => ExprSimplifier(ae))
        val expectedTypeSimplified = expectedTypeAsORList.get.map(Type.visitAndRebuild(_, ae => ExprSimplifier(ae)))

        while (!typeMatched && i < expectedTypeAsORList.get.length) {
          if (expectedTypeSimplified(i).equals(actualTypeSimplified))
            typeMatched = true
          i += 1
        }
        if (!typeMatched) {
          System.err.println("AssertType for \"" + name + "\" failed.\nExpected type(s):\n" +
            expectedTypeSimplified.toString.replace("),", "),\n") +
            "\nActual type:\n" + actualTypeSimplified.toString.replace("),", "),\n"))
          throw new java.lang.AssertionError()
        }

      case None =>
        assert(expectedLengths.isDefined)
        val actualTypeLengths = Type.getLengths(actualType).reverse.tail.reverse

        expectedLengths.get.foreach {
          case (dim, expectedLength) =>
            try {
              assert(actualTypeLengths(dim) == expectedLength)
            } catch {
              case e: java.lang.AssertionError =>
                System.err.println("AssertType for \"" + name + "\" failed.\n" +
                  f"Expected length in dim $dim:\n$expectedLength\n" +
                  s"\nActual length:\n${actualTypeLengths(dim)}")
                throw e
            }
        }
    }
  }) {

  override def toString: String = "AssertType(" + (
    if (expectedTypeAsORList.isDefined) {
      if (expectedTypeAsORList.get.length == 1) expectedTypeAsORList.get.head.toString
      else expectedTypeAsORList.get.toString
    } else {
      assert(expectedLengths.isDefined)
      expectedLengths.get.toString()
    }
  ) + (if (!name.equals("")) (", \"" + name + "\"") else "") + ")"
}

object AssertType {
  def apply(expectedType: Type): AssertType = new AssertType(Some(List(expectedType)), None, "")
  def apply(expectedType: Type, name: String): AssertType = new AssertType(Some(List(expectedType)), None, name)
  def apply(expectedTypeAsORList: List[Type]): AssertType = new AssertType(Some(expectedTypeAsORList), None, "")
  def apply(expectedTypeAsORList: List[Type], name: String): AssertType = new AssertType(Some(expectedTypeAsORList), None, name)

  def apply(expectedDims: List[Int], expectedLengths: List[ArithExpr]): AssertType =
    new AssertType(None, Some(expectedDims.zip(expectedLengths)), "")
  def apply(expectedDims: List[Int], expectedLengths: List[ArithExpr], name: String): AssertType =
    new AssertType(None, Some(expectedDims.zip(expectedLengths)), name)

  def unapply(arg: AssertType): Option[(Option[List[Type]], Option[List[(Int, ArithExpr)]], String)] =
    Some((arg.expectedTypeAsORList, arg.expectedLengths, arg.name))
}
