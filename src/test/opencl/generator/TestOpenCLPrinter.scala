package opencl.generator

import generic.ast.AstPrinter
import lift.arithmetic.Var
import generic.ast.GenericAST._
import org.junit.Test
import org.junit.Assert._

class TestOpenCLPrinter {

  @Test
  def printCondExpression(): Unit = {
    val lhs = VarRef(Var("x"))
    val rhs = VarRef(Var("y"))
    val cond = BinaryExpression(lhs, BinaryExpressionT.Operator.==, rhs)
    assertEquals("(" + lhs.v.toString + " == " + rhs.v.toString + ")",
      AstPrinter.apply(cond))
  }

  @Test
  def printBinExpression(): Unit = {
    val lhs = VarRef(Var("x"))
    val rhs = VarRef(Var("y"))
    val binop = BinaryExpression(lhs, BinaryExpressionT.Operator.+, rhs)
    assertEquals("(" + lhs.v.toString + " + " + rhs.v.toString + ")",
      AstPrinter.apply(binop))
  }

}
