package opencl.generator

import core.generator.AstPrinter
import lift.arithmetic.Var
import core.generator.GenericAST._
import org.junit.Test
import org.junit.Assert._

class TestOpenCLPrinter {

  @Test
  def printCondExpression(): Unit = {
    val lhs = VarRef(Var("x"))
    val rhs = VarRef(Var("y"))
    val cond = BinaryExpression(lhs, BinaryExpressionT.Operator.==, rhs)
    assertEquals("(" + lhs.v.v.toString + " == " + rhs.v.v.toString + ")",
      AstPrinter(cond)())
  }

  @Test
  def printBinExpression(): Unit = {
    val lhs = VarRef(Var("x"))
    val rhs = VarRef(Var("y"))
    val binop = BinaryExpression(lhs, BinaryExpressionT.Operator.+, rhs)
    assertEquals("(" + lhs.v.v.toString + " + " + rhs.v.v.toString + ")",
      AstPrinter(binop)())
  }

}
