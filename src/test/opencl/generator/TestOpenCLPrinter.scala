package opencl.generator

import lift.arithmetic.Var
import opencl.generator.OpenCLAST.{BinaryExpression, VarRef}
import org.junit.Test
import org.junit.Assert._

class TestOpenCLPrinter {

  @Test
  def printCondExpression(): Unit = {
    val lhs = VarRef(Var("x"))
    val rhs = VarRef(Var("y"))
    val cond = BinaryExpression(lhs, BinaryExpression.Operator.==, rhs)
    assertEquals("(" + lhs.v.toString + " == " + rhs.v.toString + ")", OpenCLPrinter()(cond))
  }

  @Test
  def printBinExpression(): Unit = {
    val lhs = VarRef(Var("x"))
    val rhs = VarRef(Var("y"))
    val binop = BinaryExpression(lhs, BinaryExpression.Operator.+, rhs)
    assertEquals("(" + lhs.v.toString + " + " + rhs.v.toString + ")", OpenCLPrinter()(binop))
  }

}
