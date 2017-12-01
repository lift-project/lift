package opencl.generator

import lift.arithmetic.Var
import opencl.generator.OpenCLAST.{BinaryExpression, CondExpression, VarRef}
import org.junit.Test
import org.junit.Assert._

class TestOpenCLPrinter {

  @Test
  def printCondExpression() = {
    val lhs = VarRef(Var("x"))
    val rhs = VarRef(Var("y"))
    val cond = CondExpression(lhs, rhs, CondExpression.Operator.==)
    assertEquals(lhs.v.toString + " == " + rhs.v.toString, OpenCLPrinter()(cond))
  }

  @Test
  def printBinExpression() = {
    val lhs = VarRef(Var("x"))
    val rhs = VarRef(Var("y"))
    val binop = BinaryExpression(lhs, rhs, BinaryExpression.Operator.+)
    assertEquals(lhs.v.toString + " + " + rhs.v.toString, OpenCLPrinter()(binop))
  }

}
