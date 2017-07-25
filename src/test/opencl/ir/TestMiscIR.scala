package opencl.ir

import ir.TypeChecker
import ir.ast.{Expr, Value}
import org.junit.Test

/**
  * Created by federico on 25/07/17.
  */

class TestMiscIR {
  @Test
  def buildTupleFromExprTest():Unit = {
    val expr1:Value = 1.0f
    val expr2:Value = 0.0f
    val tuple:Value = (expr1, expr2)
    TypeChecker.check(tuple)
  }
}