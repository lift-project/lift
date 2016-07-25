package ir.codeGenerator

import ir._
import ir.Type
import ir.ast._
import ir.interpreter.Interpreter
import org.junit._
import opencl.ir._
/**
  * Created by potato on 25/07/16.
  */
class codeGeneratorTest {

  @Test
  def testJoin():Unit={
    val l = codeGenerator.generateLambda( ArrayType(Float,128))
    assert(true)

  }
}
