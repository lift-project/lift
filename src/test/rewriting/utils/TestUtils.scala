package rewriting.utils

import ir._
import ir.ast._
import lift.arithmetic.{Cst, SizeVar, Var}
import opencl.executor.Eval
import opencl.ir._
import org.junit.Assert._
import org.junit.Test

class TestUtils {

  @Test
  def replaceVariableNames(): Unit = {
    val testString = "v__0, v__2135646"

    val replaced = DumpToFile.findAndReplaceVariableNames(testString)
    assertEquals(2, DumpToFile.findVariables(replaced).length)
  }

  @Test
  def replaceNamesSubstring(): Unit = {
    val testString = "v__3, v__12, v__1)"

    val replaced = DumpToFile.findAndReplaceVariableNames(testString)
    assertEquals(3, DumpToFile.findVariables(replaced).length)
  }

  @Test
  def replaceNamesSubstring2(): Unit = {
    val testString = "v__3, v__1, v__12"

    val replaced = DumpToFile.findAndReplaceVariableNames(testString)
    assertEquals(3, DumpToFile.findVariables(replaced).length)
  }

  @Test
  def replaceNamesSubstring3(): Unit = {
    val testString = "Split((v__2*1/^(v__2739)))"

    val replaced = DumpToFile.findAndReplaceVariableNames(testString)
    assertEquals(2, DumpToFile.findVariables(replaced).length)
  }

  @Test
  def replaceVariableNamesUnrolled(): Unit = {
    val testString = "v__1, v__1_0, v__1_1"

    val replaced = DumpToFile.findAndReplaceVariableNames(testString)
    assertEquals(1, DumpToFile.findVariables(replaced).length)
  }

  @Test
  def replaceOverlap(): Unit = {
    val testString = "v__11, const global float* restrict v__615, const global float* restrict v__616, const global float*       restrict v__617, float v__618, float v__619, global float* v__644, global float* v__640, global float* v__642,       int v__10, int v__11){\n   15 #ifndef WORKGROUP_GUARD\n   16 #define WORKGROUP_GUARD\n   17 #endif\n   18 WORKGROUP_GUARD\n   19 {\n   20   /* Static local memory */\n   21   local float v__638[1];"

    val replaced = DumpToFile.findAndReplaceVariableNames(testString)
    assertEquals(11, DumpToFile.findVariables(replaced).length)
  }

  @Test
  def constantDivisionDumpToString(): Unit = {

    val f = \(
      ArrayType(Float, SizeVar("N")),
      Map(Map(plusOne)) o Split(Cst(16) /^ Var()) $ _
    )

    val string = DumpToFile.dumpLambdaToString(f)
    Eval(string)
  }
}
