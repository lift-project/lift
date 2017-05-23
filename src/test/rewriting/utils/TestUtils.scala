package rewriting.utils

import ir._
import ir.ast._
import opencl.ir._
import lift.arithmetic._
import opencl.executor.Eval
import opencl.ir.pattern.MapSeq
import org.junit.Assert._
import org.junit.Test

class TestUtils {

  @Test
  def replaceVariableNames(): Unit = {
    val testString = "v__0, v__2135646"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(2, Utils.findVariables(replaced).length)
  }

  @Test
  def replaceNamesSubstring(): Unit = {
    val testString = "v__3, v__12, v__1)"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(3, Utils.findVariables(replaced).length)
  }

  @Test
  def replaceNamesSubstring2(): Unit = {
    val testString = "v__3, v__1, v__12"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(3, Utils.findVariables(replaced).length)
  }

  @Test
  def replaceNamesSubstring3(): Unit = {
    val testString = "Split((v__2*1/^(v__2739)))"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(2, Utils.findVariables(replaced).length)
  }

  @Test
  def replaceVariableNamesUnrolled(): Unit = {
    val testString = "v__1, v__1_0, v__1_1"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(1, Utils.findVariables(replaced).length)
  }

  @Test
  def replaceOverlap(): Unit = {
    val testString = "v__11, const global float* restrict v__615, const global float* restrict v__616, const global float*       restrict v__617, float v__618, float v__619, global float* v__644, global float* v__640, global float* v__642,       int v__10, int v__11){\n   15 #ifndef WORKGROUP_GUARD\n   16 #define WORKGROUP_GUARD\n   17 #endif\n   18 WORKGROUP_GUARD\n   19 {\n   20   /* Static local memory */\n   21   local float v__638[1];"

    val replaced = Utils.findAndReplaceVariableNames(testString)
    assertEquals(11, Utils.findVariables(replaced).length)
  }

  @Test
  def constantDivisionDumpToString(): Unit = {

    val f = \(
      ArrayType(Float, SizeVar("N")),
      Map(Map(plusOne)) o Split(Cst(16) /^ Var()) $ _
    )

    val string = Utils.dumpLambdaToString(f)
    println(string)
    Eval(string)
  }

  @Test
  def printCorrectRangesForVars(): Unit = {
    val M = Var("M", StartFromRange(32))
    val N = Var("N", RangeUnknown)
    val O = SizeVar("O")

    val f = fun(
      ArrayType(Float, M),
      ArrayType(Float, N),
      ArrayType(Float, O),
      (m,n,o) => MapSeq(id) $ m)

    val string = rewriting.utils.Utils.dumpLambdaToString(f)
    assertTrue(string contains "Var(\"M\", StartFromRange(32))")
    assertTrue(string contains "Var(\"N\", RangeUnknown)")
    assertTrue(string contains "SizeVar(\"O\")")
    Eval(string)
  }

}
