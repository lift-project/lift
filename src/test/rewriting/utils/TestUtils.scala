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
  def sameExpressionTwice(): Unit = {
    val f1 = \(ArrayType(Float, SizeVar("N")), Map(plusOne) $ _)
    val f2 = \(ArrayType(Float, SizeVar("N")), Map(plusOne) $ _)

    val string1 = Utils.dumpLambdaToString(f1)
    val string2 = Utils.dumpLambdaToString(f2)

    assertEquals(string1, string2)
  }

  @Test
  def declarationOrder(): Unit = {
    Var()
    Var()
    Var()
    val N = Var("N", StartFromRange(1))
    val M = Var("M", StartFromRange(N))

    val f = \(ArrayType(Float, N), ArrayType(Float, M), (a, b) => Map(\(x => Map(\(y => add(x,y))) $ b)) $ a)

    val string = Utils.dumpLambdaToString(f)
    Eval(string)
  }

  @Test
  def sameExpressionTwice2(): Unit = {
    val f1 = \(ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      Map(Map(plusOne) o Gather(ReorderWithStride(SizeVar("")))) $ _)
    val f2 = \(ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      Map(Map(plusOne) o Gather(ReorderWithStride(SizeVar("")))) $ _)

    val string1 = Utils.dumpLambdaToString(f1)
    val string2 = Utils.dumpLambdaToString(f2)

    assertEquals(string1, string2)
  }

  @Test
  def sameExpressionTwice3(): Unit = {

    val N1 = SizeVar("N")
    val M1 = SizeVar("M")
    val M2 = SizeVar("M")
    val N2 = SizeVar("N")


    val f1 = \(ArrayType(ArrayType(Float, M1), N1), Map(Map(plusOne)) $ _)
    val f2 = \(ArrayType(ArrayType(Float, M2), N2), Map(Map(plusOne)) $ _)

    val string1 = Utils.dumpLambdaToString(f1)
    val string2 = Utils.dumpLambdaToString(f2)

    assertEquals(string1, string2)
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
    //assertTrue(string contains "SizeVar(\"O\")")
    Eval(string)
  }

}
