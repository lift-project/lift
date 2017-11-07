package opencl.generator

import analysis.BenchmarkUserFun
import ir._
import opencl.executor._
import opencl.ir._
import org.junit.Assert._
import org.junit._

object TestMicroBenchmark extends TestWithExecutor

class TestMicroBenchmark {

  import BenchmarkUserFun.N

  @Test
  def inputTypesPlusOne(): Unit = {
    val types = BenchmarkUserFun.createInputTypes(plusOne)

    val expected = Seq(ArrayType(Float, N), ArrayType(Int, N))
    assertEquals(expected, types)
  }

  @Test
  def inputTypesAdd(): Unit = {
    val types = BenchmarkUserFun.createInputTypes(add)

    val expected = Seq(ArrayType(Float, N), ArrayType(Float, N), ArrayType(Int, N))
    assertEquals(expected, types)
  }

  @Test
  def inputTypesAddPair(): Unit = {
    val types = BenchmarkUserFun.createInputTypes(addPair)

    val expected = Seq(ArrayType(TupleType(Float, Float), N), ArrayType(TupleType(Float, Float), N), ArrayType(Int, N))
    assertEquals(expected, types)
  }

  @Test
  def wrapper(): Unit = {
    BenchmarkUserFun(add, 1024)
  }

//  @Test
//  def inputTypesVec(): Unit = {
//    val types = BenchmarkUserFun.createInputTypes(VectorizeUserFun(4, add))
//
//    val expected = Seq(ArrayType(Float4, N), ArrayType(Float4, N), ArrayType(Int, N))
//    assertEquals(expected, types)
//  }

  @Test
  def bla2(): Unit = {

  }

  @Test
  def bla(): Unit = {
    println(BenchmarkUserFun.benchmark(plusOne, 2*2048, 101))
  }

}
