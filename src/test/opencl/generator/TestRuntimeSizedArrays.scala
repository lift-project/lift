package opencl.generator

import ir.ast.fun
import ir.{ArrayTypeWSWC, RuntimeSizedArrayType}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, LoadExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{Ignore, Test}

object TestRuntimeSizedArrays extends LoadExecutor

class TestRuntimeSizedArrays {

  @Ignore
  @Test
  def reduce(): Unit = {
    val inputSize = 1023
    val inputData = Array(inputSize) ++ Array.fill(inputSize)(util.Random.nextInt(5))

    val l = fun(
      RuntimeSizedArrayType(Int),
      a => toGlobal(MapSeq(idI)) o ReduceSeq(fun((acc, x) => addI(acc, x)), 0) $ a
    )

    val (output: Array[Int], runtime) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.drop(1).sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Ignore
  @Test
  def reduceFloat(): Unit = {
    val inputSize = 1023
    val inputData = Array(java.lang.Float.intBitsToFloat(inputSize)) ++
      Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val l = fun(
      RuntimeSizedArrayType(Float),
      a => toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, x) => add(acc, x)), 0.0f) $ a
    )

    val (output: Array[Float], runtime) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.drop(1).sum, output.sum, 0.0)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Ignore
  @Test
  def nestedReduce(): Unit = {
    def createRow(inputSize: Int) = {
      Array(inputSize) ++
        Array.fill(inputSize)(util.Random.nextInt(5))
    }

    val size = 127
    val offsets = (0 until size).scanLeft(size)((o, i) => o + (10 + i) + 1).dropRight(1).toArray
    val inputData = Array(offsets) ++ Array.tabulate(size)(i => createRow(10 + i))

//    val v10 = inputData.flatten
//    println(s"v10.length: ${v10.length}")
//    print(s"v10: [")
//    for (v <- v10)
//      print(v + " ")
//    println("]")
//    println(s"v10(v10(0)): ${v10(v10(0))}")
//    println(s"v10(v10(1)): ${v10(v10(1))}")
//    println(s"v10(v10(2)): ${v10(v10(2))}")
//
//    val res = Array.fill(size)(0)
//    for (gl_id <- 0 until size) {
//      var acc = 0
//      for (i <- 0 until v10(v10(gl_id))) {
//        acc = acc + v10(1 + v10(gl_id) + i)
//      }
//      res(gl_id) = acc
//    }
//    for (i <- 0 until size) {
//      println(s"acc[$i]: ${res(i)}")
//    }
//    println(s"acc.sum: ${res.sum}")

    val N = SizeVar("N") // TODO: implement proper inference of N

    val l = fun(
      ArrayTypeWSWC(RuntimeSizedArrayType(Int), 127),
      a => MapGlb(toGlobal(MapSeq(idI)) o ReduceSeq(fun((acc, x) => addI(acc, x)), 0)) $ a
    )

    val (output: Array[Int], runtime) = Execute(inputData.length)(l, inputData)

    assertEquals(inputData.drop(1).map(_.drop(1).sum).sum, output.sum)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

}
