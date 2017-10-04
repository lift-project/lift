package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

object TestTriple extends TestWithExecutor

class TestTriple {
  val inputSize = 1024
  val add3Tuple = UserFun("add3", "t", "{ return t._0+t._1+t._2; }", TupleType(Float, Float, Float), Float)
  val add3 = UserFun("add3", Array("x", "y", "z"), "{ return x+y+z; }", Seq(Float, Float, Float), Float)
  val inputArray1 = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  val inputArray2 = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  val inputArray3 = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  val gold = (inputArray1 zip inputArray2 zip inputArray3).map({case ((a,b),c) => a+b+c})
  val inputArray = (inputArray1 zip inputArray2 zip inputArray3).map{ case ((a, b), c) => (a, b, c) }

  @Test def VECTOR_SUM_TRIPLE(): Unit = {
    val f = fun(ArrayTypeWSWC(TupleType(Float,Float,Float), SizeVar("N")), (input) =>
      Join() o MapWrg(
	Join() o  MapLcl(MapSeq(add3Tuple)) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputSize)[Array[Float]](f, inputArray)
    assertArrayEquals(gold, output, 0.0f)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_SUM_TRIPLE_2(): Unit = {
    val N = SizeVar("N")
    val f = fun(
      ArrayTypeWSWC(Float, N),
		  ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
		  (xs, ys, zs) =>
        Join() o MapWrg(
  	      Join() o  MapLcl(MapSeq(add3)) o Split(4)
        ) o Split(1024) $ Zip(xs, ys, zs)
    )

    val (output, runtime) = Execute(inputSize)[Array[Float]](f, inputArray1, inputArray2, inputArray3)
    assertArrayEquals(gold, output, 0.0f)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)    
  }

}
