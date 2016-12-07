package opencl.generator

import lift.arithmetic._
import ir._
import ir.ast._
import opencl.executor._
import opencl.ir._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import opencl.ir.pattern._

object TestHeadTail {
  @BeforeClass def TestMatrixBasic(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}


class TestHeadTail {


  val id = UserFun("id", "i", "return i;", Float, Float)
  val double = UserFun("times2", "i", "return (i+i);", Float, Float)
  val square = UserFun("sq","i", "return (i*i);", Float,Float)
  val add = UserFun("add", Array("a","b"), "return a+b;", Seq(Float, Float), Float)
  val mult = UserFun("mult", Array("a","b"), "return a*b;", Seq(Float, Float), Float)
  val or = UserFun("or", Array("a","b"), "return (((a>0.0f)||(b>0.0f))?(1.0f):(0.0f));", Seq(Float, Float), Float)
  val and = UserFun("and", Array("a","b"), "return (((a>0.0f)&&(b>0.0f))?(1.0f):(0.0f));", Seq(Float, Float), Float)



  @Test def TAIL_TEST (): Unit = {
    val vector : Array[Float] = Array.range(1024,2048).map(_.toFloat)
    val gold = vector.tail

    val f = fun (ArrayType(Float,SizeVar("N")),(input) =>
      MapSeq(id) o Tail() $ input
    )

    val (output:Array[Float], runtime) = Execute(1, 1)(f,vector)

    println("output(0) = "+output(0))
    println("vector = " + vector.toList.toString())
    println("output = " + output.toList.toString())
    println("runtime = " + runtime)
    assertArrayEquals(gold,output,0.0f)
  }

  @Test def HEAD_TEST (): Unit = {
    val vector : Array[Float] = Array.range(1024,2048).map(_.toFloat)
    val gold = Array(vector.head)

    val f = fun (ArrayType(Float,SizeVar("N")),(input) =>
        MapSeq(id) o Head() $ input
    )
    val (output:Array[Float], runtime) = Execute(1,1)(f,vector)


    println("output(0) = "+output(0))
    println("vector = "+vector.toList.toString())
    println("output = "+ output.toList.toString())
    println("runtime = " + runtime)
    assertArrayEquals(gold,output,0.0f)
  }

  @Test def HEAD_TAIL_TEST (): Unit = {
    val vector:Array[Float] = Array.range(1024,2048).map(_.toFloat)
    val gold = Array(vector.tail.head)

    val f = fun (ArrayType(Float,Var("N",StartFromRange(2))),(input) =>
      MapSeq(id) o Head() o Tail() $ input
    )
    val (output: Array[Float], runtime) = Execute(vector.length)(f,vector)

    println("output(0) = "+output(0))
    println("vector = "+vector.toList.toString())
    println("output = "+ output.toList.toString())
    println("runtime = " + runtime)
    assertArrayEquals(gold,output,0.0f)
  }

  @Test def MULTIDIMENSIONAL_HEAD (): Unit = {
    val vector:Array[Float] = Array.range(1024,2048).map(_.toFloat)
    val gold:Array[Float] = vector.grouped(32).map(_.head).toArray

    val f = fun (ArrayType(Float,SizeVar("N")),(input) =>
      Join() o MapSeq(MapSeq(id) o Head()) o Split(32) $ input
    )
    val (output: Array[Float], runtime) = Execute(vector.length)(f,vector)

    println("output(0) = "+output(0))
    println("vector = "+vector.toList.toString())
    println("output = "+ output.toList.toString())
    println("runtime = " + runtime)
    assertArrayEquals(gold,output,0.0f)
  }

  @Test def MULTIDIMENSIONAL_TAIL (): Unit = {
    val vector:Array[Float] = Array.range(1024,2048).map(_.toFloat)
    val gold:Array[Float] = vector.grouped(32).map(_.tail).flatten.toArray

    val f = fun (ArrayType(Float,SizeVar("N")),(input) =>
      Join() o MapSeq(MapSeq(id) o Tail()) o Split(32) $ input
    )
    val (output: Array[Float], runtime) = Execute(vector.length)(f,vector)

    println("output(0) = "+output(0))
    println("vector = "+vector.toList.toString())
    println("output = "+ output.toList.toString())
    println("runtime = " + runtime)
    assertArrayEquals(gold,output,0.0f)
  }

 }
