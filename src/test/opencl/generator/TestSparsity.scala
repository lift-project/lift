package opencl.generator

import arithmetic._
import opencl.executor._
import org.junit.Assert._
import org.junit.{Ignore, AfterClass, BeforeClass, Test}
import opencl.ir._
import ir._

object TestSparsity {
  @BeforeClass def TestMatrixBasic() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}


class TestSparsity {


  val id = UserFunDef("id", "i", "return i;", Float, Float)
  val double = UserFunDef("times2", "i", "return (i+i);", Float, Float)
  val square = UserFunDef("sq","i", "return (i*i);", Float,Float)
  val add = UserFunDef("add", Array("a","b"), "return a+b;", Seq(Float, Float), Float)
  val mult = UserFunDef("mult", Array("a","b"), "return a*b;", Seq(Float, Float), Float)
  val or = UserFunDef("or", Array("a","b"), "return (((a>0.0f)||(b>0.0f))?(1.0f):(0.0f));", Seq(Float, Float), Float)
  val and = UserFunDef("and", Array("a","b"), "return (((a>0.0f)&&(b>0.0f))?(1.0f):(0.0f));", Seq(Float, Float), Float)

  // Debug switch: show input and output
  val printArrays: Boolean = false


  @Test def TAIL_TEST (): Unit = {
    val vector : Array[Float] = Array.range(1024,2048).map(_.toFloat)
    val gold = vector.tail

    val f = fun (ArrayType(Float,Var("N")),(input) =>
      MapSeq(id) o Tail() $ input
    )

    val (output:Array[Float], runtime) = Execute(1, 1)(f,vector)

    println("output(0) = "+output(0))
    if (printArrays) {
      println("vector = " + vector.toList.toString())
      println("output = " + output.toList.toString())
    }
    println("runtime = " + runtime)
    assertArrayEquals(gold,output,0.0f)
  }

  @Test def HEAD_TEST (){
    val vector : Array[Float] = Array.range(1024,2048).map(_.toFloat)
    val gold = Array(vector.head)

    val f = fun (ArrayType(Float,Var("N")),(input) =>
        MapSeq(id) o Head() $ input
    )
    val (output:Array[Float], runtime) = Execute(1,1)(f,vector)


    println("output(0) = "+output(0))
    if (printArrays) {
      println("vector = " + vector.toList.toString())
      println("output = " + output.toList.toString())
    }
    println("runtime = " + runtime)
    assertArrayEquals(gold,output,0.0f)
  }

  @Test def HEAD_TAIL_TEST () {
    val vector:Array[Float] = Array.range(1024,2048).map(_.toFloat)
    val gold = Array(vector.tail.head)

    val f = fun (ArrayType(Float,Var("N")),(input) =>
      MapSeq(id) o Head() o Tail() $ input
    )
    val (output: Array[Float], runtime) = Execute(vector.length)(f,vector)

    println("output(0) = "+output(0))
    if (printArrays) {
      println("vector = " + vector.toList.toString())
      println("output = " + output.toList.toString())
    }
    println("runtime = " + runtime)
    assertArrayEquals(gold,output,0.0f)
  }

  @Test def MULTIDIMENSIONAL_HEAD () {
    val vector:Array[Float] = Array.range(1024,2048).map(_.toFloat)
    val gold:Array[Float] = vector.grouped(32).map(_.head).toArray

    val f = fun (ArrayType(Float,Var("N")),(input) =>
      Join() o MapSeq(MapSeq(id) o Head()) o Split(32) $ input
    )
    val (output: Array[Float], runtime) = Execute(vector.length)(f,vector)

    println("output(0) = "+output(0))
    if (printArrays) {
      println("vector = " + vector.toList.toString())
      println("output = " + output.toList.toString())
    }
    println("runtime = " + runtime)
    assertArrayEquals(gold,output,0.0f)
  }

  @Test def MULTIDIMENSIONAL_TAIL () {
    val vector:Array[Float] = Array.range(1024,2048).map(_.toFloat)
    val gold:Array[Float] = vector.grouped(32).map(_.tail).flatten.toArray

    val f = fun (ArrayType(Float,Var("N")),(input) =>
      Join() o MapSeq(MapSeq(id) o Tail()) o Split(32) $ input
    )
    val (output: Array[Float], runtime) = Execute(vector.length)(f,vector)

    println("output(0) = "+output(0))
    if (printArrays) {
      println("vector = " + vector.toList.toString())
      println("output = " + output.toList.toString())
    }
    println("runtime = " + runtime)
    assertArrayEquals(gold,output,0.0f)
  }

  //TODO: Clean up below tests
  @Ignore
  @Test def SPARSE_VECTOR_DOT_PRODUCT() {
    val sum = UserFunDef("sum", Array("acc","v"),
      "return (acc+v);",
      Seq(Int,Int), Int
    )

    val matchIndicies = UserFunDef("matchIndicies", Array("acc", "v"),
      "{return (v != 0)?v:acc;}",
      Seq(Int,Int), Int
    )

    val eqMult = UserFunDef("eqMult", Array("a","b"),
      "{if(a._0 == b._0){ return (a._1)*(b._1); }else{ return 0; }}",
      Array(TupleType(Int,Int),TupleType(Int,Int)), Int
    )

    val intId = UserFunDef("intId", "i", "return i;", Int,Int)

    val vectA = List((1, 9),(3, 3),(8, 4),(16,7),(19,1),(20,2),(21,3), (24,10)).toArray.map((t) => Array(t._1, t._2)).flatten
    val vectB = List((1, 9),(2, 3),(8, 4),(15,7),(19,1),(20,2),(22,3), (24,10)).toArray.map((t) => Array(t._1, t._2)).flatten

    val N = SizeVar("N")

    val f = fun(
      ArrayType(TupleType(Int,Int), N),  //first list
      ArrayType(TupleType(Int,Int), N),  //second list
      (A,B) => {
        toGlobal(MapSeq(intId)) o ReduceSeq(sum,0) o Join() o MapSeq( //map across the first list
          fun(aElem =>
            toGlobal(MapSeq(intId)) o ReduceSeq(matchIndicies, 0) o //reduce to find ones which are "true"
              MapSeq( //map across the second list
                fun(bElem => eqMult(aElem, bElem)) //produce a new array of equational values
              ) $ B //map across the second list
          )
        ) $ A //map across the first list
      }
    )

    val (output:Array[Int], runtime) = Execute(1,1)(f, vectA, vectB)
    println(output.toList.toString())
  }

  /*
   * Negate a sparse vector. Special case of scalar multiplication
   */
  @Ignore
  @Test def SPARSE_VECTOR_NEGATION(){
    val rawVector = generateSparseArray(1024)
    val inputVector = rawVector.map((t) => Array(t._1, t._2)).flatten.map((x)=>x.toFloat)
    val inputSize = inputVector.length/2

    val gold = rawVector.map((vi) => (vi._1, -vi._2)).map((t) => Array(t._1, t._2)).flatten.map((x)=>x.toFloat)

    val negElem = UserFunDef("negElem", "x", "{ x._0 = x._0; x._1 = -(x._1); return x; }",
      TupleType(Float, Float), TupleType(Float, Float))

    val f = fun(ArrayType(TupleType(Float, Float), Var("N")), (input) =>
       MapSeq(negElem)  $ input
    )

    val (output:Array[(Float,Float)], runtime) = Execute(inputSize)(f, inputVector)



    println("output(0) = " + output(0))
    if (printArrays) {
      println(inputVector.toList)
      println(output.toList)
    }
    println("runtime = " + runtime)
  }

  /*
   * Multiply a sparse vector by a scalar
   */
  @Ignore
  @Test def SPARSE_VECTOR_SCAL() {

    val sparseElemMult = UserFunDef("sem", Array("x","s"), "{ x._0 = x._0; x._1 = s*x._1; return x; }",
      Seq(TupleType(Float, Float),Float), TupleType(Float, Float))

    val rawVector = generateSparseArray(1024)
    val inputVector = rawVector.map((t) => Array(t._1, t._2)).flatten.map((x)=>x.toFloat)
    val inputSize = inputVector.length/2
    val alpha = 2.5f
    val gold = inputVector.map(_ * alpha)

    val scalFun = fun(ArrayType(TupleType(Float,Float), Var("N")), Float, (input, alpha) =>
      MapSeq(
          fun(x => sparseElemMult(x,alpha))
        ) $ input
    )

    val (output:Array[(Float,Float)], runtime) = Execute(inputVector.length)(scalFun, inputVector, alpha)

    println("output(0) = " + output(0))
    if (printArrays) {
      println("input = " + inputVector.toList)
      println("output = " + output.toList)
    }
    println("runtime = " + runtime)
  }

  /*
   ******************************************************************
   * Pure scala verifications of possible implementation approaches *
   ******************************************************************
   */

  @Test def PURE_SCALA_DOT_PRODUCT(){
    val vectA = List((1, 9.0),(3, 3.0),(8, 4.0),(16,7.0),(19,1.0),(20,2.0),(21,3.0)).toArray
    val vectB = List((1, 9.0),(2, 3.0),(8, 4.0),(15,7.0),(19,1.0),(20,2.0),(22,3.0)).toArray
    val initial = (vectB,0.0)

    def foldBody(acc:Tuple2[Array[Tuple2[Int,Double]],Double], vi:Tuple2[Int,Double]) : Tuple2[Array[Tuple2[Int,Double]],Double] = {
      val greaterOrEqual = acc._1.dropWhile((vj: Tuple2[Int, Double]) => vj._1 < vi._1)
      if(greaterOrEqual.head._1 == vi._1)
        (greaterOrEqual.tail, acc._2+(vi._2*greaterOrEqual.head._2))
      else
        (greaterOrEqual, acc._2)
    }
    val result = vectA.foldLeft(initial)( foldBody )
    println(result._2 )
  }

  @Test def PURE_SCALA_SEARCH_DOT_PRODUCT() {
    val vectA = List((1, 9.0),(3, 3.0),(8, 4.0),(16,7.0),(19,1.0),(20,2.0),(21,3.0)).toArray
    val vectB = List((1, 9.0),(2, 3.0),(8, 4.0),(15,7.0),(19,1.0),(20,2.0),(22,3.0)).toArray

    def findCorresponding(item : Int, list: Array[Tuple2[Int, Double]]) : Double = {
      for(li <- list) {
        if(li._1 == item)
          return li._2
      }
      0
    }
    val result = vectA.foldLeft(0.0)((acc:Double, i: Tuple2[Int,Double]) => (acc + (i._2 * findCorresponding(i._1, vectB))))
    println(result)
  }

  def sparseDotProduct(arrA:Array[(Int,Float)], arrB:Array[(Int,Float)]) = {
    //todo implement "gold" version
  }

  def generateSparseArray(length:Int) : Array[(Int,Float)] = {
    val baseArray = Array.fill(length)((util.Random.nextInt(5) + 1), (util.Random.nextInt(5).toFloat))
    for (a <- 1 to length - 1) {
      //for each element, append our random index to find our true index
      baseArray(a) = (baseArray(a - 1)._1 + baseArray(a)._1, baseArray(a)._2)
    }
    baseArray
  }

 }
