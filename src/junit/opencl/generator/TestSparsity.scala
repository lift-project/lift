package junit.opencl.generator

import opencl.executor._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
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

  def generateSparseArray(length:Int) : Array[(Int,Float)] =
  {
    var a = 0;
    var baseArray = Array.fill(length)((util.Random.nextInt(5)+1),(util.Random.nextInt(5).toFloat))
    for(a <- 1 to length-1)
    {
      //for each element, append our random index to find our true index
      baseArray(a) = (baseArray(a-1)._1+baseArray(a)._1, baseArray(a)._2)
    }
    baseArray
  }

  @Test def HEAD_TAIL_TEST () {
    val vector = Array.range(1024,2048) //(util.Random.nextInt(5).toFloat)
    val gold = Array(vector(0))
    val sum = UserFunDef("sum", Array("a","b"), "{return (a+b);}", Seq(Float,Float), Float)
    val f = fun (ArrayType(Float,Var("N")),(input) =>
      //      ReduceSeq(sum,0.0f) $ input
      //        Head() $ input
      Head() o Tail() $ input
    )
    val (output, runtime) = Execute(vector.length)(f,vector,vector.size)


    println("output(0) = "+output(0))
    println("vector = "+vector.toList.toString())
    println("output = "+ output.toList.toString())
    println("runtime = " + runtime)
  }

  @Test def HEAD_TEST (){
    val vector : Array[Float] = Array.range(1024,2048).map(_.toFloat) //(util.Random.nextInt(5).toFloat)
    val gold = Array(vector(0))
    val f = fun (ArrayType(Float,Var("N")),(input) =>
//      ReduceSeq(sum,0.0f) $ input
//        Head() $ input
//        Head() $ input
        Join() o MapGlb(Head()) o Split(32) $ input
    )
    val (output, runtime) = Execute(vector.length)(f,vector,vector.size)


    println("output(0) = "+output(0))
    println("vector = "+vector.toList.toString())
    println("output = "+ output.toList.toString())
    println("runtime = " + runtime)
//    assertArrayEquals(gold,output,0.0f)
  }

  @Test def TAIL_TEST (): Unit = {
    val vector = Array.range(0,256).map(_.toFloat) //(util.Random.nextInt(5).toFloat)
//    val vector = Array.range(32,65).map(_.toFloat) //(util.Random.nextInt(5).toFloat)
    val gold = Array(vector(0))
    val square = UserFunDef("square", "x", "{return (x*x);}", Float, Float)
    val f = fun (ArrayType(Float,Var("N")),(input) =>
      //      ReduceSeq(sum,0.0f) $ input
        Join() o MapSeq(Tail()) o Split(4) $ input
//    Tail() o Tail() $ input
//      MapSeq(square) o Tail() o Tail() o Tail() $ input
    )
    val (output, runtime) = Execute(vector.length)(f,vector,vector.size)


    println("output(0) = "+output(0))
    println("vector = "+vector.toList.toString())
    println("output = "+ output.toList.toString())
    println("runtime = " + runtime)
  }

  @Test def SPARSE_VECTOR_DOT_PRODUCT() {
    val sum = UserFunDef("sum", Array("acc","v"),
      "return (acc+v);",
      Seq(Int,Int), Int
    )

    val select = UserFunDef("select", Array("acc", "v"),
      "{return (v == 0)?acc:v;}",
      Seq(Int,Int), Int
    )

    val eqMult = UserFunDef("eqMult", Array("a","b"),
      "{if(a._0 == b._0){ return (a._1)*(b._1); }else{ return 0; }}",
      Array(TupleType(Int,Int),TupleType(Int,Int)), Int
    )

    val vectA = List((1, 9),(3, 3),(8, 4),(16,7),(19,1),(20,2),(21,3)).toArray.map((t) => Array(t._1, t._2)).flatten
    val vectB = List((1, 9),(2, 3),(8, 4),(15,7),(19,1),(20,2),(22,3)).toArray.map((t) => Array(t._1, t._2)).flatten
    val vSize = 7

    val N = SizeVar("N")

    val f = fun(
      ArrayType(TupleType(Int,Int), N),  //first list
      ArrayType(TupleType(Int,Int), N),  //second list
      (A,B) => {
        ReduceSeq(sum, 0) o MapWrg( //map across the first list
          fun(aElem =>
            //fails tests, as "ReduceSeq" returns a value which is an Arr(int,1), not an int I believe.
            ReduceSeq(select, 0) o //reduce to find ones which are "true"
              MapSeq( //map across the first list
                fun(bElem => eqMult(aElem, bElem)) //produce a new array of equational values
              ) $ B //map across the second list
          )
        ) $ A //map across the first list
      }
    )

    val (output, runtime) = Execute(vSize)(f, vectA, vectB, vSize*2)
    println(output.deep.mkString)
  }

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

    val (output, runtime) = Execute(inputSize)(f, inputVector, inputVector.size)

    assertArrayEquals(gold, output, 0.0f)

    println("output(0) = " + output(0))
    println(inputVector.toList)
    println(output.toList)
    println("runtime = " + runtime)
  }

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

    val (output, runtime) = Execute(inputVector.length)(scalFun, inputVector, alpha, inputVector.size)
    (gold, output).zipped.map(assertEquals(_, _, 0.0))
    println("output(0) = " + output(0))
    println("input = " + inputVector.toList)
    println("output = " + output.toList)
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

  /*
   *************************************************************
   * Old and broken tests below - not intended for actual use. *
   *************************************************************
   */

  @Test def SEARCH_HOST() {
    //    val rawVector = generateSparseArray(1024)
    //    val inputVector = rawVector.map((t) => Array(t._1, t._2)).flatten.map((x)=>x.toFloat)
    //    val inputSize = inputVector.length/2
    //
    //    val pred = UserFunDef("predicate", Array("s", "i"), "return (i._0 == s);",
    //      Seq(Float,TupleType(Float, Float)), Int)
    //    val proc = UserFunDef("process", "i", "return (i._1);", TupleType(Float, Float), Float)
    //
    //
    //    val searchFunc = fun(ArrayType(TupleType(Float, Float), Var("N")), (in) => {
    //      LinearSearchSeq(fun(x => {pred(2.0f,x)}), proc, 0.0f) $ in
    //    })
    //
    //    val (output, runtime) = Execute(inputSize)(searchFunc, inputVector, inputVector.size)
    //
    //    println(output.deep.mkString)
    //    println("output(0) = " + output(0))
    //    println("runtime = " + runtime)
  }

  @Test def SEARCH_DOT_PRODUCT() {
    //    val rawVector = generateSparseArray(1024)
    //    val inputVector = rawVector.map((t) => Array(t._1, t._2)).flatten.map((x)=>x.toFloat)
    //    val inputSize = inputVector.length/2
    //
    ////    val gold = vect.dropWhile(_<limit)
    //
    //    val pred = UserFunDef("predicate", Array("i", "s"), "{return (i==s)}", Seq(Float, Float), Int)
    //
    //    val correspondFunc = fun(
    //      ArrayType(Int, Var("N")),
    //      ArrayType(Int, Var("M")),
    //      (arrA, arrB) =>{
    //        Join() o MapWrg(
    //          MapLcl( //search and correspond
    //            fun (x =>
    //              SparseSearch(x, arrB, pred)
    //            )
    //          )
    //        ) o Split(128) $ arrA //split array A, and map across it
    //      }
    //    )
    //
    //    val code = Compile(dropFunc)
    //    //println(code)
    //    val (output, runtime) = Execute(length)(dropFunc, vect, limit, length)
    //    println("output(0) = " + output(0))
    //    println(output.toList)
    //    println("runtime = " + runtime)
  }

  @Test def DROP_LEFT_TEST() {
//    val length = 4096
//    val vect = (1 to length).toArray
//    val limit = 1024
//    val gold = vect.dropWhile(_<limit)
//    val lessThan = UserFunDef("lessThan", Array("x", "s"), "{return (x<s);}", Seq(Float, Float), Int)
//    //    val lessThan = UserFunDef("lessThan", "x", "{return (x<0);}", Float, Int)
//
//    val dropTest = fun(ArrayType(Float, Var("N")), Float, (input, limit) =>
//      DropLeftSeq(fun(x => lessThan(limit, x))) $ input
//    )
//
//    val code = Compile(dropTest)
//    //println(code)
//    val (output, runtime) = Execute(length)(dropTest, vect, limit, length)
//    println("output(0) = " + output(0))
//    println(output.toList)
//    println("runtime = " + runtime)
  }

}
