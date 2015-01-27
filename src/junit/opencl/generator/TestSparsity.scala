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

  def generateSparseArray(length:Int) : Array[(Int,Float)]=
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

  @Test def DROP_WHILE() {
    val length = 2048
    val vect = (1 to length).toArray
    val limit = 1024
    val gold = vect.dropWhile(_<limit)


    val lessThan = UserFunDef("lessThan", Array("x", "s"), "{return (x<s);}", Seq(Float, Float), Int)

    val dropFunc = fun(ArrayType(Float, Var("N")), Float, (input, limit) =>
      DropWhileSeq(
        fun(x => lessThan(x,limit))
      ) $ input
    )
    val code = Compile(dropFunc)
    //println(code)
    val (output, runtime) = Execute(1,1)(dropFunc, vect, limit, length)
    println("output(0) = " + output(0))
    println(output.toList)
    println("runtime = " + runtime)
  }

  @Test def SPARSE_VECTOR_DOT_PRODUCT() {
    val sum = UserFunDef(
      "sum",
      Array("acc","v"),
      "return (acc+v._1);",
      Seq(Int,TupleType(Int,Int)),
      Int
    )

    val select = UserFunDef(
      "select",
      Array("acc", "v"),
      "{return (v._1 == 0)?acc:v;}",
      Seq(TupleType(Int,Int), TupleType(Int,Int)),
      TupleType(Int,Int)
    )

    val eqMult = UserFunDef(
      "eqMult",
      Array("a","b"),
      "{if(a._0 == b._0){ a._1 = (a._1)*(b._1); }else{ a._1 = 0; } return a;}",
      Array(TupleType(Int,Int),TupleType(Int,Int)),
      TupleType(Int,Int)
    )

    val vectA = List((1, 9),(3, 3),(8, 4),(16,7),(19,1),(20,2),(21,3)).toArray.map((t) => Array(t._1, t._2)).flatten
    val vectB = List((1, 9),(2, 3),(8, 4),(15,7),(19,1),(20,2),(22,3)).toArray.map((t) => Array(t._1, t._2)).flatten
    val vSize = 7

    val N = SizeVar("N")

    val f = fun(
      ArrayType(Int, N),  //first list
      ArrayType(Int, N),  //second list
      (A,B) => {
        ReduceSeq(sum, 0) o MapWrg( //map across the first list
          fun(aElem =>
            ReduceSeq(select, 0.0f) o //reduce to find ones which are "true"
              MapSeq( //map across the first list
                fun(bElem => eqMult(aElem, bElem)) //produce a new array of equational values
              ) $ B //map across the second list
          )) $ A //map across the first list
      }
    )

    val (output, runtime) = Execute(vSize)(f, vectA, vectB, vSize*2)
    println(output.deep.mkString)
  }

  @Test def SPARSE_VECTOR_NEG(){
    val rawVector = generateSparseArray(1024)
    val inputVector = rawVector.map((t) => Array(t._1, t._2)).flatten.map((x)=>x.toFloat)
    val inputSize = inputVector.length/2

    val gold = rawVector.map((vi) => (vi._1, -vi._2)).map((t) => Array(t._1, t._2)).flatten.map((x)=>x.toFloat)

    val negElem = UserFunDef("negElem", "x", "{ x._0 = x._0; x._1 = -x._1; return x; }",
      TupleType(Float, Float), TupleType(Float, Float))

    val f = fun(ArrayType(TupleType(Float, Float), Var("N")), (input) =>
      Join() o MapWrg(
        Join() o MapLcl(MapSeq(negElem)) o Split(4)
      ) o Split(1024) $ input
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
//    val gold = inputArray.map(_ * alpha)

    val scalFun = fun(ArrayType(TupleType(Float,Float), Var("N")), Float, (input, alpha) =>
      Join() o MapWrg(
        Join() o MapLcl(MapSeq(
          fun(x => sparseElemMult(x,alpha))
        )) o Split(4)
      ) o Split(1024) $ input
    )

    val (output, runtime) = Execute(inputVector.length)(scalFun, inputVector, alpha, inputVector.size)

//    (gold, output).zipped.map(assertEquals(_, _, 0.0))

    println("output(0) = " + output(0))
    println("input = " + inputVector.toList)
    println("output = " + output.toList)
    println("runtime = " + runtime)
  }

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
}
