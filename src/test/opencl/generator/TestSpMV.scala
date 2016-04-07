package opencl.generator

import apart.arithmetic.Var
import ir._
import ir.ast._
import ir.ast.UserFun._
import opencl.executor._
import opencl.ir._
import org.junit.Assert._
import org.junit.{Ignore, AfterClass, BeforeClass, Test}
import scala.reflect.ClassTag

import opencl.ir.pattern._

object TestSpMV {
  @BeforeClass def TestSpMV() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}



class TestSpMV {
  val t_id = UserFun("tuple_id", "x", "return x;", TupleType(Int, Int), TupleType(Int, Int))
  val i_id = UserFun("int_id", "x", "return x;", Int, Int)
  val int_add = UserFun("int_add", Array("a", "b"), "return a+b;", Array(Int, Int), Int);

  @Ignore @Test def REDUCE_SEARCH_MATRIX_VECTOR() {

    val sum = UserFun("sum", Array("acc","v"),
      "return (acc+v);",
      Seq(Int,Int), Int
    )

    val matchIndicies = UserFun("matchIndicies", Array("acc", "v"),
      "{return (v != 0)?v:acc;}",
      Seq(Int,Int), Int
    )

    val eqMult = UserFun("eqMult", Array("a","b"),
      """
        |if(a._0 == b._0){
        |  return (a._1)*(b._1);
        |  printf("Matching!\n");
        |  printf("Returning %d\n", (a._1)*(b._1));
        |}else{
        |  return 0;
        |}
      """.stripMargin,
      Array(TupleType(Int,Int),TupleType(Int,Int)), Int
    )

    val intId = UserFun("intId", "i", "return i;", Int,Int)

    val width = 1000
    val height = 1000
    val paddedWidth = 200
    val concreteVectLen = 500

    val vector = generateSparseArray(concreteVectLen, width)
    val matrix = generateSparseMatrix(width, height, paddedWidth)
    val gold = sparseMatrixVector(matrix, vector)

    val N = Var("N")
    val M = Var("M")
    val L = Var("L")
    val f = fun(
      ArrayType(ArrayType(TupleType(Int,Int), N), M),  // matrix
      ArrayType(TupleType(Int,Int), L),  // vector
      (matrix,vector) => {
        MapGlb(fun(row => 
          toGlobal(MapSeq(intId)) o ReduceSeq(sum,0) o Join() o MapSeq( //map across the first list
            fun(aElem =>
              toGlobal(MapSeq(intId)) o ReduceSeq(matchIndicies, 0) o //reduce to find ones which are "true"
                MapSeq( //map across the second list
                  fun(bElem => eqMult(aElem, bElem)) //produce a new array of equational values
                ) $ vector //map across the second list
            )
          ) $ row //map across the first list
        )) $ matrix
      }
    )

    val (output:Array[Int], runtime) = Execute(1,matrix.length)(f, matrix.map(flattenTupleArray), flattenTupleArray(vector))
    println("Time: "+runtime)
    println("Gold sum = "+gold.sum.toString)
    println("Output sum = "+output.sum.toString)
    assertArrayEquals(output, gold)
  }

  // Ignore this test currently segfaulting.
  @Ignore @Test def LSEARCH_MATRIX_VECTOR() {
    val sum = UserFun("sum", Array("acc","v"),
      "return (acc+v);",
      Seq(Int,Int), Int
    )

    val tupleMult = UserFun("mult", Array("a", "b"), 
      "return (a._1 * b._1);", 
      Seq(TupleType(Int, Int), TupleType(Int, Int)),
      Int
    )

    val compare = UserFun("comp", Array("current_index", "search_index"),
    """
      |if(search_index._0 < current_index._0) {
      | if(search_index._0 == -1) {
      |   // search index is -1, need to move right
      |   // this should never happen
      |   return 1;
      | } else {
      |   // search index less, move left
      |   return -1;
      | }
      |} else if(current_index._0 < search_index._0) {
      | if(current_index._0 == -1) {
      |   // current index is -1, we should move it left
      |   return -1;
      | }else{
      |   // current index is less, we need to move right
      |   return 1;
      | }
      |} else {
      | // the indicies match - don't move!
      | return 0;
      |}
    """.stripMargin,
      Array(TupleType(Int, Int), TupleType(Int, Int)),
      Int // The direction we should move...
    )

    val intId = UserFun("intId", "i", "return i;", Int,Int)

    val width = 100
    val height = 100
    val paddedWidth = 4
    val concreteVectLen = 500

    // val vector = generateSparseArray(concreteVectLen, width)
    val vector = Array.tabulate(width+5)((i) => (i, util.Random.nextInt(10)))
    val matrix = generateSparseMatrix(width, height, paddedWidth)
    val gold = sparseMatrixVector(matrix, vector)

    val N = Var("N")
    val M = Var("M")
    val L = Var("L")
    val f = fun(
      ArrayType(ArrayType(TupleType(Int,Int), N), M),  // matrix
      ArrayType(TupleType(Int,Int), L),  // vector
      (mat,vect) => {
        MapSeq(fun(row => 
          toGlobal(MapSeq(intId)) o ReduceSeq(sum,0) o Join() o MapSeq( //map across the first list
            fun(rowElem =>
              MapSeq(
                fun((res) => tupleMult(res, rowElem))
              ) o LSearch(
                (fun((vectElem) => compare.apply(vectElem, rowElem))), (0, 0)
              ) $ vect
            )
          ) $ row //map across the first list
        )) $ mat
      }
    )
    val (output:Array[Int], runtime) = Execute(1,1)(f, matrix.map(flattenTupleArray), flattenTupleArray(vector))
    println("Time: "+runtime)
    println("Gold sum = "+gold.sum.toString)
    println("Output sum = "+output.sum.toString)
    assertArrayEquals(output, gold)
    assert(false)
  }

  @Ignore @Test def REDUCE_SEARCH_DOT_PRODUCT(){
    val sum = UserFun("sum", Array("acc","v"),
      "return (acc+v);",
      Seq(Int,Int), Int
    )

    val matchIndicies = UserFun("matchIndicies", Array("acc", "v"),
      "{return (v != 0)?v:acc;}",
      Seq(Int,Int), Int
    )

    val eqMult = UserFun("eqMult", Array("a","b"),
      """
        |if(a._0 == b._0){
        |  return (a._1)*(b._1);
        |  printf("Matching!\n");
        |  printf("Returning %d\n", (a._1)*(b._1));
        |}else{
        |  return 0;
        |}
      """.stripMargin,
      Array(TupleType(Int,Int),TupleType(Int,Int)), Int
    )

    val intId = UserFun("intId", "i", "return i;", Int,Int)

    val vectA = generateSparseArray(120, 1000)
    val vectB = generateSparseArray(100, 1100)
    val gold = sparseDotProduct(vectA, vectB)

    val N = Var("N")
    val M = Var("M")
    val f = fun(
      ArrayType(TupleType(Int,Int), N),  //first list
      ArrayType(TupleType(Int,Int), M),  //second list
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

    val (output:Array[Int], runtime) = Execute(1,1)(f, flattenTupleArray(vectA), flattenTupleArray(vectB))
    println("Time: "+runtime)
    println("Gold = "+gold)
    println("output = "+output.deep.mkString(" "))
    assert(output(0) == gold)
  }

  @Ignore @Test def LSEARCH_DOT_PRODUCT() {
    val sum = UserFun("sum", Array("acc","v"),
      "return (acc+v);",
      Seq(Int,Int), Int
    )

    val tupleMult = UserFun("mult", Array("a", "b"), 
      "return (a._1 * b._1);", 
      Seq(TupleType(Int, Int), TupleType(Int, Int)),
      Int
    )

    val compare = UserFun("comp", Array("current_index", "search_index"),
    """
      |if(search_index._0 < current_index._0) {
      | if(search_index._0 == -1) {
      |   // search index is -1, need to move right
      |   // this should never happen
      |   return 1;
      | } else {
      |   // search index less, move left
      |   return -1;
      | }
      |} else if(current_index._0 < search_index._0) {
      | if(current_index._0 == -1) {
      |   // current index is -1, we should move it left
      |   return -1;
      | }else{
      |   // current index is less, we need to move right
      |   return 1;
      | }
      |} else {
      | // the indicies match - don't move!
      | return 0;
      |}
    """.stripMargin,
      Array(TupleType(Int, Int), TupleType(Int, Int)),
      Int // The direction we should move...
    )

    val intId = UserFun("intId", "i", "return i;", Int,Int)

    val vectA = generateSparseArray(120, 1000)
    val vectB = generateSparseArray(100, 1100)
    val gold = sparseDotProduct(vectA, vectB)

    val N = Var("N")
    val M = Var("M")
    val f = fun(
      ArrayType(TupleType(Int,Int), N),  //first list
      ArrayType(TupleType(Int,Int), M),  //second list
      (A,B) => {
        toGlobal(MapSeq(intId)) o ReduceSeq(sum,0) o Join() o MapSeq( //map across the first list
          fun(aElem =>
            MapSeq(
              fun((res) => tupleMult(res, aElem))
            ) o LSearch(
              (fun((bElem) => compare.apply(bElem, aElem))), (0, 0)
            ) $ B
          )
        ) $ A //map across the first list
      }
    )

    val (output:Array[Int], runtime) = Execute(1,1)(f, flattenTupleArray(vectA), flattenTupleArray(vectB))
    println("Time: "+runtime)
    println("Gold = "+gold)
    println("output = "+output.deep.mkString(" "))
    assert(output(0) == gold)
  }

  @Ignore @Test def BSEARCH_DOT_PRODUCT() {
    val sum = UserFun("sum", Array("acc","v"),
      "return (acc+v);",
      Seq(Int,Int), Int
    )

    val tupleMult = UserFun("mult", Array("a", "b"), 
      "return (a._1 * b._1);", 
      Seq(TupleType(Int, Int), TupleType(Int, Int)),
      Int
    )

    val compare = UserFun("comp", Array("current_index", "search_index"),
    """
      |if(search_index._0 < current_index._0) {
      | if(search_index._0 == -1) {
      |   // search index is -1, need to move right
      |   // this should never happen
      |   return 1;
      | } else {
      |   // search index less, move left
      |   return -1;
      | }
      |} else if(current_index._0 < search_index._0) {
      | if(current_index._0 == -1) {
      |   // current index is -1, we should move it left
      |   return -1;
      | }else{
      |   // current index is less, we need to move right
      |   return 1;
      | }
      |} else {
      | // the indicies match - don't move!
      | return 0;
      |}
    """.stripMargin,
      Array(TupleType(Int, Int), TupleType(Int, Int)),
      Int // The direction we should move...
    )

    val intId = UserFun("intId", "i", "return i;", Int,Int)

    val vectA = generateSparseArray(120, 1000)
    val vectB = generateSparseArray(100, 1100)
    val gold = sparseDotProduct(vectA, vectB)

    val N = Var("N")
    val M = Var("M")
    val f = fun(
      ArrayType(TupleType(Int,Int), N),  //first list
      ArrayType(TupleType(Int,Int), M),  //second list
      (A,B) => {
        toGlobal(MapSeq(intId)) o ReduceSeq(sum,0) o Join() o MapSeq( //map across the first list
          fun(aElem =>
            MapSeq(
              fun((res) => tupleMult(res, aElem))
            ) o BSearch(
              (fun((bElem) => compare.apply(bElem, aElem))), (0, 0)
            ) $ B
          )
        ) $ A //map across the first list
      }
    )

    val (output:Array[Int], runtime) = Execute(1,1)(f, flattenTupleArray(vectA), flattenTupleArray(vectB))
    println("Time: "+runtime)
    println("Gold = "+gold)
    println("output = "+output.deep.mkString(" "))
    assert(output(0) == gold)
  }
  /*
   * Negate a sparse vector. Special case of scalar multiplication
   */
  @Ignore @Test def SPARSE_VECTOR_NEGATION(){
    val concreteLength = Math.pow(2, 5).toInt
    val abstractLength = Math.pow(2, 5).toInt
    val rawVector = generateSparseArray(concreteLength,abstractLength)
    val inputVector = rawVector.map((t) => Array(t._1, t._2)).flatten
    val inputSize = inputVector.length/2

    val gold = rawVector.map((vi) => (vi._1, -vi._2)).map((t) => Array(t._1, t._2)).flatten
    val negElem = UserFun("negElem", "x", "{ x._0 = x._0; x._1 = -(x._1); return x; }",
      TupleType(Int, Int), TupleType(Int, Int))

    val f = fun(ArrayType(TupleType(Int, Int), Var("N")), (input) =>
       MapGlb(negElem)  $ input
    )
    val (output:Array[Int], runtime) = Execute(1,1)(f, inputVector)

    println("output(0) = " + output(0))
    println("Input: "+inputVector.toList)
    println("Output: "+output.toList)
    println("Gold: "+gold.toList)
    println("runtime = " + runtime)
    assertArrayEquals(output, gold)
  }

  /*
   * Multiply a sparse vector by a scalar
   */
  @Ignore @Test def SPARSE_VECTOR_SCAL() {
    val concreteLength = Math.pow(2, 5).toInt
    val abstractLength = Math.pow(2, 5).toInt
    val rawVector = generateSparseArray(concreteLength, abstractLength)
    val inputVector = rawVector.map((t) => Array(t._1, t._2)).flatten.map((x)=>x.toFloat)
    val inputSize = rawVector.length
    val alpha = Array(2.5f)
    val gold = flattenTupleArray(rawVector.map{case (index, data) => (index.toFloat, data * alpha(0))}) : Array[Float]

    val sparseElemMult = UserFun("sem", Array("x","s"), "{ x._0 = x._0; x._1 = s*x._1; return x; }",
      Seq(TupleType(Float, Float),Float), TupleType(Float, Float))

    val scalFun = fun(
      ArrayType(TupleType(Float,Float), Var("N")),
      ArrayType(Float, 1), (input, alpha) =>
      MapSeq(
        fun(x =>
          MapGlb(
            fun(s => sparseElemMult(x, s))
          ) $ alpha
        )
      ) $ input
    )

    val (output:Array[Float], runtime) = Execute(1,1)(scalFun, inputVector, alpha)

    println("output(0) = " + output(0))
    println("input = " + inputVector.toList)
    println("gold = " + gold.toList)
    println("output = " + output.toList)
    println("runtime = " + runtime)
    assertArrayEquals(output, gold, 0.0f)
  }

  @Ignore @Test def SPARSE_MATRIX_GENERATION() : Unit = {
    val width = 1000
    val height = 1000
    val spElemPerRow = 10
    println("Generating matrix")
    val arr = generateSparseMatrix(width, height, spElemPerRow)
    println("Generated matrix")
    arr.foreach((row) => println(row.deep.mkString(", ")))
  }

  def flattenTupleArray[T:ClassTag](arr: Array[(T, T)]) : Array[T] = {
    arr.map{case (a,b) => Array(a, b)}.flatten
  }

  def sparseMatrixVector(matrix: Array[Array[(Int, Int)]], vector: Array[(Int, Int)]) : Array[Int] = {
    matrix.par.map((row) => sparseDotProduct(row, vector)).toArray
  }

  def sparseDotProduct(arrA:Array[(Int,Int)], arrB:Array[(Int,Int)]) : Int = {
    def findCorresponding(item: Int, list: Array[(Int, Int)]): Int ={
      list.foldLeft(0)((acc, e) => {
        if(e._1 == item){
          e._2
        }else{
          acc
        }
      })
    }
    arrA.map((elem) => findCorresponding(elem._1, arrB) * elem._2).sum
  }

  // slow, but produces better random values
  def generateSparseArray(concreteLength:Int, abstractLength: Int) : Array[(Int,Int)] = {
    var numSet: Set[Int] = Set()
    while(numSet.size < concreteLength){
      numSet += util.Random.nextInt(abstractLength)
    }
    var outArray = numSet.toArray
    return outArray.sorted.map((x) => (x, util.Random.nextInt(5)))
  }

  // generates an array of exactly concreteLength elements, in the range [0, abstractLength-1]
  def generateSparseIndicies(concreteLength: Int, abstractLength: Int, padVal: Int) : Array[Int] = {
    var m = 0
    var a = 0
    var g = Array.tabulate(concreteLength)(_ => 0.toInt)
    for( i <- 0 to abstractLength-1){
      a = util.Random.nextInt(abstractLength)
      if (a < concreteLength - m) {
        g(m) = i
        m = m + 1
      }
    }
    Array.concat(Array(g.head), g.tail.map((i:Int) => {
      if (i == 0){
        padVal
      }else{
        i
      }
    }))
  }

  def generateSparseMatrix(width: Int, height: Int, paddedWidth: Int) : Array[Array[(Int, Int)]] = {
    // generate an array of integers, giving the max element and the number of non zero elements:
    // var rowLengths = Array.tabulate(height)( _ => util.Random.nextInt(paddedWidth))
    // println(rowLengths.deep.mkString(", "))
    // use that data to generate an array of sparse arrays
    // var index = 0
    Array.tabulate(height)(_ => 0).par.map {_ =>
      generateSparseIndicies(paddedWidth, width, -1).map((i) => {
        if(i == -1){
          (i, i)
        }else{
          (i, util.Random.nextInt(5))
        }
      })
    }.toArray
  }
}
