package opencl.generator

import ir.ast._
import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import opencl.executor.{Compile, Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._


object TestUnrollAndInlineValues extends TestWithExecutor


class TestUnrollAndInlineValues
{
  val delta = 0.00001f

  val N = 4

  val tftii_id = UserFun("nestedtuple_id", "x", "return x;", TupleType(Float, TupleType(Int,Int)), TupleType(Float, TupleType(Int,Int)))

  val tfafN_id = UserFun("tuplewitharray4_id", "x", "return x;", TupleType(Int, ArrayType(Float,N)), TupleType(Int,ArrayType(Float,N)))

  val tfafNN_id = UserFun("tuplewitharray4x4_id", "x", "return x;", TupleType(Int,ArrayType(ArrayType(Float,N),N)), TupleType(Int,ArrayType(ArrayType(Float,N),N)))

  val tfatiif_id = UserFun("tupleofarrayoftuple_id", "x", "return x;", TupleType(ArrayType(TupleType(Int,Int),N),Float),TupleType(ArrayType(TupleType(Int,Int),N),Float))

  def runUnrolledIndexTest(inputString : String, returnIdx : Int, returnSuffix : String) : Unit =
  {
    val unrolled = UnrollValues.getIndexSuffix(inputString)
    assertEquals(unrolled._1, returnIdx)
    assertEquals(unrolled._2, returnSuffix)
  }

  // index tests
  @Test
  def testNoSuffix(): Unit =
  {
      var inputString : String = ""
      var returnIdx : Int = -1
      var returnSuffix : String = ""

      runUnrolledIndexTest(inputString,returnIdx,returnSuffix)

  }

  @Test
  def testSimpleSuffix(): Unit =
  {
    var inputString : String = "_7"
    var returnIdx : Int = 7
    var returnSuffix : String = ""

    runUnrolledIndexTest(inputString,returnIdx,returnSuffix)

  }

  @Test
  def testTuple(): Unit =
  {
    var inputString : String = "._1"
    var returnIdx : Int = 1
    var returnSuffix : String = ""

    runUnrolledIndexTest(inputString,returnIdx,returnSuffix)

  }


  @Test
  def testTupleSuffix(): Unit =
  {
    var inputString : String = "_2._1"
    var returnIdx : Int = 2
    var returnSuffix : String = "._1"

    runUnrolledIndexTest(inputString,returnIdx,returnSuffix)

  }

  @Test
  def testMultiDSuffix(): Unit =
  {
    var inputString : String = "_2_2_5"
    var returnIdx : Int = 2
    var returnSuffix : String = "_2_5"

    runUnrolledIndexTest(inputString,returnIdx,returnSuffix)

  }

  @Test
  def testMultiDSuffixWithTuple(): Unit =
  {
    var inputString : String = "_2_2._1"
    var returnIdx : Int = 2
    var returnSuffix : String = "_2._1"

    runUnrolledIndexTest(inputString,returnIdx,returnSuffix)

  }

  @Test
  def testUnrollPrivateArrayOfStructs(): Unit =
  {
      /* Arr[Tuple(float,int)] */

    val data = Array.tabulate(N) { (i) => (i + 1).toFloat }
    val input = (data zip data)
    val compare = (data zip data).toVector

    val lambda = fun(
      ArrayTypeWSWC(TupleType(Float,Float), N),
      (A) =>
        toGlobal(MapSeq(tf_id)) o toPrivate(MapSeq(tf_id)) $ A)

    println(Compile(lambda))

    val (output, _) = Execute(N,N)[Vector[(Float, Float)]](lambda, input)
    assertEquals(compare, output)

  }

  @Test
  def testUnrollPrivateArrayOfStructsOfStructs(): Unit =
  {
    /* Arr[Tuple(float,Tuple(int,int))] */

    val data = Array.tabulate(N) { (i) => (i + 1).toFloat }
    val input1 = (data zip data)
    val input2 = (data.map(_.toInt) zip data.map(_.toInt))
    val compare = (data zip input2).toVector

    val lambda = fun(
      ArrayTypeWSWC(TupleType(Float,TupleType(Int,Int)), N),
      (A) =>
        toGlobal(MapSeq(tftii_id)) o toPrivate(MapSeq(tftii_id)) $ A)

    println(Compile(lambda))

    //val (output, _) = Execute(N,N)[Vector[(Float, Float)]](lambda, input)
    //assertEquals(compare, output)

  }

  @Test
  def testUnrollPrivateArrayOfStructsOfPrivateArrays(): Unit =
  {
    /* Arr[Tuple(int,Arr[float])] */

    val data = Array.tabulate(N) { (i) => (i + 1).toFloat }
    val data2D = Array.tabulate(N,N) { (i,j) => (i + j).toFloat }
    val input = (data zip data2D)
    val compare = (data zip data2D).toVector

    val lambda = fun(
      ArrayTypeWSWC(TupleType(Int,ArrayTypeWSWC(Float, N)), N),
      (A) =>
        toGlobal(MapSeq(tfafN_id)) o toPrivate(MapSeq(tfafN_id)) $ A)

    println(Compile(lambda))

    //val (output, _) = Execute(N,N)[Vector[(Float, Float)]](lambda, input)
    //assertEquals(compare, output)

  }

  @Test
  def testUnrollStructOfPrivateArraysOfPrivateArrays(): Unit =
  {
    /* Tuple(Arr[Arr[float]],int) */
    val data = Array.tabulate(N) { (i) => (i + 1).toFloat }
    val data3D = Array.tabulate(N,N,N) { (i,j,k) => (i + j + k).toFloat }
    val input = (data zip data3D)
    val compare = (data zip data3D).toVector


    val lambda = fun(
      ArrayTypeWSWC(TupleType(Int,ArrayTypeWSWC(ArrayTypeWSWC(Float, N),N)),N),
      (A) =>
        toGlobal(MapSeq(tfafNN_id)) o toPrivate(MapSeq(tfafNN_id)) $ A)

    println(Compile(lambda))

    //val (output, _) = Execute(N,N)[Vector[(Float, Float)]](lambda, input)
    //assertEquals(compare, output)

  }

  @Test
  def testUnrollStructOfPrivateArraysOfStructs(): Unit =
  {
    /* Tuple(Arr[Tuple(int,int)],float) */

    val data = Array.tabulate(N) { (i) => (i + 1) }
    val dataF = Array.tabulate(N) { (i) => (i + 1).toFloat }
    val tupleArr = ( data zip data )
    val tupleArrFloat = ( tupleArr zip dataF )
    val data3D = Array.tabulate(N,N,N) { (i,j,k) => (i + j + k).toFloat }
    val input = (data zip data3D)
    val compare = (data zip data3D).toVector

    val lambda = fun(
      ArrayTypeWSWC(TupleType(ArrayTypeWSWC(TupleType(Int,Int),N),Float),N),
      (A) =>
        toGlobal(MapSeq(tfatiif_id)) o toPrivate(MapSeq(tfatiif_id)) $ A)

    println(Compile(lambda))

    //val (output, _) = Execute(N,N)[Vector[(Float, Float)]](lambda, input)
    //assertEquals(compare, output)

  }

}
