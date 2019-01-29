package opencl.generator

import ir.ast._
import ir.{ArrayTypeWSWC, TupleType}
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._

object TestUnrollAndInlineValues extends TestWithExecutor

class TestUnrollAndInlineValues {

  val delta = 0.00001f

  val M = 6

  val N = 4

  val O = 8

  val tftii_id = UserFun("nestedtuple_id", "x", "return x;", TupleType(Float, TupleType(Int, Int)), TupleType(Float, TupleType(Int, Int)))

  val tftitff_id = UserFun("nestednestedtuple_id", "x", "return x;", TupleType(Float, TupleType(Int, TupleType(Float, Float))), TupleType(Float, TupleType(Int, TupleType(Float, Float))))

  def runUnrolledIndexTest(inputString: String, returnIdx: Int, returnSuffix: String): Unit = {
    val unrolled = UnrollValues.getIndexSuffix(inputString)
    assertEquals(unrolled._1, returnIdx)
    assertEquals(unrolled._2, returnSuffix)
  }

  // index tests
  @Test
  def testNoSuffix(): Unit = {
    var inputString: String = ""
    var returnIdx: Int = -1
    var returnSuffix: String = ""

    runUnrolledIndexTest(inputString, returnIdx, returnSuffix)

  }

  @Test
  def testSimpleSuffix(): Unit = {
    var inputString: String = "_7"
    var returnIdx: Int = 7
    var returnSuffix: String = ""

    runUnrolledIndexTest(inputString, returnIdx, returnSuffix)

  }

  @Test
  def testTuple(): Unit = {
    var inputString: String = "._1"
    var returnIdx: Int = 1
    var returnSuffix: String = ""

    runUnrolledIndexTest(inputString, returnIdx, returnSuffix)

  }


  @Test
  def testTupleSuffix(): Unit = {
    var inputString: String = "_2._1"
    var returnIdx: Int = 2
    var returnSuffix: String = "._1"

    runUnrolledIndexTest(inputString, returnIdx, returnSuffix)

  }

  @Test
  def testMultiDSuffix(): Unit = {
    var inputString: String = "_2_2_5"
    var returnIdx: Int = 2
    var returnSuffix: String = "_2_5"

    runUnrolledIndexTest(inputString, returnIdx, returnSuffix)

  }

  @Test
  def testMultiDSuffixWithTuple(): Unit = {
    var inputString: String = "_2_2._1"
    var returnIdx: Int = 2
    var returnSuffix: String = "_2._1"

    runUnrolledIndexTest(inputString, returnIdx, returnSuffix)

  }

  @Test
  def testUnrollPrivateArrayOfStructs(): Unit = {
    /* Arr[Tuple(float,int)] */

    val ISflag = InlineStructs()
    InlineStructs(true)

    val data = Array.tabulate(N) { (i) => (i + 1).toFloat }
    val input = (data zip data)
    val compare = (data zip data).toVector

    val lambda = fun(
      ArrayTypeWSWC(TupleType(Float, Float), N),
      (A) =>
        toGlobal(MapSeq(tf_id)) o toPrivate(MapSeq(tf_id)) $ A)

    val (output, _) = Execute(N, N)[Vector[(Float, Float)]](lambda, input)
    assertEquals(compare, output)

    InlineStructs(ISflag)

  }

  @Test
  def testUnrollPrivateArrayOfStructsOfStructs(): Unit = {
    /* Arr[Tuple(float,Tuple(int,int))] */

    val ISflag = InlineStructs()
    InlineStructs(true)

    val data = Array.tabulate(N) { (i) => (i + 1).toFloat }
    val input = (data zip (data.map(_.toInt) zip data.map(_.toInt)))
    val compare = (data zip (data.map(_.toInt) zip data.map(_.toInt))).toVector

    val lambda = fun(
      ArrayTypeWSWC(TupleType(Float, TupleType(Int, Int)), N),
      (A) =>
        toGlobal(MapSeq(tftii_id)) o toPrivate(MapSeq(tftii_id)) $ A)

    val (output, _) = Execute(N, N)[Vector[(Float, (Int, Int))]](lambda, input)
    assertEquals(compare, output)


    InlineStructs(ISflag)

  }

  @Test
  def testUnrollPrivateArrayOfStructsOfStructsOfStructs(): Unit = {

    /* Arr[Tuple(float,Tuple(int,Tuple(float,float)))] */

    val ISflag = InlineStructs()
    InlineStructs(true)

    val data = Array.tabulate(N) { (i) => (i + 1).toFloat }
    val input = (data zip (data.map(_.toInt) zip (data zip data)))
    val compare = input.toVector

    val lambda = fun(
      ArrayTypeWSWC(TupleType(Float, TupleType(Int, TupleType(Float, Float))), N),
      (A) =>
        toGlobal(MapSeq(tftitff_id)) o toPrivate(MapSeq(tftitff_id)) $ A)

    val (output, _) = Execute(N, N)[Vector[(Float, (Int, (Float, Float)))]](lambda, input)
    assertEquals(compare, output)

    InlineStructs(ISflag)

  }

  @Test
  def testUnrollPrivateArraysOfPrivateArrays(): Unit = {

    /* Arr[Arr[float]]) */

    val data = Array.tabulate(M, N) { (i, j) => (i + j + 1).toFloat }
    val gold: Array[Float] = data.flatten


    val lambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      (A) =>
        toGlobal(MapSeq(MapSeq(id))) o toPrivate(MapSeq(MapSeq(id))) $ A)

    val (output, _) = Execute(2, 2)[Array[Float]](lambda, data)
    assertArrayEquals(gold, output, delta)

  }

  @Test
  def testUnrollPrivateArraysOfPrivateArraysOfPrivateArrays(): Unit = {

    /* Arr[Arr[Arr[float]]]) */

    val data = Array.tabulate(M, N, O) { (i, j, k) => (i + j + k + 1).toFloat }
    val gold: Array[Float] = data.flatten.flatten

    val lambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      (A) =>
        toGlobal(MapSeq(MapSeq(MapSeq(id)))) o toPrivate(MapSeq(MapSeq(MapSeq(id)))) $ A)

    val (output, _) = Execute(2, 2)[Array[Float]](lambda, data)
    assertArrayEquals(gold, output, delta)

  }

  @Test
  def testUnrollPrivateArraysOfPrivateArraysOfPrivateArraysofStructs(): Unit = {

    /* Arr[Arr[Arr[Tuple(float, float)]]]) */

    val ISflag = InlineStructs()
    InlineStructs(true)

    val data3D = Array.tabulate(M, N, O) { (i, j, k) => ((i + j + k + 1).toFloat, (i + j + k + 1).toFloat) }
    val gold3D: Vector[(Float, Float)] = data3D.flatten.flatten.toVector

    val lambda3D = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(TupleType(Float, Float), O), N), M),
      (A) =>
        toGlobal(MapSeq(MapSeq(MapSeq(tf_id)))) o toPrivate(MapSeq(MapSeq(MapSeq(tf_id)))) $ A)

    val (output, _) = Execute(2, 2)[Vector[Vector[Vector[(Float, Float)]]]](lambda3D, data3D)
    assertEquals(gold3D, output.flatten.flatten)

    InlineStructs(ISflag)

  }


}
