package opencl.generator

import ir.ArrayTypeWSWC
import ir.ast.{Slide, fun}
import opencl.executor.TestWithExecutor
import opencl.ir._
import opencl.ir.pattern.{MapGlb, toGlobal}
import org.junit.Assert._
import org.junit._


object TestUnrollAndInlineValues extends TestWithExecutor

class TestUnrollAndInlineValues
{

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
  def testPrivateArrayOfStructs(): Unit =
  {
      /* Arr[Tuple(float,int)] */

    val n = 10

    val lambda1DOriginal = fun(
      (ArrayTypeWSWC(Float, n)),
      (m) =>
        MapGlb(0)(fun(m => {
          toGlobal( tf_id )$ m.at(1)
        })) o Slide(3,1) $ m)


  }

  @Test
  def testPrivateArrayOfStructsOfStructs(): Unit =
  {
    /* Arr[Tuple(float,Tuple(double,double))] */

  }

  @Test
  def testPrivateArrayOfStructsOfPrivateArrays(): Unit =
  {
    /* Arr[Tuple(int,Arr[float])] */

  }

  @Test
  def testStructOfPrivateArrays(): Unit =
  {
    /* Tuple(Arr[float],int) */

  }

  @Test
  def testStructOfPrivateArraysOfPrivateArrays(): Unit =
  {
    /* Tuple(Arr[Arr[float]],int) */

  }

  @Test
  def testStructOfPrivateArraysOfStructs(): Unit =
  {
    /* Tuple(Arr[Tuple(int,int)],float) */

  }

}
