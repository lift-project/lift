package opencl.ir

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.executor.{Execute, Utils}
import opencl.generator.TreatWarningsAsErrors
import opencl.ir.pattern._
import org.junit.Assume.assumeFalse
import org.junit.Test

class TestTypeChecker {

  private val K = SizeVar("K")

  @Test(expected = classOf[TypeException])
  def asScalarOn2DArray(): Unit = {
    val lambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float4, K), K),
      a => asScalar() $ a
    )

    TypeChecker(lambda)
  }

  def asScalar1DArray(): Unit = {
    val lambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float4, K), K),
      a => Map(asScalar()) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def asScalar1DScalarArray(): Unit = {
    val lambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), K),
      a => Map(asScalar()) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def asVector2DArray(): Unit = {
    val lambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), K),
      a => asVector(4) $ a
    )

    TypeChecker(lambda)
  }

  def asVector1DArray(): Unit = {
    val lambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), K),
      a => Map(asVector(4)) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def asVector1DVectorArray(): Unit = {
    val lambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float4, K), K),
      a => Map(asVector(4)) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def incorrectReduceSeq(): Unit = {
    val lambda = fun(
      ArrayTypeWSWC(Float, K),
      a => ReduceSeq(
        fun((acc, x) => MapSeq(fun(a => add(acc, a))) $ x),
        0.0f) o Split(1) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def incorrectReduceCustomisingFunctionIssue74(): Unit = {
    val lambda = fun(
      ArrayTypeWSWC(Float, K),
      input =>
        Reduce(fun((acc, next) =>
          multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(input, input)
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def incorrectZip(): Unit = {
    val lambda = fun(
      ArrayTypeWSWC(Float, K),
      ArrayTypeWSWC(Float, 8),
      (a, b) =>
        Zip(a,b)
    )

    TypeChecker(lambda)
  }

//  @Test
//  def unboundedArrayMap(): Unit = {
//
//    val len = PosVar("len")
//
//    val lambda = fun(
//      UnknownLengthArrayTypeWSWC(Float, len),
//      a => MapSeq(id) $ a
//    )
//
//    val t = TypeChecker(lambda)
//    t match {
//      case UnknownLengthArrayTypeWSWC(Float, l) =>
//        assert (l == len)
//      case _ => assert(false)
//    }
//  }

  @Test
  def unboundedArrayReduce(): Unit = {

    val lambda = fun(
      RuntimeSizedArrayType(Float),
      a => ReduceSeq(fun((acc, x) => add(acc, x)), 0.0f) $ a
    )

    val t = TypeChecker(lambda)
    t match {
      case ArrayTypeWS(Float, Cst(1)) =>
      case _ => assert(false)
    }
  }

  @Test(expected = classOf[SuspiciousTypeVariableDeclaredException])
  def issue5(): Unit = {
    TreatWarningsAsErrors(true)

    val f = fun(
      Float,
      ArrayTypeWSWC(Float, SizeVar("N")),
      ArrayTypeWSWC(Float, SizeVar("N")),
      (a,xs,ys) => /*MapGlb(/*...*/) $*/ Zip(xs, ys)
    )

    TypeChecker(f)
  }


  @Test def iterativeSlide(): Unit = {

   /* val innerL = fun(
      ArrayTypeWSWC(Float, 6),
      (input) => {
        Join() o Map(ReduceSeq(add, 0.0f)) o Slide(3,1) $ input
      })
    val innerT = TypeChecker(innerL)*/

    val lambda = fun(
      ArrayTypeWSWC(Float, 6),
      (input) => {
        Iterate(2) (Join() o Map(ReduceSeq(add, 0.0f)) o Slide(3,1)) $ input
      })

    val t = TypeChecker(lambda)
    t match {
      case ArrayTypeWSWC(Float, Cst(2), Cst(2)) =>
      case _ => assert(false, "Expect type: "+ArrayTypeWSWC(Float, Cst(2), Cst(2))+" but found: "+t)
    }

  }

}
