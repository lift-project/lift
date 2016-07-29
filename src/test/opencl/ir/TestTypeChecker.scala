package opencl.ir

import apart.arithmetic._
import ir._
import ir.ast._
import opencl.ir.pattern._
import org.junit.Test

class TestTypeChecker {

  private val K = SizeVar("K")

  @Test(expected = classOf[TypeException])
  def asScalarOn2DArray(): Unit = {
    val lambda = fun(
      ArrayType(ArrayType(Float4, K), K),
      a => asScalar() $ a
    )

    TypeChecker(lambda)
  }

  def asScalar1DArray(): Unit = {
    val lambda = fun(
      ArrayType(ArrayType(Float4, K), K),
      a => Map(asScalar()) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def asScalar1DScalarArray(): Unit = {
    val lambda = fun(
      ArrayType(ArrayType(Float, K), K),
      a => Map(asScalar()) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def asVector2DArray(): Unit = {
    val lambda = fun(
      ArrayType(ArrayType(Float, K), K),
      a => asVector(4) $ a
    )

    TypeChecker(lambda)
  }

  def asVector1DArray(): Unit = {
    val lambda = fun(
      ArrayType(ArrayType(Float, K), K),
      a => Map(asVector(4)) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def asVector1DVectorArray(): Unit = {
    val lambda = fun(
      ArrayType(ArrayType(Float4, K), K),
      a => Map(asVector(4)) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def incorrectReduceSeq(): Unit = {
    val lambda = fun(
      ArrayType(Float, K),
      a => ReduceSeq(
        fun((acc, x) => MapSeq(fun(a => add(acc, a))) $ x),
        0.0f) o Split(1) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def incorrectReduceCustomisingFunctionIssue74(): Unit = {
    val lambda = fun(
      ArrayType(Float, K),
      input =>
        Reduce(fun((acc, next) =>
          multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(input, input)
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def incorrectZip(): Unit = {
    val lambda = fun(
      ArrayType(Float, K),
      ArrayType(Float, 8),
      (a, b) =>
        Zip(a,b)
    )

    TypeChecker(lambda)
  }

  @Test
  def unboundedArrayMap(): Unit = {

    val len = PosVar("len")

    val lambda = fun(
      UnknownLengthArrayType(Float, len),
      a => MapSeq(id) $ a
    )

    val t = TypeChecker(lambda)
    t match {
      case UnknownLengthArrayType(Float, l) =>
        assert (l == len)
      case _ => assert(false)
    }
  }

  @Test
  def unboundedArrayReduce(): Unit = {

    val lambda = fun(
      UnknownLengthArrayType(Float),
      a => ReduceSeq(fun((acc, x) => add(acc, x)), 0.0f) $ a
    )

    val t = TypeChecker(lambda)
    t match {
      case ArrayType(Float, Cst(1)) =>
      case _ => assert(false)
    }
  }
}
