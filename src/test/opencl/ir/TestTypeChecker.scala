package opencl.ir

import apart.arithmetic._
import ir._
import ir.ast._
import opencl.ir.pattern._
import org.junit.Test

class TestTypeChecker {

  @Test(expected = classOf[TypeException])
  def incorrectReduceSeq(): Unit = {
    val lambda = fun(
      ArrayType(Float, SizeVar("K")),
      a => ReduceSeq(fun((acc, x) => MapSeq(fun(a => add(acc, a))) $ x), 0.0f) o Split(1) $ a
    )

    TypeChecker(lambda)
  }

  @Test(expected = classOf[TypeException])
  def incorrectZip(): Unit = {
    val lambda = fun(
      ArrayType(Float, SizeVar("K")),
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
