package ir


import apart.arithmetic.SizeVar
import ir.ast.{Value, Zip, fun}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal, toPrivate}
import org.junit.Assert._
import org.junit.Test

class TestMemory {

  @Test
  def mapSeqId(): Unit = {
    val msid = MapSeq(id)
    val lambda = fun(ArrayType(Float, 16), (A) => msid $ A)
    TypeChecker(lambda)
    OpenCLAddressSpace.setAddressSpace(lambda)

    assertEquals(1, lambda.body.addressSpaces.size)
    assertEquals(GlobalMemory, lambda.body.addressSpaces.head)
  }

  @Test(expected = classOf[UnexpectedAddressSpaceException])
  def mapSeqReturnPrivate(): Unit = {
    val msidGlbToPrv = MapSeq(id)
    val lambda = fun(ArrayType(Float, 16), (A) => toPrivate(msidGlbToPrv) $ A)
    TypeChecker(lambda)
    OpenCLAddressSpace.setAddressSpace(lambda)
  }

  @Test
  def mapSeqPrivateGlobal(): Unit = {
    val msidPrvToGlb = MapSeq(id)
    val msidGlbToPrv = MapSeq(id)
    val lambda = fun(ArrayType(Float, 16), (A) =>
      toGlobal(msidPrvToGlb) o toPrivate(msidGlbToPrv) $ A)

    TypeChecker(lambda)
    OpenCLAddressSpace.setAddressSpace(lambda)

    assertEquals(1, lambda.body.addressSpaces.size)
    assertEquals(GlobalMemory, lambda.body.addressSpaces.head)

    assertEquals(1, msidGlbToPrv.f.body.addressSpaces.size)
    assertEquals(PrivateMemory, msidGlbToPrv.f.body.addressSpaces.head)

    assertEquals(1, msidGlbToPrv.f.params(0).addressSpaces.size)
    assertEquals(GlobalMemory, msidGlbToPrv.f.params(0).addressSpaces.head)

    assertEquals(1, msidPrvToGlb.f.body.addressSpaces.size)
    assertEquals(GlobalMemory, msidPrvToGlb.f.body.addressSpaces.head)

    assertEquals(1, msidPrvToGlb.f.params(0).addressSpaces.size)
    assertEquals(PrivateMemory, msidPrvToGlb.f.params(0).addressSpaces.head)
  }

  @Test
  def test(): Unit = {
    val uf = MapSeq(plusOne)
    val f = fun(
      ArrayType(ArrayType(Float, 4), SizeVar("N")),
      input => toGlobal(MapSeq(MapSeq(id))) o
        ReduceSeq(fun((acc, elem) => MapSeq(add) o fun(elem => Zip(acc, uf $ elem)) $ elem),
          Value(0.0f, ArrayType(Float, 4))) $ input
    )
    TypeChecker(f)
    OpenCLAddressSpace.setAddressSpace(f)

    assertEquals(1, uf.f.body.addressSpaces.size)
    assertEquals(PrivateMemory, uf.f.body.addressSpaces.head)

  }


  @Test
  def testPrivateArray(): Unit = {
    val uf = MapSeq(plusOne)

    val f = fun(
      ArrayType(ArrayType(Float, 4), SizeVar("N")),
      input => toGlobal(MapSeq(MapSeq(id))) o
        ReduceSeq(fun((acc, elem) => MapSeq(add) o fun(elem => Zip(acc, uf $ elem)) $ elem),
          Value(0.0f, ArrayType(Float, 4))) $ input
    )
    TypeChecker(f)
    OpenCLAddressSpace.setAddressSpace(f)

    assertEquals(1, uf.f.body.addressSpaces.size)
    assertEquals(PrivateMemory, uf.f.body.addressSpaces.head)

  }


}
