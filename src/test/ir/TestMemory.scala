package ir


import java.io.{File, PrintWriter, Writer}

import apart.arithmetic.SizeVar
import ir.ast.{Value, Zip, fun}
import ir.printer.DotPrinter
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal, toPrivate}
import org.junit.Test
import org.junit.Assert

/**
  *     assert(msidGlbToPrv.f.params(0).t.addressSpace == GlobalMemory)
@author cdubach
  */
class TestMemory {

  @Test
  def mapSeqId(): Unit = {
    val msid = MapSeq(id)
    val lambda = fun(ArrayType(Float, 16), (A) => msid $ A)
    TypeChecker(lambda)
    OpenCLAddressSpace.setAddressSpace(lambda)
    assert(lambda.body.addressSpaces.size == 1 && lambda.body.addressSpaces.contains(GlobalMemory))
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
    val lambda = fun(ArrayType(Float, 16), (A) => toGlobal(msidPrvToGlb) o toPrivate(msidGlbToPrv) $ A)
    TypeChecker(lambda)
    OpenCLAddressSpace.setAddressSpace(lambda)

    //new DotPrinter(new PrintWriter(new File("/home/cdubach/graph.dot")), false, true).print(lambda)

    assert(lambda.body.addressSpaces.size == 1 && lambda.body.addressSpaces.contains(GlobalMemory))

    assert(msidGlbToPrv.f.body.addressSpaces.size == 1 && msidGlbToPrv.f.body.addressSpaces.contains(PrivateMemory))
    assert(msidGlbToPrv.f.params(0).addressSpaces.size == 1 && msidGlbToPrv.f.params(0).addressSpaces.contains(GlobalMemory))

    assert(msidPrvToGlb.f.body.addressSpaces.size == 1 && msidPrvToGlb.f.body.addressSpaces.contains(GlobalMemory))
    assert(msidPrvToGlb.f.params(0).addressSpaces.size == 1 && msidPrvToGlb.f.params(0).addressSpaces.contains(PrivateMemory))
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
    assert(uf.f.body.addressSpaces.contains(PrivateMemory))

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
    new DotPrinter(new PrintWriter(new File("/home/cdubach/graph.dot")), false, true).print(f)
    assert(uf.f.body.addressSpaces.contains(PrivateMemory))

  }


}
