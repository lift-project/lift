package opencl.generator

import ir.{ArrayType, ArrayTypeWSWC, TupleType, TypeChecker}
import ir.ast.{FunCall, Gather, Join, Lambda1, Lambda2, Param, Split, TransposeW, Zip, fun, transposeFunction}
import ir.view.{InferPtrOutType, ItSpaceDimCount}
import lift.arithmetic.SizeVar
import opencl.executor.Compile
import opencl.ir.pattern.{MapGlb, MapSeq}
import opencl.ir.{Float, id, tf_id}
import org.junit.Test
import org.junit.Assert._


class TestPtrOutType {


  @Test def zip(): Unit = {

    val N = SizeVar("N")

    val p = Param(TupleType(Float,Float))
    val tfIdFunCall = FunCall(tf_id, p)

    val p0 = Param(Float)
    val p1 = Param(Float)
    val idFC0 = FunCall(id, p0)
    val idFC1 = FunCall(id, p1)

    val f = fun(
      ArrayTypeWSWC(Float,N), ArrayTypeWSWC(Float,N),
      (in1, in2) => {
        MapSeq(new Lambda1(Array(p), tfIdFunCall)) $ Zip(MapSeq(new Lambda1(Array(p0),idFC0)) $ in1,MapSeq(new Lambda1(Array(p1),idFC1)) $ in2)
      })

    Compile(f)

    assertEquals(ArrayType(TupleType(Float,Float), N), tfIdFunCall.outPtrType)
    assertEquals(ArrayType(Float, N), idFC0.outPtrType)
    assertEquals(ArrayType(Float, N), idFC1.outPtrType)
  }

  @Test def transposeW(): Unit = {

    val M = SizeVar("M")
    val N = SizeVar("N")

    val p0 = Param(Float)
    val idFC0 = FunCall(id, p0)
    val idLambda0 = new Lambda1(Array(p0), idFC0)

    val f1 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float,N),M),
      (in) => {
        TransposeW() o MapSeq(MapSeq(idLambda0)) $ in
      })
    Compile(f1)
    assertEquals(ArrayTypeWSWC(ArrayTypeWSWC(Float,M),N), idFC0.outPtrType)

    val f2 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float,N),M),
      (in) => {
        TransposeW() o TransposeW() o MapSeq(MapSeq(idLambda0)) $ in
      })
    Compile(f2)
    assertEquals(ArrayTypeWSWC(ArrayTypeWSWC(Float,N),M), idFC0.outPtrType)

    val f3 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float,N),M),
      (in) => {
        Split(2) o MapSeq(MapSeq(idLambda0)) $ in
      })
    Compile(f3)
    assertEquals(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float,N),2),M/^2), idFC0.outPtrType)

    val f4 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float,N),M),
      (in) => {
        TransposeW() o Split(2) o MapSeq(MapSeq(idLambda0)) $ in
      })
    TypeChecker(f4)
    val itDimNum = ItSpaceDimCount(f4.body)
    InferPtrOutType(f4.body, itDimNum)

    //Compile(f4)
    assertEquals(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float,N),M/^2),2), idFC0.outPtrType)
  }





}
