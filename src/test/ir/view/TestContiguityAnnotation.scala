package ir.view

import ir.ast.{Expr, FunCall, Join, Lambda, Split, Transpose, fun}
import ir.{ArrayType, ArrayTypeWSWC, TypeChecker}
import opencl.generator.{NDRange, RangesAndCounts}
import opencl.ir.pattern.MapSeq
import opencl.ir.{Float, InferOpenCLAddressSpace, OpenCLMemoryAllocator, RemoveRedundantMemory, id}
import org.junit.Test
import lift.arithmetic.ArithExpr._
import lift.arithmetic.Cst
import opencl.executor.Compile

class TestContiguityAnnotation {

  def compilePartially(f: Lambda, localSize: NDRange, globalSize: NDRange): Unit = {
    TypeChecker(f)
    InferOpenCLAddressSpace(f)
    RangesAndCounts(f, localSize, globalSize, collection.Map())
    OpenCLMemoryAllocator(f)
    RemoveRedundantMemory(f)
    View(f)
  }

  @Test
  def t0_naive(): Unit = {
    val N = 512
    val M = 256

    val f = fun(
      ArrayTypeWSWC( ArrayTypeWSWC(Float, M), N),
      input => MapSeq(MapSeq(id)) $ input)

    println(Compile(f, NDRange(1), NDRange(1)))

    val expr = Expr.visitWithState[Option[Expr]](None)(f.body, {
      case (_, Some(e)) => Some(e)
      case (FunCall(MapSeq(_), arg), _) => Some(arg)
      case (_, s) => s
    }).get

    assert(View.isContiguous(expr.view))
  }

  @Test
  def t1_transposed(): Unit = {
    val N = 512
    val M = 256

    val f = fun(
      ArrayTypeWSWC( ArrayTypeWSWC(Float, M), N),
      input => MapSeq(MapSeq(id)) o Transpose() $ input)

    compilePartially(f, NDRange(1), NDRange(1))

    val expr = Expr.visitWithState[Option[Expr]](None)(f.body, {
      case (_, Some(e)) => Some(e)
      case (FunCall(MapSeq(_), arg), _) => Some(arg)
      case (_, s) => s
    }).get

    assert(!View.isContiguous(expr.view))
  }

  @Test
  def t1_2_transposedTransposed(): Unit = {
    val N = 512
    val M = 256

    val f = fun(
      ArrayTypeWSWC( ArrayTypeWSWC(Float, M), N),
      input => MapSeq(MapSeq(id)) o Transpose() o Transpose() $ input)

//    compilePartially(f, NDRange(1), NDRange(1))
    println(Compile(f, NDRange(1), NDRange(1)))

    val expr = Expr.visitWithState[Option[Expr]](None)(f.body, {
      case (_, Some(e)) => Some(e)
      case (FunCall(MapSeq(_), arg), _) => Some(arg)
      case (_, s) => s
    }).get

    assert(View.isContiguous(expr.view))
  }

  @Test
  def t2_transposeJoinSplitN(): Unit = {
    val N = 512
    val M = 256

    val f = fun(
      ArrayTypeWSWC( ArrayTypeWSWC(Float, M), N),
      input => MapSeq(MapSeq(id)) o Split(N) o Join() o Transpose() $ input)

    //    compilePartially(f, NDRange(1), NDRange(1)
    println(Compile(f, NDRange(1), NDRange(1)))

    val expr = Expr.visitWithState[Option[Expr]](None)(f.body, {
      case (_, Some(e)) => Some(e)
      case (FunCall(MapSeq(_), arg), _) => Some(arg)
      case (_, s) => s
    }).get
    assert(!View.isContiguous(expr.view))
  }

  @Test
  def t2_2_transposeJoinSplitNTranspose(): Unit = {
    val N = 512
    val M = 256

    val f = fun(
      ArrayTypeWSWC( ArrayTypeWSWC(Float, M), N),
      input => MapSeq(MapSeq(id)) o Transpose() o Split(N) o Join() o Transpose() $ input)

    //    compilePartially(f, NDRange(1), NDRange(1)
    println(Compile(f, NDRange(1), NDRange(1)))

    val expr = Expr.visitWithState[Option[Expr]](None)(f.body, {
      case (_, Some(e)) => Some(e)
      case (FunCall(MapSeq(_), arg), _) => Some(arg)
      case (_, s) => s
    }).get
    assert(View.isContiguous(expr.view))
  }

  @Test
  def t3_transposeJoinSplitM(): Unit = {
    val N = 512
    val M = 256
    val f = fun(
      ArrayTypeWSWC( ArrayTypeWSWC(Float, M), N),
      input => MapSeq(MapSeq(id)) o Split(M) o Join() o Transpose() $ input)

//    compilePartially(f, NDRange(1), NDRange(1)
    println(Compile(f, NDRange(1), NDRange(1)))

    val expr = Expr.visitWithState[Option[Expr]](None)(f.body, {
      case (_, Some(e)) => Some(e)
      case (FunCall(MapSeq(_), arg), _) => Some(arg)
      case (_, s) => s
    }).get

    assert(!View.isContiguous(expr.view))
  }
}
