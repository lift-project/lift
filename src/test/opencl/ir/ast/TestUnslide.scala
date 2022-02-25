package opencl.ir.ast

import ir.ArrayTypeWSWC
import ir.ast.{Join, Slide, Split, Unslide, fun}
import lift.arithmetic.{ContinuousRange, Var}
import opencl.executor.Compile
import opencl.generator.NDRange
import opencl.ir.{Float, id, idF4}
import opencl.ir.pattern.{MapSeq, MapSeqVector, toGlobal, toPrivate}
import org.junit.Test

class TestUnslide {

  @Test
  def t0_SlideUnslide(): Unit = {
    val N = 10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = Var("wSize", ContinuousRange(3, 7))//3
    val wStep = Var("wStep", ContinuousRange(1, 3)) //1

    val fNaive = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq(toGlobal(id)) $
          input)

    val fSlideUnslide = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq(toGlobal(id)) o
          Unslide(wSize, wStep) o Slide(wSize, wStep) $ input)

    val kernelNaive = Compile(fNaive, NDRange(1), NDRange(1))
    val kernelSlideUnslide = Compile(fSlideUnslide, NDRange(1), NDRange(1))
    println(kernelNaive)
    println(kernelSlideUnslide)
  }

  @Test
  def t1_UnslideSlideTiled(): Unit = {
    val N = 10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = Var("wSize", ContinuousRange(3, 7))//3
    val wStep = Var("wStep", ContinuousRange(1, 3)) //1

    val fNaive = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq( MapSeq(MapSeq(toGlobal(id))) ) o
        MapSeq( MapSeq(MapSeq(toGlobal(id))) ) o
          Split(2) o Slide(wSize, wStep) $
          input)

    val fUnslideSlide = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq( MapSeq(MapSeq(toGlobal(id))) ) o
        MapSeq( Slide(wSize, wStep) o MapSeq(toGlobal(id)) o Unslide(wSize, wStep)) o
          Split(2) o Slide(wSize, wStep) $
          input)

    val kernel1 = Compile(fNaive, NDRange(1), NDRange(1))
    val kernel2 = Compile(fUnslideSlide, NDRange(1), NDRange(1))
    println(kernel1)
    println(kernel2)
  }

  @Test
  def t2_SlideUnslideSlide(): Unit = {
    val N = 10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = Var("wSize", ContinuousRange(3, 7))//3
    val wStep = Var("wStep", ContinuousRange(1, 3)) //1

//    val fUnslide = fun(
//      ArrayTypeWSWC( Float, N),
//      input =>
//        MapSeq(toGlobal(id)) o Unslide(wSize, wStep) o
//          MapSeq(MapSeq(toGlobal(id))) o Slide(wSize, wStep) $
//          input)

    val fNaive  = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq(MapSeq(toGlobal(id))) o /* nothing here */
          /*MapSeq(MapSeq(toGlobal(id)))*/ Slide(wSize, wStep) $
          input)

    val fUnslideSlide = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq(MapSeq(toGlobal(id))) o Slide(wSize, wStep) o Unslide(wSize, wStep) o
          /*MapSeq(MapSeq(toGlobal(id))) o */Slide(wSize, wStep) $
          input)

//    val kernel1 = Compile(fUnslide, NDRange(1), NDRange(1))
    val kernel2 = Compile(fNaive, NDRange(1), NDRange(1))
    val kernel3 = Compile(fUnslideSlide, NDRange(1), NDRange(1))
//    println(kernel1)
    println(kernel2)
    println(kernel3)
  }

  @Test
  def t3_UnslideIdSlide(): Unit = {
    val N = 10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = 3//Var("wSize", ContinuousRange(3, 7))//3
    val wStep = 1//Var("wStep", ContinuousRange(1, 3)) //1

    val fNaive = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq(MapSeq(toGlobal(id))) o /* nothing here */
          MapSeq(MapSeq(toGlobal(id))) o Slide(wSize, wStep) $
          input)

    val fUnslideSlide = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        MapSeq(MapSeq(toGlobal(id))) o Slide(wSize, wStep) o MapSeq(toGlobal(id)) o Unslide(wSize, wStep) o
          MapSeq(MapSeq(toGlobal(id))) o Slide(wSize, wStep) $
          input)

    val kernel1 = Compile(fNaive, NDRange(1), NDRange(1))
    val kernel2 = Compile(fUnslideSlide, NDRange(1), NDRange(1))
    println(kernel1)
    println(kernel2)
  }
}
