package opencl.ir.pattern

import ir.ArrayTypeWSWC
import ir.ast.{Join, Slide, Split, Unslide, fun}
import opencl.executor.Compile
import opencl.generator.NDRange
import opencl.ir.{Float, id, idF4}
import org.junit.Test

class TestMapSeqVector {
  @Test
  def t0_naive(): Unit = {
    val N = 9
    val vectorLen = 4

    val f = fun(
      ArrayTypeWSWC( Float, N), input =>
        MapSeqVector(idF4, id, vectorLen) $
          input)

    val kernel = Compile(f, NDRange(1), NDRange(1))
    println(kernel)
  }

  @Test
  def t1_slided(): Unit = {
    val N = 10 // 10 - 2 = 8 windows = 4 tiles * 2 windows
    val wSize = 3
    val wStep = 1
    val vectorLen = 4

    val f = fun(
      ArrayTypeWSWC( Float, N),
      input =>
        Join() o MapSeq(
          MapSeq(MapSeq(toGlobal(id))) o Slide(wSize, wStep) o

            toPrivate(MapSeqVector(idF4, id, vectorLen)) o

            Unslide(wSize, wStep)) o

          Split(2) o // Tile by 2 windows
          Slide(wSize, wStep) $ input)

    val kernel = Compile(f, NDRange(1), NDRange(1))
    println(kernel)
  }
}
