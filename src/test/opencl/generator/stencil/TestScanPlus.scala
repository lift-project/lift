package opencl.generator.stencil

import ir.ArrayType
import ir.ast.{Get, Pad, Slide, Zip, fun}
import lift.arithmetic.{SizeVar, StartFromRange, Var}
import opencl.executor._
import org.junit.{AfterClass, BeforeClass}
import org.junit.Assert._
import org.junit._
import opencl.ir.pattern._
import ir.ast._
import ir.{ArrayType, TupleType}
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._

object TestScanPlus
{
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}
class TestScanPlus
{

  @Ignore
  @Test
  def reduceSlide1DTest(): Unit = {

    val size = 8
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(3,1).toArray.map(x => x.reduceLeft(_ + _))


    val stencil = fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        MapGlb(
          toGlobal(MapSeqUnroll(id)) o
           // ReduceSeq(fun((acc, y) => {
              ScanPlus(fun((acc, y) => {
              absAndSumUp.apply(acc, y)
            }), 0.0f))
      } o Slide(3, 1)  $ input
    )

//    println(Compile(stencil))

    val (output: Array[Float], _) = Execute(2,2)(stencil, values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(output)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestWithWeights(): Unit = {

    val size = 8
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val weights = Array( 0.5f, 1.0f, 0.5f )
    val gold = Array( 4.0f,6.0f,8.0f,10.0f,12.0f,14.0f ) //values.sliding(3,1).toArray.map(x => x.reduceLeft(0.5f*_ + 0.5f*_))


    val stencil = fun(
      ArrayType(Float, SizeVar("N")),
      ArrayType(Float, 3),
      (input,wgts) => {
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(add, 0.0f) o
              MapSeqUnroll(mult) $
              Zip(wgts, neighbourhood)
          })
        )} o Slide(3, 1)  $ input
    )

    val (output: Array[Float], _) = Execute(2,2)(stencil, values, weights)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArray(gold)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide2DTest(): Unit = {

    val size = 8
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }

    val firstSlide = values.sliding(3,1).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(3,1).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten


    val stencil = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      (mat) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeq(add, 0.0f) o Join() $ neighbours
          }))
        ) o Slide2D(3,1) $ mat
      })

    val (output: Array[Float], _) = Execute(2,2)(stencil, values)

    StencilUtilities.print2DArray(values)
    StencilUtilities.print1DArrayAs2DArray(output,size-2)
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArray(gold)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide2DTestWithWeights(): Unit = {

    val size = 8
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }
    val weights = Array( Array(0.0f, 0.5f, 0.0f ), Array(0.5f, 1.0f, 0.5f ),Array(0.0f, 0.5f, 0.0f ) )
    val gold = Array( 4.0f,6.0f,8.0f,10.0f,12.0f,14.0f )


    val stencil = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      ArrayType(ArrayType(Float, weights.length), weights.length),
      (input,wgts) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, Join() $ wgts)
          }))
        ) o Slide2D(3,1) $ input
      })

    val (output: Array[Float], _) = Execute(2,2)(stencil, values, weights)

    StencilUtilities.print2DArray(values)
    StencilUtilities.print1DArrayAs2DArray(output,size-2)
    StencilUtilities.print1DArrayAs2DArray(gold,size-2)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestMapAt(): Unit = {

    val size = 8
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(3,1).toArray.map(x => x.reduceLeft(_ + _))


    val stencil = fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        MapGlb(
          toGlobal(MapSeqUnroll(id)) o
            ReduceSeqUnroll(fun((acc, y) => {
              absAndSumUp.apply(acc, y)
            }), 0.0f))
      } o Slide(3, 1)  $ input
    )

    val (output: Array[Float], _) = Execute(2,2)(stencil, values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(output)

    assertArrayEquals(gold, output, 0.1f)

  }
}
