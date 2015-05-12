package opencl.generator

import ir.UserFunDef._
import opencl.executor._
import opencl.ir.Group._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import opencl.ir._
import ir._
import arithmetic._

import scala.util.Random

object TestStencil {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestStencil {

  val sumUp = UserFunDef("sumUp", Array("x", "y"), "{ return x + y; }", Seq(Float, Float), Float)
  val mult = UserFunDef("mult", Array("l", "r"), "{ return l * r; }", Seq(Float, Float), Float)

  def scala1DNeighbours(data: Array[Float], relIndices: Array[Int], idx: Int) = {
    relIndices.map(x => {
      val newIdx = idx + x

      // Boundary check
      if (newIdx < 0) {
        data(0)
      } else if (newIdx >= data.length) {
        data(data.length - 1)
      } else {
        data(newIdx)
      }
    })
  }

  def scala2DNeighbours(data: Array[Array[Float]], relRows: Array[Int],
                        relColumns: Array[Int], r: Int, c: Int) = {
    val nrRows = data.length
    val nrColumns = data(0).length

    relRows.map(x => {
      var newR = r + x
      if (newR < 0) {newR = 0}
      else if (newR >= nrRows) {newR = nrRows - 1}

      relColumns.map(y => {
        var newC = c + y
        if (newC < 0) {newC = 0}
        else if (newC >= nrColumns) {newC = nrColumns - 1}

        data(newR)(newC)
      })
    }).flatten
  }

  def scala1DStencil(data: Array[Float], relIndices: Array[Int], weights: Array[Float]) = {
    val neighbours = (0 until data.length).map(x => scala1DNeighbours(data, relIndices, x))
    neighbours.map(_.zip(weights).foldLeft(0.0f)((acc, p) => acc + p._1 * p._2)).toArray
  }

  def scala2DStencil(data: Array[Array[Float]],
                     relRows: Array[Int],
                     relColumns: Array[Int],
                     weights: Array[Float]) = {
    val nrRows = data.length
    val nrColumns = data(0).length

    val neighbours = (0 until nrRows).flatMap(r =>
      (0 until nrColumns).map(c =>
        scala2DNeighbours(data, relRows, relColumns, r, c))
    )

    neighbours.map(_.zip(weights).foldLeft(0.0f)((acc, p) => acc + p._1 * p._2)).toArray
  }

  @Test def SIMPLE_GROUP_1D_STENCIL(): Unit = {
    val data = Seq.fill(1024)(Random.nextFloat()).toArray
    val weights = Array(1, 2, 5, 2, 1) map {_ / 11.0f}
    val relativeIndices = Array(-2, -1, 0, 1, 2)

    val gold = scala1DStencil(data, relativeIndices, weights)

    val stencilFun = fun(
      ArrayType(Float, Var("N")),
      ArrayType(Float, Var("M")),
      (input, weights) => {
        MapGlb(
          fun(neighbours => toGlobal(MapSeq(id)) o ReduceSeq(sumUp, 0.0f) o
                            MapSeq(mult) $ Zip(weights, neighbours))
        ) o Group(relativeIndices, Group.edgeNeg, Group.edgePos) $ input
      }
    )

    val (output: Array[Float], runtime) = Execute(data.length)(stencilFun, data, weights)
    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.00001f)
  }

  @Test def GROUP_DOTPRODUCT_1D_STENCIL(): Unit = {
    val data = Seq.fill(1024)(Random.nextFloat()).toArray
    val weights = Array(1, 2, 5, 2, 1) map {_ / 11.0f}
    val relativeIndices = Array(-2, -1, 0, 1, 2)

    val gold = scala1DStencil(data, relativeIndices, weights)

    val stencilFun = fun(
      ArrayType(Float, Var("N")),
      ArrayType(Float, Var("M")),
      (input, weights) => {
        MapGlb(
          fun(neighbours => {
            toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => {
              multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
            }), 0.0f) $ Zip(neighbours, weights)
          })
        ) o Group(relativeIndices, Group.edgeNeg, Group.edgePos) $ input
      }
    )

    val (output: Array[Float], runtime) = Execute(data.length)(stencilFun, data, weights)
    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.00001f)
  }

  @Test def GROUP2D_DOTPRODUCT_2D_STENCIL(): Unit = {
    val Nsize = 128
    val Msize = 128
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => Random.nextFloat())
    val relColumns = Array(-1, 0, 1)
    val relRows = Array(-1, 0, 1)
    val weights = Array(0.08f, 0.12f, 0.08f, 0.12f, 0.2f, 0.12f, 0.08f, 0.12f, 0.08f)

    val gold = scala2DStencil(matrix, relRows, relColumns, weights)
    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(Float, Var("O")),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeq(id)) o
              ReduceSeq(fun((acc, y) => {
                multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Group2D(relColumns, relRows, edgeNeg, edgePos) $ matrix
      })

    val (output: Array[Float], runtime) =
      Execute(16, 16, Nsize, Msize, (false, false))(f, matrix, weights)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold, output, 0.000001f)
  }

  @Test def GROUP_EDGE(): Unit = {
    val data = Array(1, 2, 3, 4, 5).map(_.toFloat)
    val relIndices = Array(-2, 2)
    val edgeGold = Array(1, 3, 1, 4, 1, 5, 2, 5, 3, 5).map(_.toFloat).array

    val f = fun(
      ArrayType(Float, Var("N")),
      (input) => MapGlb(MapSeq(id)) o Group(relIndices, Group.edgeNeg, Group.edgePos) $ input
    )
    val (output: Array[Float], runtime) = Execute(1, data.length)(f, data)
    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(edgeGold, output, 0.0f)
  }

  @Test def GROUP_REFLECT(): Unit = {
    val data = Array(1, 2, 3, 4, 5).map(_.toFloat)
    val relIndices = Array(-2, 2)
    val reflectGold = Array(2, 3, 1, 4, 1, 5, 2, 5, 3, 4).map(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      (input) => MapGlb(MapSeq(id)) o Group(relIndices, Group.reflectNeg, Group.reflectPos) $ input
    )
    val (output: Array[Float], runtime) = Execute(1, data.length)(f, data)
    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(reflectGold, output, 0.0f)
  }

  @Test def GROUP_WRAP(): Unit = {
    val data = Array(1, 2, 3, 4, 5).map(_.toFloat)
    val relIndices = Array(-2, 2)
    val wrapGold = Array(4, 3, 5, 4, 1, 5, 2, 1, 3, 2).map(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      (input) => MapGlb(MapSeq(id)) o Group(relIndices, Group.wrapNeg, Group.wrapPos) $ input
    )
    val (output: Array[Float], runtime) = Execute(1, data.length)(f, data)
    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(wrapGold, output, 0.0f)
  }


}