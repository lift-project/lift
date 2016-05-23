package opencl.generator

import apart.arithmetic.{SizeVar, Var}
import benchmarks.MatrixTransposition
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestTiling {
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

class TestTiling {

  @Test def tileMatrix(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))

    println("matrix: ")
    Utils.myPrint(matrix)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)( MapLcl(0)(MapLcl(1)(id)))) o
          Tile(4) $ matrix
      })

    val (output: Array[Float], runtime) = Execute(32, Nsize * Msize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("tile: ")
    Utils.myPrint(gold(0)(0))

    println("gold: ")
    Utils.myPrint(gold.flatten.flatten.flatten, 8)

    println("output: ")
    Utils.myPrint(output, Msize)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  @Test def tileMatrixLocalMemory(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))

    println("matrix: ")
    Utils.myPrint(matrix)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)(
           toGlobal(MapLcl(0)(MapLcl(1)(id))) o
             toLocal(MapLcl(0)(MapLcl(1)(id)))
        )) o
          Tile(4) $ matrix
      })

    val (output: Array[Float], runtime) = Execute(4,4, Nsize, Msize, (false, false))(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("tile: ")
    Utils.myPrint(gold(0)(0))

    println("gold: ")
    Utils.myPrint(gold.flatten.flatten.flatten, 8)

    println("output: ")
    Utils.myPrint(output, Msize)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  @Test def tileAndUntileMatrix(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val tiled = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))
    val gold = tiled.flatMap(_.transpose.map(_.flatten))

    println("matrix: ")
    Utils.myPrint(matrix)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        // Merge the tiles
        MapWrg(0)(MapWrg(1)(id)) o
          Join() o Map(Map(Join()) o Transpose()) o
          // Tile the matrix
          Tile(4) $ matrix
      })

    val (output: Array[Float], runtime) = Execute(32, Nsize * Msize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    Utils.myPrint(gold.flatten, 8)

    println("output: ")
    Utils.myPrint(output, Msize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def tileAndUntileMatrixLocalMemory(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val tiled = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))
    val gold = tiled.flatMap(_.transpose.map(_.flatten))

    println("matrix: ")
    Utils.myPrint(matrix)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        // Merge the tiles
        Untile() o
        MapWrg(0)(MapWrg(1)(
           toGlobal(MapLcl(0)(MapLcl(1)(id))) o
           toLocal(MapLcl(0)(MapLcl(1)(id)))
        )) o
          // Tile the matrix
          Tile(4) $ matrix
      })

    val (output: Array[Float], runtime) = Execute(4,4, Nsize, Msize, (false, false))(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    Utils.myPrint(gold.flatten, 8)

    println("output: ")
    Utils.myPrint(output, Msize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def transposeMatrixInsideTiles(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold = matrix.grouped(4).toArray.map(_.transpose.grouped(4)
                                              .toArray.map(_.transpose)).map(_.map(_.transpose))

    println("matrix: ")
    Utils.myPrint(matrix)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)( MapLcl(0)(MapLcl(1)(id)) o Transpose())) o Tile(4) $ matrix
      })

    val (output: Array[Float], runtime) = Execute(32, Nsize * Msize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("tile: 0,0 ")
    Utils.myPrint(gold(0)(0))

    println("tile: 0,1 ")
    Utils.myPrint(gold(0)(1))

    println("gold: ")
    Utils.myPrint(gold.flatten.flatten.flatten, 8)

    println("output: ")
    Utils.myPrint(output, Msize)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  @Test def transposeMatrixInsideTilesLocalMemory(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold = matrix.grouped(4).toArray.map(_.transpose.grouped(4)
                                              .toArray.map(_.transpose)).map(_.map(_.transpose))

    println("matrix: ")
    Utils.myPrint(matrix)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)(
           toGlobal(MapLcl(0)(MapLcl(1)(id))) o
            TransposeW() o  toLocal(MapLcl(0)(MapLcl(1)(id))))) o Tile(4) $ matrix
      })

    val (output: Array[Float], runtime) = Execute(4,4, Nsize, Msize, (false, false))(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("tile: 0,0 ")
    Utils.myPrint(gold(0)(0))

    println("tile: 0,1 ")
    Utils.myPrint(gold(0)(1))

    println("gold: ")
    Utils.myPrint(gold.flatten.flatten.flatten, 8)

    println("output: ")
    Utils.myPrint(output, Msize)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  @Test def transposeTiles(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold = matrix.grouped(4).toArray.map(_.transpose.grouped(4)
                                              .toArray.map(_.transpose)).transpose


    println("matrix: ")
    Utils.myPrint(matrix)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)( MapLcl(0)(MapLcl(1)(id)))) o Transpose() o
          Tile(4) $ matrix
      })

    val (output: Array[Float], runtime) = Execute(32, Nsize * Msize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("tile: 0,0 ")
    Utils.myPrint(gold(0)(0))

    println("tile: 0,1 ")
    Utils.myPrint(gold(0)(1))

    println("gold: ")
    Utils.myPrint(gold.flatten.flatten.flatten, Nsize)

    println("output: ")
    Utils.myPrint(output, Nsize)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  @Test def tiledMatrixTranspose(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val tiled = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))
    val tiledTransposed = tiled.map(_.map(_.transpose)).transpose

    val gold = tiledTransposed.flatMap(_.transpose.map(_.flatten))

    println("matrix: ")
    Utils.myPrint(matrix)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        // Merge the tiles
        Untile() o
        MapWrg(0)(MapWrg(1)(MapLcl(0)(MapLcl(1)(id)))) o
          // Transpose the tiles and then the insides of tiles
          Map(Map(Transpose())) o Transpose() o
          // Tile the matrix
          Tile(4) $ matrix
      })

    val (output: Array[Float], runtime) = Execute(32, Nsize * Msize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    Utils.myPrint(gold.flatten, Nsize)

    println("output: ")
    Utils.myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def tiledMatrixTransposeLocalMemory(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val tiled = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))
    val tiledTransposed = tiled.map(_.map(_.transpose)).transpose

    val gold = tiledTransposed.flatMap(_.transpose.map(_.flatten))

    println("matrix: ")
    Utils.myPrint(matrix)

    val (output: Array[Float], runtime) =
      Execute(32, Nsize * Msize)(MatrixTransposition.coalesced(), matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    Utils.myPrint(gold.flatten, Nsize)

    println("output: ")
    Utils.myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def tiledMatrixTransposeNonSquareTile(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val tiled = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))
    val tiledTransposed = tiled.map(_.map(_.transpose)).transpose

    val gold = tiledTransposed.flatMap(_.transpose.map(_.flatten))

    println("matrix: ")
    Utils.myPrint(matrix)

    val (output: Array[Float], runtime) =
      Execute(32, Nsize * Msize)(MatrixTransposition.coalesced(4,8), matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    Utils.myPrint(gold.flatten, Nsize)

    println("output: ")
    Utils.myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }
}
