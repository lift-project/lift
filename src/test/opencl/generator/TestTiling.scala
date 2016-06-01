package opencl.generator

import apart.arithmetic.SizeVar
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

  private val N = SizeVar("N")
  private val M = SizeVar("M")

  private val Nsize = 12
  private val Msize = 8

  private val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
  private val transposeGold = matrix.transpose

  @Test def tileMatrix(): Unit = {

    val gold   = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))

    println("matrix: ")
    Utils.myPrint(matrix)

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)( MapLcl(0)(MapLcl(1)(id)))) o
          Tile(4) $ matrix
      })

    val (output: Array[Float], _) = Execute(32, Nsize * Msize)(f, matrix)

    println("tile: ")
    Utils.myPrint(gold(0)(0))

    println("gold: ")
    Utils.myPrint(gold.flatten.flatten.flatten, 8)

    println("output: ")
    Utils.myPrint(output, Msize)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  @Test def tileMatrixLocalMemory(): Unit = {

    val gold   = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))

    println("matrix: ")
    Utils.myPrint(matrix)

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)(
           toGlobal(MapLcl(0)(MapLcl(1)(id))) o
             toLocal(MapLcl(0)(MapLcl(1)(id)))
        )) o
          Tile(4) $ matrix
      })

    val (output: Array[Float], _) = Execute(4,4, Nsize, Msize, (false, false))(f, matrix)

    println("tile: ")
    Utils.myPrint(gold(0)(0))

    println("gold: ")
    Utils.myPrint(gold.flatten.flatten.flatten, 8)

    println("output: ")
    Utils.myPrint(output, Msize)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  @Test def tileAndUntileMatrix(): Unit = {

    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val tiled = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))
    val gold = tiled.flatMap(_.transpose.map(_.flatten))

    println("matrix: ")
    Utils.myPrint(matrix)

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        // Merge the tiles
        MapWrg(0)(MapWrg(1)(id)) o
          Join() o Map(Map(Join()) o Transpose()) o
          // Tile the matrix
          Tile(4) $ matrix
      })

    val (output: Array[Float], _) = Execute(32, Nsize * Msize)(f, matrix)

    println("gold: ")
    Utils.myPrint(gold.flatten, 8)

    println("output: ")
    Utils.myPrint(output, Msize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def tileAndUntileMatrixLocalMemory(): Unit = {

    val tiled = matrix.grouped(4).toArray.
      map(_.transpose.grouped(4).toArray.map(_.transpose))
    val gold = tiled.flatMap(_.transpose.map(_.flatten))

    println("matrix: ")
    Utils.myPrint(matrix)

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

    val (output: Array[Float], _) = Execute(4,4, Nsize, Msize, (false, false))(f, matrix)

    println("gold: ")
    Utils.myPrint(gold.flatten, 8)

    println("output: ")
    Utils.myPrint(output, Msize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def transposeMatrixInsideTiles(): Unit = {

    val gold = matrix.grouped(4).toArray.map(_.transpose.grouped(4)
      .toArray.map(_.transpose)).map(_.map(_.transpose))

    println("matrix: ")
    Utils.myPrint(matrix)

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)( MapLcl(0)(MapLcl(1)(id)) o Transpose())) o Tile(4) $ matrix
      })

    val (output: Array[Float], _) = Execute(32, Nsize * Msize)(f, matrix)

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

    val gold = matrix.grouped(4).toArray.map(_.transpose.grouped(4)
      .toArray.map(_.transpose)).map(_.map(_.transpose))

    println("matrix: ")
    Utils.myPrint(matrix)

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)(
           toGlobal(MapLcl(0)(MapLcl(1)(id))) o
            TransposeW() o  toLocal(MapLcl(0)(MapLcl(1)(id))))) o Tile(4) $ matrix
      })

    val (output: Array[Float], _) = Execute(4,4, Nsize, Msize, (false, false))(f, matrix)

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

    val gold = matrix.grouped(4).toArray.map(_.transpose.grouped(4)
      .toArray.map(_.transpose)).transpose

    println("matrix: ")
    Utils.myPrint(matrix)

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)( MapLcl(0)(MapLcl(1)(id)))) o Transpose() o
          Tile(4) $ matrix
      })

    val (output: Array[Float], _) = Execute(32, Nsize * Msize)(f, matrix)

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

    val (output: Array[Float], _) = Execute(32, Nsize * Msize)(f, matrix)

    println("gold: ")
    Utils.myPrint(transposeGold.flatten, Nsize)

    println("output: ")
    Utils.myPrint(output, Nsize)

    assertArrayEquals(transposeGold.flatten, output, 0.0f)
  }

  @Test def tiledMatrixTransposeLocalMemory(): Unit = {

    println("matrix: ")
    Utils.myPrint(matrix)

    val (output: Array[Float], _) =
      Execute(32, Nsize * Msize)(MatrixTransposition.coalesced(), matrix)

    println("gold: ")
    Utils.myPrint(transposeGold.flatten, Nsize)

    println("output: ")
    Utils.myPrint(output, Nsize)

    assertArrayEquals(transposeGold.flatten, output, 0.0f)
  }

  @Test def tiledMatrixTransposePrivateMemory(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        // Merge the tiles
        Untile() o
          MapGlb(1)(MapGlb(0)(
            toGlobal(MapSeq(MapSeq(id))) o
              TransposeW() o
              toPrivate(MapSeq(MapSeq(id)))
          )) o
          // Transpose the tiles and then the insides of tiles
          Transpose() o
          // Tile the matrix
          Tile(4) $ matrix
      })

    val (output: Array[Float], _) = Execute(32, Nsize * Msize)(f, matrix)

    assertArrayEquals(transposeGold.flatten, output, 0.0f)
  }

  @Test def tiledMatrixTransposeNonSquareTile(): Unit = {

    println("matrix: ")
    Utils.myPrint(matrix)

    val (output: Array[Float], _) =
      Execute(32, Nsize * Msize)(MatrixTransposition.coalesced(4,8), matrix)

    println("gold: ")
    Utils.myPrint(transposeGold.flatten, Nsize)

    println("output: ")
    Utils.myPrint(output, Nsize)

    assertArrayEquals(transposeGold.flatten, output, 0.0f)
  }
}
