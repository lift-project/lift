package opencl.generator

import ir._
import ir.UserFunDef._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.IndexFunction.transpose

import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestTransposeAndTiling {
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

class TestTransposeAndTiling {

  @Test def MATRIX_PLUS_ONE(): Unit = {

    val Msize = 512
    val Ksize = 512
    val matrix = Array.tabulate(Msize, Ksize)((r, c) => 1.0f * c * r)
    val gold   = matrix.map(_.map(_+1.0f))

    val M = Var("M")
    val K = Var("K")

    val r = 2
    val c = 4

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      (matrix) => {
        Join() o MapWrg(0)(fun( rows =>
          MapLcl(0)(fun( row =>
            Join() o MapWrg(1)(fun( cols =>
              MapLcl(1)(fun( col =>
                plusOne(col)
              )) $ cols
            )) o Split(c) $ row
          )) $ rows
        )) o Split(r) $ matrix
      })

    val (output, runtime) = Execute(Ksize * Msize)(f, matrix, Ksize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def MATRIX_PLUS_ONE_TILED(): Unit = {

    val Msize = 8
    val Ksize = 16
    val matrix = Array.tabulate(Msize, Ksize)((r, c) => 1.0f * (c + r))
    val gold   = matrix.map(_.map(_+1.0f))

    val M = Var("M")
    val K = Var("K")

    val r = 4
    val c = 8

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      (matrix) => {
        Join() o MapWrg(0)(fun( cols =>
          MapSeq(Join()) o MapWrg(1)(fun( tile =>

            // step 2: compute plus one
            toGlobal(
              MapLcl(0)(fun( row =>
                MapLcl(1)(fun( elem =>
                  id(elem)
                )) $ row
              ))
            ) o
              // step 1: load tile to local memory
              toLocal(
                MapLcl(0)(fun( row =>
                  MapLcl(1)(fun( elem =>
                    plusOne(elem)
                  )) $ row
                ))
              ) $ tile

          )) o MapSeq(Split(c)) $ cols
        )) o Split(r) $  matrix
      })

    val (output, runtime) = Execute(32, Ksize * Msize)(f, matrix, Ksize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten, Ksize)

    println("output: ")
    myPrint(output, Ksize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def MATRIX_PLUS_ONE_TILED_TRANSPOSE_WITH_JOIN_REORDER_SPLIT(): Unit = {

    val Msize = 8
    val Ksize = 16
    val matrix = Array.tabulate(Msize, Ksize)((r, c) => 1.0f * (c + r))
    val gold   = matrix.map(_.map(_+1.0f))

    val M = Var("M")
    val K = Var("K")

    val r = 4
    val c = 8

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      (matrix) => {
        Join() o MapWrg(0)(fun( cols =>
          MapSeq(Join()) o MapWrg(1)(fun( tile =>

            MapLcl(0)(fun( row =>
              MapLcl(1)(fun( elem =>
                plusOne(elem)
              )) $ row
            )) $ tile

          )) o MapSeq(Split(c)) $ cols
        )) o Split(r) $  matrix
      })

    val (output, runtime) = Execute(32, Ksize * Msize)(f, matrix, Ksize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten, Ksize)

    println("output: ")
    myPrint(output, Ksize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  private def myPrint(m: Array[Array[Array[Float]]]): Unit = {
    m.map( r => {
      println(r.map( e => {
        "(" + e.map("%2.0f".format(_)).reduce(_ + ", " + _) + ")"
      }).reduce(_ + " " + _))
    } )
  }

  private def myPrint(m: Array[Array[Float]]): Unit = {
    m.map( r => {
      println(r.map("%2.0f".format(_)).reduce(_ + " " + _))
    } )
  }

  private def myPrint(m: Array[Float], cols: Int): Unit = {
    val (row, rest) = m.splitAt(cols)
    if (row.nonEmpty) println(row.map("%2.0f".format(_)).reduce(_ + " " + _))
    if (rest.nonEmpty) myPrint(rest, cols)
  }

  private def printRow(r: Array[Float], elems: Int): Unit = {
    val (elem, rest) = r.splitAt(elems)
    if (elem.nonEmpty) print("(" + elem.map("%2.0f".format(_)).reduce(_ + ", " + _) + ") ")
    if (rest.nonEmpty) printRow(rest, elems)
  }

  private def myPrint(m: Array[Float], cols: Int, elems: Int): Unit = {
    val (row, rest) = m.splitAt(cols*elems)
    if (row.nonEmpty) printRow(row, elems); println("")
    if (rest.nonEmpty) myPrint(rest, cols, elems)
  }

  @Test def MATRIX_TRANSPOSE_Join_Gather_Split(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    myPrint(matrix)

    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapGlb(0)(MapGlb(1)(id)) o Gather(transpose)(Split(N)) o Join() $ matrix
      })

    val (output, runtime) = Execute(32, Nsize * Msize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("output: ")
    myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def transposeMatrixOnWrite(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    myPrint(matrix)

    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        TransposeW() o MapGlb(0)(MapGlb(1)(id)) $ matrix
      })

    val (output, runtime) = Execute(32, Nsize * Msize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("output: ")
    myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def transposeMatrix3DOuterOnWrite(): Unit = {

    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    println("matrix: ")
    myPrint(matrix)

    val gold   = matrix.transpose

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), M), N),
      (matrix) => {
        TransposeW() o
          MapGlb(0)(
            MapGlb(1)(
              MapSeq(id)
            )
          ) $ matrix
      })

    val (output, runtime) = Execute(4, Nsize * Msize)(f, matrix, Nsize, Msize, Ksize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten.flatten, Nsize, Ksize)

    println("output: ")
    myPrint(output, Nsize, Ksize)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def transposeMatrix3DInnerOnWrite(): Unit = {

    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    println("matrix: ")
    myPrint(matrix)

    val gold   = matrix.map(_.transpose)

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), M), N),
      (matrix) => {
          MapGlb(0)(
            TransposeW() o MapGlb(1)(
              MapSeq(id)
            )
          ) $ matrix
      })

    val (output, runtime) = Execute(4, Nsize * Msize)(f, matrix, Nsize, Msize, Ksize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten.flatten, Nsize, Ksize)

    println("output: ")
    myPrint(output, Nsize, Ksize)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def tileMatrix(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))

    println("matrix: ")
    myPrint(matrix)

    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)(MapLcl(0)(MapLcl(1)(id)))) o
          MapWrg(0)(MapWrg(1)(Transpose()
          ) o Split(4) o Transpose()) o Split(4) $ matrix
      })

    val (output, runtime) = Execute(32, Nsize * Msize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("tile: ")
    myPrint(gold(0)(0))

    println("gold: ")
    myPrint(gold.flatten.flatten.flatten, 8)

    println("output: ")
    myPrint(output, Msize)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  @Test def tileAndUntileMatrix(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val tiled = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))
    val gold = tiled.map(_.transpose.map(_.flatten)).flatten

    println("matrix: ")
    myPrint(matrix)

    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        // Merge the tiles
        MapWrg(0)(MapWrg(1)(id)) o
          Join() o MapWrg(0)(MapWrg(1)(Join()) o Transpose()) o
          // Tile the matrix
          MapWrg(0)(MapWrg(1)(Transpose()) o Split(4) o Transpose()) o Split(4) $ matrix
      })

    val (output, runtime) = Execute(32, Nsize * Msize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten, 8)

    println("output: ")
    myPrint(output, Msize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def transposeMatrixInsideTiles(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose)).map(_.map(_.transpose))

    println("matrix: ")
    myPrint(matrix)

    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)(MapLcl(0)(MapLcl(1)(id)) o Transpose())) o
          MapWrg(0)(MapWrg(1)(Transpose()
          ) o Split(4) o Transpose()) o Split(4) $ matrix
      })

    val (output, runtime) = Execute(32, Nsize * Msize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("tile: 0,0 ")
    myPrint(gold(0)(0))

    println("tile: 0,1 ")
    myPrint(gold(0)(1))

    println("gold: ")
    myPrint(gold.flatten.flatten.flatten, 8)

    println("output: ")
    myPrint(output, Msize)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  @Test def transposeTiles(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose)).transpose


    println("matrix: ")
    myPrint(matrix)

    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapWrg(0)(MapWrg(1)(MapLcl(0)(MapLcl(1)(id)))) o Transpose() o
          MapWrg(0)(MapWrg(1)(Transpose()
          ) o Split(4) o Transpose()) o Split(4) $ matrix
      })

    val (output, runtime) = Execute(32, Nsize * Msize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("tile: 0,0 ")
    myPrint(gold(0)(0))

    println("tile: 0,1 ")
    myPrint(gold(0)(1))

    println("gold: ")
    myPrint(gold.flatten.flatten.flatten, Nsize)

    println("output: ")
    myPrint(output, Nsize)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  @Test def tiledMatrixTranspose(): Unit = {
    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val tiled = matrix.grouped(4).toArray.map(_.transpose.grouped(4).toArray.map(_.transpose))
    val tiledTransposed = tiled.map(_.map(_.transpose)).transpose

    val gold = tiledTransposed.map(_.transpose.map(_.flatten)).flatten

    println("matrix: ")
    myPrint(matrix)

    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        // Merge the tiles
        MapWrg(0)(MapWrg(1)(id)) o
          Join() o MapWrg(0)(MapWrg(1)(Join()) o Transpose()) o
          // Transpose the tiles and then the insides of tiles
          MapWrg(0)(MapWrg(1)(Transpose())) o Transpose() o
          // Tile the matrix
          MapWrg(0)(MapWrg(1)(Transpose()) o Split(4) o Transpose()) o Split(4) $ matrix
      })

    val (output, runtime) = Execute(32, Nsize * Msize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten, Nsize)

    println("output: ")
    myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    myPrint(matrix)

    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapGlb(0)(MapGlb(1)(id)) o Transpose() $ matrix
      })

    val (output, runtime) = Execute(32, Nsize * Msize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("output: ")
    myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE_3D(): Unit = {

    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    println("matrix: ")
    myPrint(matrix)

    val gold   = matrix.transpose

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), M), N),
      (matrix) => {
        Gather(transpose)(
          MapGlb(0)(
            MapGlb(1)(
              MapSeq(id)
            )
          ) o Split(Nsize)
        ) o Join() $ matrix
      })

    val (output, runtime) = Execute(4, Nsize * Msize)(f, matrix, Nsize, Msize, Ksize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    myPrint(gold.flatten.flatten, Nsize, Ksize)

    println("output: ")
    myPrint(output, Nsize, Ksize)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE_4D(): Unit = {

    val Nsize = 16
    val Msize = 8
    val Ksize = 4
    val Lsize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize, Lsize)((r, c, z, l) => c * 4.0f + r * 16.0f + z * 2.0f + l * 1.0f)

    val gold   = matrix.transpose

    val N = Var("N")
    val M = Var("M")
    val K = Var("K")
    val L = Var("L")

    val f = fun(
      ArrayType(ArrayType(ArrayType(ArrayType(Float, L), K), M), N),
      (matrix) => {
        MapWrg(0)(
          MapWrg(1)(
            MapLcl(0)(
              MapLcl(1)(id)
            )
          )
        ) o Transpose() $ matrix
      })

    val (output, runtime) = Execute(4, Nsize * Msize * Lsize)(f, matrix, Nsize, Msize, Ksize, Lsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

}
