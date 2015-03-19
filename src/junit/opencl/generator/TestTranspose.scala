package opencl.generator

import benchmarks.MatrixTransposition
import ir._
import ir.UserFunDef._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.IndexFunction.transpose

import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestTranspose {
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

class TestTranspose {

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
    PrintUtils.myPrint(gold.flatten, Ksize)

    println("output: ")
    PrintUtils.myPrint(output, Ksize)

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
    PrintUtils.myPrint(gold.flatten, Ksize)

    println("output: ")
    PrintUtils.myPrint(output, Ksize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE_Join_Gather_Split(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    PrintUtils.myPrint(matrix)

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
    PrintUtils.myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def transposeMatrixOnWrite(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    PrintUtils.myPrint(matrix)

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
    PrintUtils.myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def transposeMatrix3DOuterOnWrite(): Unit = {

    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    println("matrix: ")
    PrintUtils.myPrint(matrix)

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
    PrintUtils.myPrint(gold.flatten.flatten, Nsize, Ksize)

    println("output: ")
    PrintUtils.myPrint(output, Nsize, Ksize)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def transposeMatrix3DInnerOnWrite(): Unit = {

    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    println("matrix: ")
    PrintUtils.myPrint(matrix)

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
    PrintUtils.myPrint(gold.flatten.flatten, Nsize, Ksize)

    println("output: ")
    PrintUtils.myPrint(output, Nsize, Ksize)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    PrintUtils.myPrint(matrix)

    val (output, runtime) = Execute(32, Nsize * Msize)(MatrixTransposition.naive, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("output: ")
    PrintUtils.myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE_3D(): Unit = {

    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    println("matrix: ")
    PrintUtils.myPrint(matrix)

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
    PrintUtils.myPrint(gold.flatten.flatten, Nsize, Ksize)

    println("output: ")
    PrintUtils.myPrint(output, Nsize, Ksize)

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
