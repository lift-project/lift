package opencl.generator

import lift.arithmetic.SizeVar
import benchmarks.MatrixTransposition
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import scala.util.Random

object TestTranspose {
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

class TestTranspose {

  @Test def transposeWrite2Dims(): Unit = {
    val input = Array.tabulate(2, 4, 8)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    val gold = input.map(_.transpose).transpose

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, SizeVar("N")), SizeVar("M")), SizeVar("L")),
      input => TransposeW() o MapWrg(TransposeW() o MapLcl(MapSeq(id))) $ input
    )

    val (output: Array[Float], _) = Execute(4, 4)(f, input)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def idTransposeWrite(): Unit = {
    val input = Array.tabulate(2, 4, 8)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    val gold = input


    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, SizeVar("N")), SizeVar("M")), SizeVar("L")),
      input => MapWrg(TransposeW() o TransposeW() o MapLcl(MapSeq(id))) $ input
    )

    val (output: Array[Float], _) = Execute(4, 4)(f, input)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def twiceTransposeWriteScala(): Unit = {
    val N = 2
    val M = 4
    val L = 8
    val input = Array.tabulate(N, M, L)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    val gold = input.map(_.transpose).transpose

    val test = Array.ofDim[Float](L, N, M).flatten.flatten

    for (i <- 0 until N) {
      for (j <- 0 until M) {
        for (k <- 0 until L) {
          test(j + M*i + N*M*k) = input(i)(j)(k)
        }
      }
    }

    assertArrayEquals(gold.flatten.flatten, test, 0.0f)
  }

  @Test def transposeTwiceAfterPadId(): Unit = {
    val input = Array.tabulate(1024, 1024) { (i, j) => Random.nextFloat() }
    val f = fun(
      ArrayType(ArrayType(Float, SizeVar("N")), SizeVar("M")),
      input => MapSeq(MapSeq(id)) o Transpose() o Pad(2,2,Pad.Boundary.Wrap) o Transpose() o Pad(2,2,Pad.Boundary.Wrap) $ input
    )

    val (output: Array[Float], _) = Execute(32, 32)(f, input)
    val gold = input(0) ++ input ++ input(input.size -1)
  }

  @Test def idTranspose(): Unit = {
    val input = Array.tabulate(2, 4, 8)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)
    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, SizeVar("N")), SizeVar("M")), SizeVar("L")),
      input => MapWrg(
        toGlobal(MapLcl(MapSeq(id))) o
          Transpose() o TransposeW() o
          toLocal(MapLcl(MapSeq(id)))
      ) $ input
    )

    val (output: Array[Float], _) = Execute(4, 4)(f, input)

    assertArrayEquals(input.flatten.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE_Join_Gather_Split(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    Utils.myPrint(matrix)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        MapGlb(0)(MapGlb(1)(id)) o Split(N) o Gather(transposeFunction(M, N)) o Join() $ matrix
      })

    val (output: Array[Float], runtime) = Execute(32, Nsize * Msize)(f, matrix)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("output: ")
    Utils.myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def transposeMatrixOnWrite(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    Utils.myPrint(matrix)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      (matrix) => {
        TransposeW() o MapGlb(0)(MapGlb(1)(id)) $ matrix
      })

    val (output: Array[Float], runtime) = Execute(32, Nsize * Msize)(f, matrix)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("output: ")
    Utils.myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def transposeMatrix3DOuterOnWrite(): Unit = {

    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    println("matrix: ")
    Utils.myPrint(matrix)

    val gold   = matrix.transpose

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

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

    val (output: Array[Float], runtime) = Execute(4, Nsize * Msize)(f, matrix)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    Utils.myPrint(gold.flatten.flatten, Nsize, Ksize)

    println("output: ")
    Utils.myPrint(output, Nsize, Ksize)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def transposeMatrix3DInnerOnWrite(): Unit = {

    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    println("matrix: ")
    Utils.myPrint(matrix)

    val gold   = matrix.map(_.transpose)

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), M), N),
      (matrix) => {
          MapGlb(0)(
            TransposeW() o MapGlb(1)(
              MapSeq(id)
            )
          ) $ matrix
      })

    val (output: Array[Float], runtime) = Execute(4, Nsize * Msize)(f, matrix)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    Utils.myPrint(gold.flatten.flatten, Nsize, Ksize)

    println("output: ")
    Utils.myPrint(output, Nsize, Ksize)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE(): Unit = {

    val Nsize = 12
    val Msize = 8
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix: ")
    Utils.myPrint(matrix)

    val (output: Array[Float], runtime) =
      Execute(32, Nsize * Msize)(MatrixTransposition.naive, matrix)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("output: ")
    Utils.myPrint(output, Nsize)

    assertArrayEquals(gold.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE_3D(): Unit = {

    val Nsize = 8
    val Msize = 4
    val Ksize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    println("matrix: ")
    Utils.myPrint(matrix)

    val gold   = matrix.transpose

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayType(ArrayType(ArrayType(Float, K), M), N),
      (matrix) => {

          MapGlb(0)(
            MapGlb(1)(
              MapSeq(id)
            )
          ) o Split(Nsize) o Gather(transposeFunction(M, N)) o
         Join() $ matrix
      })

    val (output: Array[Float], runtime) = Execute(4, Nsize * Msize)(f, matrix)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    println("gold: ")
    Utils.myPrint(gold.flatten.flatten, Nsize, Ksize)

    println("output: ")
    Utils.myPrint(output, Nsize, Ksize)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def MATRIX_TRANSPOSE_4D(): Unit = {

    val Nsize = 16
    val Msize = 8
    val Ksize = 4
    val Lsize = 2
    val matrix = Array.tabulate(Nsize, Msize, Ksize, Lsize)((r, c, z, l) => {
        c * 4.0f + r * 16.0f + z * 2.0f + l * 1.0f
      })

    val gold   = matrix.transpose

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")
    val L = SizeVar("L")

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

    val (output: Array[Float], runtime) = Execute(4, Nsize * Msize * Lsize)(f, matrix)

    println("output.length = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(gold.flatten.flatten.flatten, output, 0.0f)
  }

  // Two transpose in a row should generate the ID function
  @Test def MATRIX_TRANSPOSE_NOOP(): Unit = {
    val Nsize = 16
    val gold = Array.fill(Nsize)(util.Random.nextFloat())

    val f = fun(
      ArrayType(Float, SizeVar("N")),
      (domain) => MapGlb(id)
        o Join()
        o Transpose() o Transpose()
        o Split(4)
        $ domain
    )

    val (output: Array[Float], _) = Execute(Nsize,Nsize)(f, gold)

    assertArrayEquals(gold, output, 0.0f)
  }

}
