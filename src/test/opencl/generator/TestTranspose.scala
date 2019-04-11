package opencl.generator

import benchmarks.MatrixTransposition
import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute, TestWithExecutor, Utils}
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

object TestTranspose extends TestWithExecutor

class TestTranspose {

  @Test def transposeWrite2Dims(): Unit = {
    val input = Array.tabulate(2, 4, 8)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    val gold = input.map(_.transpose).transpose

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("N")), SizeVar("M")), SizeVar("L")),
      input => TransposeW() o MapWrg(TransposeW() o MapLcl(MapSeq(id))) $ input
    )

    val (output, _) = Execute(4, 4)[Array[Float]](f, input)

    assertArrayEquals(gold.flatten.flatten, output, 0.0f)
  }

  @Test def idTransposeWrite(): Unit = {
    val input = Array.tabulate(2, 4, 8)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)

    val gold = input


    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("N")), SizeVar("M")), SizeVar("L")),
      input => MapWrg(TransposeW() o TransposeW() o MapLcl(MapSeq(id))) $ input
    )

    val (output, _) = Execute(4, 4)[Array[Float]](f, input)

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
    val input = Array(0,1,2,3).map(_.toFloat).grouped(2).toArray
    val gold = Array(
      0,0,1,1,
      0,0,1,1,
      2,2,3,3,
      2,2,3,3).map(_.toFloat)
    val f = fun(
      ArrayType(ArrayType(Float, SizeVar("N")), SizeVar("M")),
      input => MapSeq(MapSeq(id)) o Transpose() o Pad(1,1,Pad.Boundary.Clamp) o Transpose() o Pad(1,1,Pad.Boundary.Clamp) $ input
    )

    val (output, _) = Execute(32, 32)[Array[Float]](f, input)
    assertArrayEquals(gold, output, 0.0f)
  }

  @Test def idTranspose(): Unit = {
    val input = Array.tabulate(2, 4, 8)((r, c, z) => c * 2.0f + r * 8.0f + z * 1.0f)
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("N")), SizeVar("M")), SizeVar("L")),
      input => MapWrg(
        toGlobal(MapLcl(MapSeq(id))) o
          Transpose() o TransposeW() o
          toLocal(MapLcl(MapSeq(id)))
      ) $ input
    )

    val (output, _) = Execute(4, 4)[Array[Float]](f, input)

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
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (matrix) => {
        MapGlb(0)(MapGlb(1)(id)) o Split(N) o Gather(transposeFunction(M, N)) o Join() $ matrix
      })

    val (output, runtime) = Execute(32, Nsize * Msize)[Array[Float]](f, matrix)

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
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (matrix) => {
        TransposeW() o MapGlb(0)(MapGlb(1)(id)) $ matrix
      })

    val (output, runtime) = Execute(32, Nsize * Msize)[Array[Float]](f, matrix)

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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), N),
      (matrix) => {
        TransposeW() o
          MapGlb(0)(
            MapGlb(1)(
              MapSeq(id)
            )
          ) $ matrix
      })

    val (output, runtime) = Execute(4, Nsize * Msize)[Array[Float]](f, matrix)

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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), N),
      (matrix) => {
          MapGlb(0)(
            TransposeW() o MapGlb(1)(
              MapSeq(id)
            )
          ) $ matrix
      })

    val (output, runtime) = Execute(4, Nsize * Msize)[Array[Float]](f, matrix)

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

    val (output, runtime) = Execute(32, Nsize * Msize)[Array[Float]](MatrixTransposition.naive, matrix)

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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M), N),
      (matrix) => {

          MapGlb(0)(
            MapGlb(1)(
              MapSeq(id)
            )
          ) o Split(Nsize) o Gather(transposeFunction(M, N)) o
         Join() $ matrix
      })

    val (output, runtime) = Execute(4, Nsize * Msize)[Array[Float]](f, matrix)

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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, L), K), M), N),
      (matrix) => {
        MapWrg(0)(
          MapWrg(1)(
            MapLcl(0)(
              MapLcl(1)(id)
            )
          )
        ) o Transpose() $ matrix
      })

    val (output, runtime) = Execute(4, Nsize * Msize * Lsize)[Array[Float]](f, matrix)

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
      ArrayTypeWSWC(Float, SizeVar("N")),
      (domain) => MapGlb(id)
        o Join()
        o Transpose() o Transpose()
        o Split(4)
        $ domain
    )

    val (output, _) = Execute(Nsize,Nsize)[Array[Float]](f, gold)

    assertArrayEquals(gold, output, 0.0f)
  }

  // The following tests are for Issue #160, where the use of Transpose in conjunction with ArrayAccess is proving problematic

  @Test
  def boundaryTest2DColumnBroken(): Unit =
  {

    val localSizeX = 6
    val localSizeY = 8

    val values = Array.tabulate(localSizeX,localSizeY) { (i,j) => (i*localSizeY+ j + 1).toFloat }
    val gold = Array(8.0f,16.0f,24.0f,32.0f,40.0f,48.0f)

    val N = SizeVar("N")
    val M = SizeVar("M")

    def getColumnBoundary() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N),M),
      (input) => {

        MapSeq(id) o ArrayAccess(N-1) o Transpose() $ input

      })

    println(Compile(getColumnBoundary()))

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](getColumnBoundary,values)

    StencilUtilities.print2DArray(values)
    println("")
    StencilUtilities.print1DArray(gold)
    println("")
    StencilUtilities.print1DArray(output)

    assertArrayEquals(output, gold, StencilUtilities.stencilDelta)

  }

  @Test
  def boundaryTest2DColumnWorkaround(): Unit = {

    val localSizeX = 6
    val localSizeY = 8

    var values = Array.tabulate(localSizeX,localSizeY) { (i,j) => (i*localSizeY + j + 1).toFloat }
    val gold = Array(8.0f,16.0f,24.0f,32.0f,40.0f,48.0f)

    val N = SizeVar("N")
    val M = SizeVar("M")

    def getColumnBoundary() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N),M),
      (input) => {

        toGlobal(MapSeq(id)) o ArrayAccess(N-1) o toGlobal(MapSeq(MapSeq(id))) o Transpose() $ input

      })

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](getColumnBoundary,values)

    StencilUtilities.print2DArray(values)
    println("")
    StencilUtilities.print1DArray(gold)
    println("")
    StencilUtilities.print1DArray(output)

    assertArrayEquals(output, gold, StencilUtilities.stencilDelta)

  }

  @Test
  def boundaryTest2DRowAlsoBroken(): Unit = {

    val localSizeX = 6
    val localSizeY = 8
    val size = 12

    val values = Array.tabulate(localSizeX,localSizeY) { (i,j) => (i*size + j + 1).toFloat }
    val gold = Array(61.0f,62.0f,63.0f,64.0f,65.0f,66.0f,67.0f,68.0f)

    val N = SizeVar("N")
    val M = SizeVar("M")

    def getRowBoundary() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N),M),
      (input) => {

        toGlobal(MapSeq(id)) o ArrayAccess(M-1) o Transpose() o Transpose() $ input

      })

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](getRowBoundary,values)

    StencilUtilities.print2DArray(values)
    println("")
    StencilUtilities.print1DArray(gold)
    println("")
    StencilUtilities.print1DArray(output)

    assertArrayEquals(output, gold, StencilUtilities.stencilDelta)

  }

}
