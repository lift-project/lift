package polybench

import benchmarks.GESUMMV
import ir.ArrayTypeWSWC
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.{AfterClass, BeforeClass, Test}

object HighLevel {
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

class HighLevel {

  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")

  val mm = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
    (A, B) => {
      MapGlb(fun( aRow =>
        MapSeq(fun( bCol =>
          toGlobal(MapSeq(id)) o
            ReduceSeq(fun((acc, y) =>
              multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
            ), 0.0f) $ Zip(aRow, bCol)
        )) o Transpose() $ B
      )) $ A
    })

  val mv = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(Float, K),
    (matrix, vector) =>
      MapGlb(fun(row => toGlobal(MapSeq(id)) o
        ReduceSeq(fun((acc, y) =>
          multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
        ), 0.0f) $ Zip(row, vector)
      )) $ matrix
  )

  val mvAlpha = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
    ArrayTypeWSWC(Float, K),
    Float,
    (matrix, vector, alpha) =>
      MapGlb(fun(row => toGlobal(MapSeq(id)) o
        MapSeq(fun(x => mult(x, alpha))) o
        ReduceSeq(fun((acc, y) =>
          multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
        ), 0.0f) $ Zip(row, vector)
      )) $ matrix
  )

  val gemvKernel = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    ArrayTypeWSWC(Float, M),
    ArrayTypeWSWC(Float,N),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) => {
      MapGlb(fun( t =>
        MapSeq(fun( x => multAndSumUp(x, Get(t, 1), beta))) o
          MapSeq(fun(x => mult(alpha, x))) o
          toGlobal(MapSeq(id)) o
          ReduceSeq(fun((acc, y) =>
            multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
          ), 0.0f) $ Zip(vectorX, Get(t, 0))
      )) $ Zip(matrix, vectorY)
    })

  val gemvTransposed = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
    ArrayTypeWSWC(Float, M),
    ArrayTypeWSWC(Float,N),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) => {
      MapGlb(fun( t =>
        MapSeq(fun( x => multAndSumUp(x, Get(t, 1), beta))) o
          MapSeq(fun(x => mult(alpha, x))) o
          toGlobal(MapSeq(id)) o
          ReduceSeq(fun((acc, y) =>
            multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
          ), 0.0f) $ Zip(vectorX, Get(t, 0))
      )) $ Zip(Transpose() $ matrix, vectorY)
    })

  val vecAdd = fun(
    ArrayTypeWSWC(Float, N),
    ArrayTypeWSWC(Float, N),
    (a,b) => MapGlb(add) $ Zip(a, b)
  )
  
  val matrixAdd = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (A, B) =>
      MapGlb(fun(x => MapSeq(add) $ Zip(Get(x, 0), Get(x, 1)))) $ Zip(A, B)
  )

  @Test
  def twoMM(): Unit = {
    // D=A.B; E=C.D
    // polybench actually implements D := alpha*A*B*C + beta*D, polybench gpu does the other
    // array sizes messed up in polybench, doesn't show up because of square matrices
    val i = 128
    val j = 16
    val k = 32
    val l = 64
    val A = Array.fill(i, k)(util.Random.nextInt(5).toFloat)
    val B = Array.fill(k, j)(util.Random.nextInt(5).toFloat)
    val C = Array.fill(l, i)(util.Random.nextInt(5).toFloat)

    val dGold = Utils.matrixMatrixMultiply(A, B)
    val eGold = Utils.matrixMatrixMultiply(C, dGold)

    val D = Execute(i)[Array[Float]](mm, A, B)._1.grouped(j).toArray
    val E = Execute(i)[Array[Float]](mm, C, D)._1

    assertArrayEquals(dGold.flatten, D.flatten, 0.0f)
    assertArrayEquals(eGold.flatten, E, 0.0f)
  }

  @Test
  def threeMM(): Unit = {
    // E=A.B; F=C.D; G=E.F
    val i = 128
    val j = 256
    val k = 32
    val l = 64
    val m = 16

    val A = Array.fill(i, k)(util.Random.nextInt(5).toFloat)
    val B = Array.fill(k, j)(util.Random.nextInt(5).toFloat)
    val C = Array.fill(j, m)(util.Random.nextInt(5).toFloat)
    val D = Array.fill(m, l)(util.Random.nextInt(5).toFloat)

    val eGold = Utils.matrixMatrixMultiply(A, B)
    val fGold = Utils.matrixMatrixMultiply(C, D)
    val gGold = Utils.matrixMatrixMultiply(eGold, fGold)

    val E = Execute(i)[Array[Float]](mm, A, B)._1.grouped(j).toArray
    val F = Execute(j)[Array[Float]](mm, C, D)._1.grouped(l).toArray
    val G = Execute(i)[Array[Float]](mm, E, F)._1

    assertArrayEquals(eGold.flatten, E.flatten, 0.0f)
    assertArrayEquals(fGold.flatten, F.flatten, 0.0f)
    assertArrayEquals(gGold.flatten, G, 0.0f)
  }

  @Test
  def gesummv(): Unit = {
    // y = A . x * alpha + B . x * beta
    val n = 128

    val alpha = 2.0f
    val beta = 1.5f
    val x = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val A = Array.fill(n, n)(util.Random.nextInt(5).toFloat)
    val B = Array.fill(n, n)(util.Random.nextInt(5).toFloat)

    val tmp1Gold = Utils.matrixVector(A, x, alpha)
    val tmp2Gold = Utils.matrixVector(B, x, beta)
    val yGold = (tmp1Gold, tmp2Gold).zipped.map(_+_)

    val tmp1 = Execute(n)[Array[Float]](mvAlpha, A, x, alpha)._1
    val tmp2 = Execute(n)[Array[Float]](mvAlpha, B, x, beta)._1
    val y    = Execute(n)[Array[Float]](vecAdd, tmp1, tmp2)._1

    assertArrayEquals(tmp1Gold, tmp1, 0.001f)
    assertArrayEquals(tmp2Gold, tmp2, 0.001f)
    assertArrayEquals(yGold, y, 0.001f)
  }

  @Test
  def gesummv2(): Unit = {
    // y = A . x * alpha + B . x * beta
    val n = 128

    val alpha = 2.0f
    val beta = 1.5f
    val x = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val A = Array.fill(n, n)(util.Random.nextInt(5).toFloat)
    val B = Array.fill(n, n)(util.Random.nextInt(5).toFloat)

    val tmp1Gold = Utils.matrixVector(A, x, alpha)
    val tmp2Gold = Utils.matrixVector(B, x, beta)
    val yGold = (tmp1Gold, tmp2Gold).zipped.map(_+_)

    val gesummv = GESUMMV.fused

    val y = Execute(n)[Array[Float]](gesummv, A, B, x, alpha, beta)._1

    assertArrayEquals(yGold, y, 0.001f)
  }

  @Test
  def gesummv3(): Unit = {
    // y = A . x * alpha + B . x * beta
    val n = 128

    val alpha = 2.0f
    val beta = 1.5f
    val x = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val A = Array.fill(n, n)(util.Random.nextInt(5).toFloat)
    val B = Array.fill(n, n)(util.Random.nextInt(5).toFloat)

    val tmp1Gold = Utils.matrixVector(A, x, alpha)
    val tmp2Gold = Utils.matrixVector(B, x, beta)
    val yGold = (tmp1Gold, tmp2Gold).zipped.map(_+_)

    val gesummv = GESUMMV.simpleUserFun

    val y = Execute(n)[Array[Float]](gesummv, A, B, x, alpha, beta)._1

    assertArrayEquals(yGold, y, 0.001f)
  }

  @Test
  def gesummvKepler(): Unit = {

    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)


    // y = A . x * alpha + B . x * beta
    val n = 1024

    val alpha = 2.0f
    val beta = 1.5f
    val x = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val A = Array.fill(n, n)(util.Random.nextInt(5).toFloat)
    val B = Array.fill(n, n)(util.Random.nextInt(5).toFloat)

    val tmp1Gold = Utils.matrixVector(A, x, alpha)
    val tmp2Gold = Utils.matrixVector(B, x, beta)
    val yGold = (tmp1Gold, tmp2Gold).zipped.map(_+_)

    val stride = 128

    val gesummv = GESUMMV.fusedOptimised

    val y = Execute(stride, stride*n, (true, true))[Array[Float]](gesummv, A, B, x, alpha, beta)._1

    assertArrayEquals(yGold, y, 0.001f)
  }

  // this is missing in polybench-gpu
  @Test
  def gemver(): Unit = {
    // A = A + u1.v1^T + u2v2^T
    // x = beta*A^T.y + z
    // w = alpha*A.x
    val n = 128

    val A = Array.fill(n, n)(util.Random.nextInt(5).toFloat)
    val y = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val z = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val u1 = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val u2 = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val v1 = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val v2 = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val alpha = 1.5f
    val beta = 2.5f

    val u1v1gold = u1.map(x => v1.map(_*x))
    val u2v2gold = u2.map(x => v2.map(_*x))

    val aGold = Utils.add(Utils.add(u1v1gold, u2v2gold), A)
    val xGold = Utils.matrixVector(aGold.transpose, y, z, beta, 1.0f)
    val wGold = Utils.matrixVector(aGold, xGold, alpha)

    val outerProduct = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (a, b) => MapGlb(fun(x => MapSeq(fun(y => mult(x, y))) $ b)) $ a
    )

    val (u1v1, _) = Execute(n)[Array[Float]](outerProduct, u1, v1)
    val (u2v2, _) = Execute(n)[Array[Float]](outerProduct, u2, v2)
    val (partialSum, _) = Execute(n)[Array[Float]](matrixAdd, u1v1.grouped(n).toArray, u2v2.grouped(n).toArray)
    val (newA, _) = Execute(n)[Array[Float]](matrixAdd, A, partialSum.grouped(n).toArray)
    val (x, _) = Execute(n)[Array[Float]](gemvTransposed, newA.grouped(n).toArray, y, z, beta, 1.0f)
    val (w, _) = Execute(n)[Array[Float]](mvAlpha, newA.grouped(n).toArray, x, alpha)

    assertArrayEquals(u1v1gold.flatten, u1v1, 0.001f)
    assertArrayEquals(u2v2gold.flatten, u2v2, 0.001f)
    assertArrayEquals(aGold.flatten, newA, 0.001f)
    assertArrayEquals(xGold, x, 0.001f)
    assertArrayEquals(wGold, w, 0.001f)
  }

  @Test
  def gemv(): Unit = {
    val n = 256
    val m = 128

    val alpha = 1.5f
    val beta = 2.5f
    val vectorX = Array.fill(m)(util.Random.nextInt(5).toFloat)
    val vectorY = Array.fill(n)(util.Random.nextInt(5).toFloat)
    val matrix = Array.fill(n, m)(util.Random.nextInt(5).toFloat)

    val gold = Utils.matrixVector(matrix, vectorX, vectorY, alpha, beta)

    val (result, _) = Execute(n)[Array[Float]](gemvKernel, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, result, 0.001f)
  }

  @Test
  def gemm(): Unit = {
    // C=alpha.A.B+beta.C
    val n = 128
    val m = 256
    val k = 64

    val A = Array.fill(n, k)(util.Random.nextInt(5).toFloat)
    val B = Array.fill(k, m)(util.Random.nextInt(5).toFloat)
    val C = Array.fill(n, m)(util.Random.nextInt(5).toFloat)
    val alpha = 1.5f
    val beta = 0.5f

    val AB = Utils.matrixMatrixMultiply(A, B)
    val gold = (AB, C).zipped.map((x, y) => (x, y).zipped.map((x, y) => x * alpha + y * beta))

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), K),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      Float,
      Float,
      (A, B, C, alpha, beta) => {
        MapGlb(fun( aRow =>
          Join() o  MapSeq(fun( bCol =>
            toGlobal(MapSeq(id)) o
            toPrivate(MapSeq(fun(x => multAndSumUp(x, beta, Get(bCol, 1))))) o
            toPrivate(MapSeq(fun(x => mult(x, alpha)))) o
            ReduceSeq(fun((acc, y) =>
              multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
            ), 0.0f) $ Zip(Get(aRow, 0), Get(bCol, 0))
          )) $ Zip(Transpose() $ B, Get(aRow, 1))
        )) $ Zip(A, C)
      })

    val (res, _) = Execute(n)[Array[Float]](f, A, B, C, alpha, beta)

    assertArrayEquals(gold.flatten, res, 0.001f)
  }

  @Test
  def atax(): Unit = {
    // y = A^T.A.x
    val n = 128
    val m = 256
    val x = Array.fill(m)(util.Random.nextInt(5).toFloat)
    val A = Array.fill(n, m)(util.Random.nextInt(5).toFloat)

    val axGold = Utils.matrixVector(A, x)
    val gold = Utils.matrixVector(A.transpose, axGold)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      ArrayTypeWSWC(Float, K),
      (matrix, vector) =>
        MapGlb(fun(row => toGlobal(MapSeq(id)) o
          ReduceSeq(fun((acc, y) =>
            multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
          ), 0.0f) $ Zip(row, vector)
        )) o Transpose() $ matrix
    )

    val (ax, _) = Execute(n)[Array[Float]](mv, A, x)
    val (atax, _) = Execute(n)[Array[Float]](f, A, ax)

    assertArrayEquals(gold, atax, 0.001f)
  }

}
