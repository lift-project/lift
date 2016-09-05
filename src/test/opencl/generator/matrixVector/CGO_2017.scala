package opencl.generator.matrixVector

import apart.arithmetic.SizeVar
import ir._
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

object CGO_2017 {
  @BeforeClass def before(): Unit =
    Executor.loadAndInit()

  @AfterClass def after(): Unit =
    Executor.shutdown()
}

class CGO_2017 {

  val N = SizeVar("N")
  val M = SizeVar("M")

  val n = 256
  val m = 256

  val alpha = 1.5f
  val beta = 2.5f
  val vectorX = Array.fill(m)(util.Random.nextInt(5).toFloat)
  val vectorY = Array.fill(n)(util.Random.nextInt(5).toFloat)
  val matrix = Array.fill(n, m)(util.Random.nextInt(5).toFloat)

  val gold = Utils.matrixVector(matrix, vectorX, vectorY, alpha, beta)

  @Test
  def clblas_gemv_kepler_N(): Unit = {

  }

  @Test
  def clblas_gemv_kepler_T(): Unit = {

  }

  @Ignore
  @Test
  def clblas_gemv_hawaii_N(): Unit = {

  val f = fun(
    ArrayType(ArrayType(Float, N), M),
    ArrayType(Float, N),
    ArrayType(Float,M),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) =>

      MapWrg(1)(
        MapWrg(0)(fun(x =>
          MapSeq(
            Join() o MapLcl(1)(
              Join() o MapLcl(0)(
                MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))
              ) o Split(2) ) o Split(16)
          ) o
            toLocal(MapSeq(MapLcl(1)(MapLcl(0)(id)))) o
          ReduceSeq(fun((acc, next) =>
            MapLcl(0)(Join() o MapLcl(1)(ReduceSeq(add, 0.0f)) o Split(4)) $ next._0),
          Value(0.0f, ArrayType(ArrayType(Float, 8), 16))) $ Zip(x, Split(32) $ vectorX)
        )) o Tile(16, 32)
      ) o Split(M) $ matrix
    )

    val (result: Array[Float], _) =
      Execute(8, 8, m/2, 8, (true, true))(f, matrix, vectorX, vectorY, alpha, beta)

    println(f.body.t)

    assertArrayEquals(gold, result, 0.001f)
  }

  @Test
  def clblas_gemv_hawaii_T(): Unit = {

  }

  @Test
  def clblast_gemv_N(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), M),
      ArrayType(Float, N),
      ArrayType(Float,M),
      Float,
      Float,
      (matrix, vectorX, vectorY, alpha, beta) =>
        Join() o MapWrg(fun( matChunk =>

          MapSeq(
            toGlobal(fun(y =>
              MapLcl(fun(x =>
                add(
                  toPrivate(mult)(x._0, alpha),
                  toPrivate(mult)(x._1, beta)
                )
              )) $ Zip(y, Map(Get(1)) $ matChunk)
            ))
          ) o
            ReduceSeq(fun((acc, next) =>
              Let(localX =>
                Join() o MapLcl(fun(x => ReduceSeq(fun((acc2, next2) =>
                  multAndSumUp(acc2, Get(next2, 0), Get(next2, 1)))
                  , Get(x, 0)) $ Zip(Get(x, 1), localX))) $ Zip(acc, Get(next, 0))
              )  o toLocal(MapLcl(id)) $ Get(next, 1)),

              MapLcl(id) $ Value(0.0f, ArrayType(Float, 64)))
            $ Zip(Transpose() o Map(Split(64) o Get(0)) $ matChunk, Split(64) $ vectorX)
        )) o Split(64) $ Zip(matrix, vectorY)
    )

    val (result: Array[Float], _) =
      Execute(64, n, (true, true))(f, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, result, 0.001f)
  }

  @Test
  def clblast_gemv_T(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(Float, N),
      ArrayType(Float,M),
      Float,
      Float,
      (matrix, vectorX, vectorY, alpha, beta) =>
        Join() o MapWrg(fun( matChunk =>

          MapSeq(
            toGlobal(fun(y =>
              MapLcl(fun(x =>
                add(
                  toPrivate(mult)(x._0, alpha),
                  toPrivate(mult)(x._1, beta)
                )
              )) $ Zip(y, Map(Get(1)) $ matChunk)
            ))
          ) o
            ReduceSeq(fun((acc, next) =>
              Let(localX =>
                Join() o MapLcl(fun(x => ReduceSeq(fun((acc2, next2) =>
                  multAndSumUp(acc2, Get(next2, 0), Get(next2, 1)))
                  , Get(x, 0)) $ Zip(Get(x, 1), localX))) $ Zip(acc, Get(next, 0))
              )  o toLocal(MapLcl(id)) $ Get(next, 1)),

              MapLcl(id) $ Value(0.0f, ArrayType(Float, 64)))
            $ Zip(Transpose() o Map(Split(64) o Get(0)) $ matChunk, Split(64) $ vectorX)
        )) o Split(64) $ Zip(Transpose() $ matrix, vectorY)
    )

    val (result: Array[Float], _) =
      Execute(64, n, (true, true))(f, matrix.transpose, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, result, 0.001f)

  }
}
