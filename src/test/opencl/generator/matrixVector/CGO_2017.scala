package opencl.generator.matrixVector

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.Test

object CGO_2017 extends TestWithExecutor

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
  def clblast_gemv_N(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float,M),
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
                Join() o MapLcl(fun(x =>
                  ReduceSeq(fun((acc2, next2) =>
                    multAndSumUp(acc2, Get(next2, 0), Get(next2, 1)))
                    , Get(x, 0)
                  ) $ Zip(Get(x, 1), localX))
                ) $ Zip(acc, Get(next, 0))
              )  o toLocal(MapLcl(id)) $ Get(next, 1)),

              MapLcl(id) $ Value(0.0f, ArrayTypeWSWC(Float, 64))
            ) $ Zip(Transpose() o Map(Split(64) o Get(0)) $ matChunk, Split(64) $ vectorX)
        )) o Split(64) $ Zip(matrix, vectorY)
    )

    val (result, _) = Execute(64, n, (true, true))[Array[Float]](f, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, result, 0.001f)
  }

  @Test
  def clblast_gemv_T(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float,M),
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

              MapLcl(id) $ Value(0.0f, ArrayTypeWSWC(Float, 64)))
            $ Zip(Transpose() o Map(Split(64) o Get(0)) $ matChunk, Split(64) $ vectorX)
        )) o Split(64) $ Zip(Transpose() $ matrix, vectorY)
    )

    val (result, _) = Execute(64, n, (true, true))[Array[Float]](f, matrix.transpose, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, result, 0.001f)

  }
}
