package opencl.generator.matrixVector

import apart.arithmetic.{ArithExpr, SizeVar}
import ir._
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}
import rewriting.InferNDRange

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

  val alpha = 1.0f
  val beta = 0.0f
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

  @Test
  def clblas_gemv_hawaii_N(): Unit = {

  val f = fun(
    ArrayType(ArrayType(Float, N), M),
    ArrayType(Float, N),
    ArrayType(Float,M),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) =>
      Join() o MapWrg(MapLcl(fun( t =>
        MapSeq(fun( x => multAndSumUp(mult(alpha, x), Get(t, 1), beta))) o
          toGlobal(MapSeq(id)) o
          ReduceSeq(fun((acc, y) =>
            multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
          ), 0.0f) $ Zip(vectorX, Get(t, 0)))
      )) o Split(16) $ Zip(matrix, vectorY)
    )

    val (result: Array[Float], _) =
      Execute(64, n)(f, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, result, 0.001f)
  }

  @Test
  def clblas_gemv_hawaii_T(): Unit = {

  }

  @Test
  def clblast_gemv_kepler_N(): Unit = {

  }

  @Test
  def clblast_gemv_kepler_T(): Unit = {

  }

  @Test
  def clblast_gemv_hawaii_N(): Unit = {

  val f = fun(
    ArrayType(ArrayType(Float, N), M),
    ArrayType(Float, N),
    ArrayType(Float,M),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) =>
      Join() o MapWrg(fun( matChunk =>

        MapSeq(toGlobal(MapLcl(id))) o
        ReduceSeq(fun((acc, next) =>
          Let(localX =>
          Join() o MapLcl(fun(x => ReduceSeq(fun((acc2, next2) =>
            multAndSumUp(acc2, Get(next2, 0), Get(next2, 1)))
            , Get(x, 0)) $ Zip(Get(x, 1), localX))) $ Zip(acc, Get(next, 0))
        )  o toLocal(MapLcl(id)) $ Get(next, 1)),

          MapLcl(id) $ Value(0.0f, ArrayType(Float, 64))) /*o
          MapLcl(fun( t =>
            ReduceSeq(fun((acc, y) =>
              multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
            ), toPrivate(id) $ 0.0f) $ Zip(vectorX, t)

            //
        ))*/ $ Zip(Transpose() o Map(Split(64)) $ matChunk, Split(64) $ vectorX)
      )) o Split(64) $ matrix
  )

    val (result: Array[Float], _) =
      Execute(64, n, (true, true))(f, matrix, vectorX, vectorY, alpha, beta)

    val code = """
     |typedef float real;
     |typedef real realVFR;
     |inline realVFR LoadMatrixAVFR(const __global realVFR* restrict agm, const int x, const int y,
     |                              const int a_ld) {
     |  return agm[a_ld*y + x];
     |}
     |__attribute__((reqd_work_group_size(64, 1, 1)))
     |__kernel void KERNEL(
     |      const __global realVFR* restrict agm,
     |      const __global real* restrict xgm, __global real* ygmi, const real alpha, const real beta,
     |      __global real* ygmo, const int n, const int a_ld) {
     |const int parameter = 0;
     |const int y_offset = 0;
     |const int y_inc = 1;
     |const int a_offset = 0;
     |const int x_offset = 0;
     |const int x_inc = 1;
     |  __local real xlm[64];
     |
     |
     |  real acc[1];
     |#pragma unroll
     |  for (int w=0; w<1; ++w) {
     |    acc[w] = 0.0f;
     |  }
     |
     |
     |  for (int kwg=0; kwg<n; kwg+=64) {
     |
     |
     |    const int lid = get_local_id(0);
     |    xlm[lid] = xgm[(kwg + lid)*x_inc + x_offset];
     |
     |
     |    barrier(CLK_LOCAL_MEM_FENCE);
     |
     |
     |#pragma unroll
     |    for (int kl=0; kl<64/1; ++kl) {
     |      const int k = (kwg/1) + kl;
     |#pragma unroll
     |      for (int w=0; w<1; ++w) {
     |        const int gid = 1*get_global_id(0) + w;
     |        realVFR avec = LoadMatrixAVFR(agm, k, gid, a_ld/1);
     |
     |          acc[w] += xlm[1*kl+0] * avec;
     |# 709 "clblast_gemv_suzuka.c"
     |      }
     |    }
     |
     |
     |    barrier(CLK_LOCAL_MEM_FENCE);
     |  }
     |
     |
     |#pragma unroll
     |  for (int w=0; w<1; ++w) {
     |    const int gid = 1*get_global_id(0) + w;
     |    real yval = ygmi[gid*y_inc + y_offset];
     |    ygmo[gid*y_inc + y_offset] = alpha*acc[w] + beta*yval;
     |  }
     |}""".stripMargin

//    val (result: Array[Float], _) =
//      Execute(64, n)(code, f, matrix, vectorX, vectorY, alpha, beta)

    assertArrayEquals(gold, result, 0.001f)
  }

  @Test
  def clblast_gemv_hawaii_T(): Unit = {

  }
}
