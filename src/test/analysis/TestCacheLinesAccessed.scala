package analysis

import analysis.CacheLinesAccessed.DeviceConfig
import ir.ArrayType
import ir.ast.{asScalar, asVector, fun}
import opencl.ir._
import opencl.ir.pattern.{MapLcl, toGlobal, toLocal}
import org.junit.Assert.assertEquals
import org.junit.Test


class TestCacheLinesAccessed {
  val maliConfig: DeviceConfig = DeviceConfig(
    nCores = 12,
    nParQuadsPerCore = 3, // 12*3*4 = 144 simultaneous threads*/
    cacheLineSizeInBytes = 64, // 512 bits
    localMemIsGlobal = true
  )

  /**
   * As many threads as values, way above Mali's parallelization capacity
   */
  @Test
  def t0_naive(): Unit = {
    val N = 1024

    val f = fun(ArrayType(Float, N), x =>
      MapLcl(0)(toGlobal(id)) $ x)

    val cacheUsageEfficiency = CacheLinesAccessed(f, localSizes = (N,1,1), globalSizes = (N,1,1),
      maliConfig, verbose = true)
    assertEquals(128, cacheUsageEfficiency, 0.01f)
  }

  /**
   * Half as many threads as values, still above Mali's parallelization capacity
   */
  @Test
  def t1_halfAsManyThreadsAsValues(): Unit = {
    val N = 1024

    val f = fun(ArrayType(Float, N), x =>
      MapLcl(0)(toGlobal(id)) $ x)

    val cacheUsageEfficiency = CacheLinesAccessed(f, localSizes = (512,1,1), globalSizes = (512,1,1),
      maliConfig, verbose = true)
    assertEquals(128, cacheUsageEfficiency, 0.01f)
  }

  /**
   * Making sure an extra MapLcl that doesn't read global mem doesn't affect the metric
   */
  @Test
  def t2_halfAsManyThreadsAsValuesWithExtraMapLcl(): Unit = {
    val N = 1024

    val f = fun(ArrayType(Float, N), x =>
      MapLcl(0)(toGlobal(id)) o
      MapLcl(0)(toLocal(id)) $ x)

    val cacheUsageEfficiency = CacheLinesAccessed(f, localSizes = (512,1,1), globalSizes = (512,1,1),
      maliConfig, verbose = true)
    assertEquals(128, cacheUsageEfficiency, 0.01f)
  }

  /**
   * Vectorized reads improving cache usage (full 512 bits of one line).
   * Still too many threads with 256 vectors to process;
   * First, 144 threads will process 144 vectors;
   * then, 112 other threads will process remaining vectors;
   * other threads will not be scheduled.
   */
  @Test
  def t3_vectorizedWithTooManyThreads(): Unit = {
    val N = 1024

    val f = fun(ArrayType(Float, N), x =>
      MapLcl(0)(toGlobal(id)) o asScalar() o
        MapLcl(0)(toLocal(idF4)) o asVector(4) $ x)

    val cacheUsageEfficiency = CacheLinesAccessed(f, localSizes = (512,1,1), globalSizes = (512,1,1),
      maliConfig, verbose = true)
    assertEquals(2048, cacheUsageEfficiency, 0.01f)
  }

  /**
   * As many threads as values, but still above Mali's parallel capacity.
   * First, 144 threads will process 144 vectors;
   * then, 112 other threads will process remaining vectors;
   * other threads will not be scheduled.
   */
  @Test
  def t4_vectorizedWithAsManyThreadsAsVectorValues(): Unit = {
    val N = 1024

    val f = fun(ArrayType(Float, N), x =>
      MapLcl(0)(toGlobal(id)) o asScalar() o
        MapLcl(0)(toLocal(idF4)) o asVector(4) $ x)

    val cacheUsageEfficiency = CacheLinesAccessed(f, localSizes = (256,1,1), globalSizes = (256,1,1),
      maliConfig, verbose = true)
    assertEquals(2048, cacheUsageEfficiency, 0.01f)
  }

  /**
   * HALF as many threads as values, but it doesn't affect the metric since the
   * number of threads is still above Mali's parallel capacity.
   * First, 128 threads will process 128 vectors;
   * then, the same 128 threads will process remaining vectors.
   */
  @Test
  def t5_vectorizedWithHalfAsManyThreadsAsVectorValues(): Unit = {
    val N = 1024

    val f = fun(ArrayType(Float, N), x =>
      MapLcl(0)(toGlobal(id)) o asScalar() o
        MapLcl(0)(toLocal(idF4)) o asVector(4) $ x)
    val cacheUsageEfficiency = CacheLinesAccessed(f, localSizes = (128,1,1), globalSizes = (128,1,1),
      maliConfig, verbose = true)
    assertEquals(2048, cacheUsageEfficiency, 0.01f)
  }

  /**
   * As many threads as Mali supports at any one time.
   * First, 144 threads will process 144 vectors;
   * then, the same 112 threads will process remaining 112 vectors,
   * while 32 threads remain idle.
   */
  @Test
  def t6_vectorizedWithAsManyThreadsAsMaliSupportsInPar(): Unit = {
    val N = 1024

    val f = fun(ArrayType(Float, N), x =>
      MapLcl(0)(toGlobal(id)) o asScalar() o
        MapLcl(0)(toLocal(idF4)) o asVector(4) $ x)
    val cacheUsageEfficiency = CacheLinesAccessed(f, localSizes = (144,1,1), globalSizes = (144,1,1),
      maliConfig, verbose = true)
    assertEquals(2048, cacheUsageEfficiency, 0.01f)
  }
  /**
   * One thread too little to perform the job in 2 sequential iterations.
   * First, 127 threads will process 127 vectors;
   * then, the same 127 threads will process 127 vectors more;
   * finally, 2 thread process the remaining 2 out of 256 vectors,
   * while 125 threads remain idle.
   */
  @Test
  def t7_vectorizedWithOneThreadLittle(): Unit = {
    val N = 1024

    val f = fun(ArrayType(Float, N), x =>
      MapLcl(0)(toGlobal(id)) o asScalar() o
        MapLcl(0)(toLocal(idF4)) o asVector(4) $ x)
    val cacheUsageEfficiency = CacheLinesAccessed(f, localSizes = (127,1,1), globalSizes = (127,1,1),
      maliConfig, verbose = true)
    assertEquals(1365.3333740234375, cacheUsageEfficiency, 0.01f)
  }
}