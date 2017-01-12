package lift.nn.cnn

/**
  * Created by nm on 09/01/17.
  */

import ir.ArrayType
import ir.ast.{Join, Slide2D, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestCNN {
  @BeforeClass def before(): Unit = {

    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init(0, 0)
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestCNN {
  val kernel_xdim = SizeVar("kernel_xdim")
  val kernel_ydim = SizeVar("kernel_ydim")
  val input_xdim = SizeVar("input_xdim")
  val input_ydim = SizeVar("input_ydim")

  def convolute2D(kernel_w: Int, kernel_h: Int) = fun(
    ArrayType(ArrayType(Float, input_xdim), input_ydim),
    ArrayType(ArrayType(Float, kernel_w), kernel_h),
    (X, K) => {
      MapSeq(fun((pass_horizontal) => {
        MapSeq(fun((window) => {
          toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join() o
          MapSeq(fun((window_row, kernel_row) => {
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(window_row, kernel_row)
          })) $ Zip(window, K)
        })) $ pass_horizontal
      })) o Slide2D(kernel_h, 1, kernel_w, 1) $ X
    }
  )

  // Test values
  val input_X = Array(Array(0.0f,  1.0f,  2.0f,  3.0f,  4.0f,  5.0f,  6.0f,  7.0f),
                      Array(8.0f,  9.0f,  10.0f, 11.0f, 12.0f, 13.0f, 14.0f, 15.0f),
                      Array(16.0f, 17.0f, 18.0f, 19.0f, 20.0f, 21.0f, 22.0f, 23.0f),
                      Array(24.0f, 25.0f, 26.0f, 27.0f, 28.0f, 29.0f, 30.0f, 31.0f),
                      Array(32.0f, 33.0f, 34.0f, 35.0f, 36.0f, 37.0f, 38.0f, 39.0f),
                      Array(40.0f, 41.0f, 42.0f, 43.0f, 44.0f, 45.0f, 46.0f, 47.0f),
                      Array(48.0f, 49.0f, 50.0f, 51.0f, 52.0f, 53.0f, 54.0f, 55.0f),
                      Array(56.0f, 57.0f, 58.0f, 59.0f, 60.0f, 61.0f, 62.0f, 63.0f))

  val input_K = Array(Array(1.0f, 3.0f, 5.0f),
                      Array(7.0f, 9.0f, 11.0f))

  val gold = Array(Array(260.0f,  296.0f,  332.0f,  368.0f,  404.0f,  440.0f),
                   Array(548.0f,  584.0f,  620.0f,  656.0f,  692.0f,  728.0f),
                   Array(836.0f,  872.0f,  908.0f,  944.0f,  980.0f,  1016.0f),
                   Array(1124.0f, 1160.0f, 1196.0f, 1232.0f, 1268.0f, 1304.0f),
                   Array(1412.0f, 1448.0f, 1484.0f, 1520.0f, 1556.0f, 1592.0f),
                   Array(1700.0f, 1736.0f, 1772.0f, 1808.0f, 1844.0f, 1880.0f),
                   Array(1988.0f, 2024.0f, 2060.0f, 2096.0f, 2132.0f, 2168.0f))

  val precision: Float = 0.0001f

  @Test
  def Sanity_MLPSequential(): Unit = {
    val (lift_result: Array[Float], runtime) = Execute(1,1)(
      convolute2D(3, 2), input_X, input_K)

    println(f"\n1. Convolution sanity check.\n" +
            f"Runtime: $runtime%1.5f ms")
    println(lift_result.mkString(", "))
    println(gold.flatten.mkString(", "))

    assertArrayEquals(gold.flatten, lift_result, precision)
  }
}
