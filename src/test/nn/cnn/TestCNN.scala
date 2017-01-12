package lift.nn.cnn

/**
  * Created by nm on 09/01/17.
  */

import ir.ArrayType
import ir.ast.{Slide2D, Transpose, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.executor.Executor
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.{AfterClass, BeforeClass}

object TestCNN {
  @BeforeClass def before(): Unit = {

    Executor.loadLibrary()
    println("Initialize the executor")
    val intellij_path = System.getProperty("user.dir") + "/../../src/test/nn/cnn"
    if (java.nio.file.Files.exists(java.nio.file.Paths.get(intellij_path)))
    // Use GPU on the development machine
      Executor.init(1, 0)
    else
    // Use GPU on the testing machine
      Executor.init(1, 0)
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
    ArrayType(ArrayType(Float, kernel_xdim), kernel_ydim),
    (X, K) => {
      Map(fun((pass_band) => {
        Map(ReduceSeq(add, 0.0f)) o Transpose() o
        Map(fun((pass_row, kernel_row) => {
          Map(fun((window_row) => {
            ReduceSeq(add, 0.0f) o Map(mult) $ Zip(window_row, kernel_row)
          }))
        })) $ Zip(pass_band, K)
      })) o Slide2D(kernel_w, 1, kernel_h, 1) $ X
    }
  )

}
