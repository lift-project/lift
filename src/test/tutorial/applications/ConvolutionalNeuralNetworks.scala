package tutorial.applications

import ir.ArrayType
import ir.ast.{FunDecl, Join, Slide2D, Transpose, UserFun, Zip, λ}
import lift.arithmetic.SizeVar
import opencl.executor.TestWithExecutor
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import opencl.ir._
import org.junit.Test


/**
  * Here is a test class showing an example of a convolutional neural network
  */

object ConvolutionalNeuralNetworks extends TestWithExecutor

class ConvolutionalNeuralNetworks
{
  @Test def simpleDotProduct(): Unit = {
    val kernel_xdim_SV = SizeVar("kernel_xdim_SV")
    val kernel_ydim_SV = SizeVar("kernel_ydim_SV")
    val input_xdim_SV = SizeVar("input_xdim_SV")
    val input_ydim_SV = SizeVar("input_ydim_SV")
    val layer_idim_SV = SizeVar("layer_idim_SV")
    val layer_odim_SV = SizeVar("layer_odim_SV")
    val in_channels_SV = SizeVar("in_channels_SV")
    val out_channels_SV = SizeVar("out_channels_SV")
    val n_inputs_SV = SizeVar("n_inputs_SV")
    val n_batches_SV = SizeVar("n_batches_SV")
  
  def Seq(kernel_h: Int, kernel_w: Int, activation_f: UserFun): FunDecl = λ(
    ArrayType(ArrayType(ArrayType(ArrayType(Float, out_channels_SV), in_channels_SV), kernel_w), kernel_h),
    ArrayType(Float, out_channels_SV),
    ArrayType(ArrayType(ArrayType(ArrayType(Float, in_channels_SV), input_xdim_SV), input_ydim_SV), n_inputs_SV),
    (K, B, X) => {
      MapSeq(λ((single_input) => {
        MapSeq(λ((pass_strip) => {
          MapSeq(λ((pass_window) => { Join() o
            MapSeq(λ((weighted_window_per_out_ch, b_per_out_ch) => { // Reduce weighted pass window separately for each output
              MapSeq(toGlobal(activation_f)) o ReduceSeq(add, id(b_per_out_ch)) $ weighted_window_per_out_ch
            })) o λ((weighted_window_across_out_chs) => Zip(weighted_window_across_out_chs, B)) o Transpose() o
            MapSeq(λ((window_row, kernel_row) => { Join() o
              MapSeq(λ((weighted_row_per_out_ch) => { // Reduce weighted pass window rows separately for each output
                MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) $ weighted_row_per_out_ch
              })) o Transpose() o
              MapSeq(λ((x_el_in_chs, k_el_in_chs) => { Join() o
                MapSeq(λ((k_el_out_ch) => { // Reduce input channels of each element separately for each output
                  MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(x_el_in_chs, k_el_out_ch)
                })) o Transpose() $ k_el_in_chs
              })) $ Zip(window_row, kernel_row)
            })) $ Zip(pass_window, K)
          })) $ pass_strip
        })) o Slide2D(kernel_h, 1, kernel_w, 1) $ single_input
      })) $ X
    }
  }
}
