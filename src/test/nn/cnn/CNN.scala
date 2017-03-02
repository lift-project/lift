package nn.cnn

/**
  * Created by nm on 09/01/17.
  */

import ir.ArrayType
import ir.ast.{Map, Join, Slide2D, Split, Transpose, UserFun, Zip, λ}
import lift.arithmetic.SizeVar
import opencl.ir._
import opencl.ir.pattern._
import nn.{Shape, Tile}

object CNN {
  val kernel_xdim = SizeVar("kernel_xdim")
  val kernel_ydim = SizeVar("kernel_ydim")
  val input_xdim = SizeVar("input_xdim")
  val input_ydim = SizeVar("input_ydim")
  val layer_idim = SizeVar("layer_idim")
  val layer_odim = SizeVar("layer_odim")
  val input_channels = SizeVar("input_channels")
  val output_channels = SizeVar("output_channels")
  val n_inputs = SizeVar("n_inputs")
  val n_batches = SizeVar("n_batches")

  /* Sequential layer */
  def Seq(kernel_h: Int, kernel_w: Int, activation_f: UserFun) = λ(
    ArrayType(ArrayType(ArrayType(ArrayType(Float, output_channels), input_channels), kernel_w), kernel_h),
    ArrayType(Float, output_channels),
    ArrayType(ArrayType(ArrayType(ArrayType(Float, input_channels), input_xdim), input_ydim), n_inputs),
    (K, B, X) => {
      MapLcl(0)(λ((single_input) => {
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
  )

  /* Parallel layer */
  def Par(kernel_h: Int, kernel_w: Int, dim: Shape, activation_f: UserFun) = λ(
    ArrayType(ArrayType(ArrayType(ArrayType(Float, output_channels), input_channels), kernel_w), kernel_h),
    ArrayType(Float, output_channels),
    ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float,
      input_channels), dim.x), dim.y), n_inputs), n_batches),
    (K, B, X) => {
      MapWrg(0)(λ((input_batch) => {
        MapWrg(1)(λ((input_single) => {
          MapWrg(2)(λ((input_single_wrapped) => {
            Map(λ((pass_strip) => {
              MapLcl(2)(λ((pass_window) => {
                Join() o
                  MapSeq(λ((weighted_window_per_out_ch, b_per_out_ch) => {
                    // Reduce weighted pass window separately for each output
                    MapSeq(toGlobal(activation_f)) o ReduceSeq(add, id(b_per_out_ch)) $ weighted_window_per_out_ch
                  })) o λ((weighted_window_across_out_chs) => Zip(weighted_window_across_out_chs, B)) o Transpose() o
                  MapLcl(1)(λ((window_row, kernel_row) => {
                    Join() o
                      MapSeq(λ((weighted_row_per_out_ch) => {
                        // Reduce weighted pass window rows separately for each output
                        MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) $ weighted_row_per_out_ch
                      })) o
                      /* Map below returns (n_out_chs, n_els) */
                      Map(λ((x_out_ch) => {
                        MapLcl(0)(λ((x_el_in_chs, k_el_in_chs) => {
                          Join() o
                            MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $
                            Zip(x_el_in_chs, k_el_in_chs)
                        })) $ Zip(x_out_ch, kernel_row)
                      })) o /* (n_els, n_in_chs, n_out_chs) -> (n_out_chs, n_els, n_in_chs) */
                      Transpose() o Map(Transpose()) $ window_row
                  })) $ Zip(pass_window, K)
              })) $ pass_strip
            })) $ input_single_wrapped
          })) o Split(dim.y) $ input_single // Wrap input in a one-element array
        })) $ input_batch
      })) o MapSeq(MapSeq(Slide2D(kernel_h, 1, kernel_w, 1))) $ X
    }
  )
}

/*class CNN {
}
*/