package ir.ast.nnet

import ir.ArrayType
import ir.ast.{FunDecl, Join, Slide2D, Transpose, UserFun, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}//{add,id,mult, Float}

object Conv {

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

  /* Sequential layer */
  def Seq(kernel_h: Int, kernel_w: Int, activation_f: UserFun): FunDecl = fun(
    ArrayType(ArrayType(ArrayType(ArrayType(Float, out_channels_SV), in_channels_SV), kernel_w), kernel_h),
    ArrayType(Float, out_channels_SV),
    ArrayType(ArrayType(ArrayType(ArrayType(Float, in_channels_SV), input_xdim_SV), input_ydim_SV), n_inputs_SV),
    (K, B, X) => {
      MapSeq(fun((single_input) => {
        MapSeq(fun((pass_strip) => {
          MapSeq(fun((pass_window) => { Join() o
            MapSeq(fun((weighted_window_per_out_ch, b_per_out_ch) => { // Reduce weighted pass window separately for each output
              MapSeq(toGlobal(activation_f)) o ReduceSeq(add, id(b_per_out_ch)) $ weighted_window_per_out_ch
            })) o fun((weighted_window_across_out_chs) => Zip(weighted_window_across_out_chs, B)) o Transpose() o
            MapSeq(fun((window_row, kernel_row) => { Join() o
              MapSeq(fun((weighted_row_per_out_ch) => { // Reduce weighted pass window rows separately for each output
                MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) $ weighted_row_per_out_ch
              })) o Transpose() o
              MapSeq(fun((x_el_in_chs, k_el_in_chs) => { Join() o
                MapSeq(fun((k_el_out_ch) => { // Reduce input channels of each element separately for each output
                  MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(x_el_in_chs, k_el_out_ch)
                })) o Transpose() $ k_el_in_chs
              })) $ Zip(window_row, kernel_row)
            })) $ Zip(pass_window, K)
          })) $ pass_strip
        })) o Slide2D(kernel_h, 1, kernel_w, 1) $ single_input
      })) $ X
    }
  )

  /* Test values */
  val input_X = Array(Array(Array(
    Array(Array(0.0f, 0.0f),   Array(1.0f, 1.0f),   Array(2.0f, 2.0f),   Array(3.0f, 3.0f),
      Array(4.0f, 4.0f),   Array(5.0f, 5.0f),   Array(6.0f, 6.0f),  Array(7.0f, 7.0f)),
    Array(Array(8.0f, 8.0f),   Array(9.0f, 9.0f),   Array(10.0f, 10.0f), Array(11.0f, 11.0f),
      Array(12.0f, 12.0f), Array(13.0f, 13.0f), Array(14.0f, 14.0f), Array(15.0f, 15.0f)),
    Array(Array(16.0f, 16.0f), Array(17.0f, 17.0f), Array(18.0f, 18.0f), Array(19.0f, 19.0f),
      Array(20.0f, 20.0f), Array(21.0f, 21.0f), Array(22.0f, 22.0f), Array(23.0f, 23.0f)),
    Array(Array(24.0f, 24.0f), Array(25.0f, 25.0f), Array(26.0f, 26.0f), Array(27.0f, 27.0f),
      Array(28.0f, 28.0f), Array(29.0f, 29.0f), Array(30.0f, 30.0f), Array(31.0f, 31.0f)),
    Array(Array(32.0f, 32.0f), Array(33.0f, 33.0f), Array(34.0f, 34.0f), Array(35.0f, 35.0f),
      Array(36.0f, 36.0f), Array(37.0f, 37.0f), Array(38.0f, 38.0f), Array(39.0f, 39.0f)),
    Array(Array(40.0f, 40.0f), Array(41.0f, 41.0f), Array(42.0f, 42.0f), Array(43.0f, 43.0f),
      Array(44.0f, 44.0f), Array(45.0f, 45.0f), Array(46.0f, 46.0f), Array(47.0f, 47.0f)),
    Array(Array(48.0f, 48.0f), Array(49.0f, 49.0f), Array(50.0f, 50.0f), Array(51.0f, 51.0f),
      Array(52.0f, 52.0f), Array(53.0f, 53.0f), Array(54.0f, 54.0f), Array(55.0f, 55.0f)),
    Array(Array(56.0f, 56.0f), Array(57.0f, 57.0f), Array(58.0f, 58.0f), Array(59.0f, 59.0f),
      Array(60.0f, 60.0f), Array(61.0f, 61.0f), Array(62.0f, 62.0f), Array(63.0f, 63.0f))),
    Array(
      Array(Array(0.0f, 0.0f),   Array(1.0f, 1.0f),   Array(2.0f, 2.0f),   Array(3.0f, 3.0f),
        Array(4.0f, 4.0f),   Array(5.0f, 5.0f),   Array(6.0f, 6.0f),  Array(7.0f, 7.0f)),
      Array(Array(8.0f, 8.0f),   Array(9.0f, 9.0f),   Array(10.0f, 10.0f), Array(11.0f, 11.0f),
        Array(12.0f, 12.0f), Array(13.0f, 13.0f), Array(14.0f, 14.0f), Array(15.0f, 15.0f)),
      Array(Array(16.0f, 16.0f), Array(17.0f, 17.0f), Array(18.0f, 18.0f), Array(19.0f, 19.0f),
        Array(20.0f, 20.0f), Array(21.0f, 21.0f), Array(22.0f, 22.0f), Array(23.0f, 23.0f)),
      Array(Array(24.0f, 24.0f), Array(25.0f, 25.0f), Array(26.0f, 26.0f), Array(27.0f, 27.0f),
        Array(28.0f, 28.0f), Array(29.0f, 29.0f), Array(30.0f, 30.0f), Array(31.0f, 31.0f)),
      Array(Array(32.0f, 32.0f), Array(33.0f, 33.0f), Array(34.0f, 34.0f), Array(35.0f, 35.0f),
        Array(36.0f, 36.0f), Array(37.0f, 37.0f), Array(38.0f, 38.0f), Array(39.0f, 39.0f)),
      Array(Array(40.0f, 40.0f), Array(41.0f, 41.0f), Array(42.0f, 42.0f), Array(43.0f, 43.0f),
        Array(44.0f, 44.0f), Array(45.0f, 45.0f), Array(46.0f, 46.0f), Array(47.0f, 47.0f)),
      Array(Array(48.0f, 48.0f), Array(49.0f, 49.0f), Array(50.0f, 50.0f), Array(51.0f, 51.0f),
        Array(52.0f, 52.0f), Array(53.0f, 53.0f), Array(54.0f, 54.0f), Array(55.0f, 55.0f)),
      Array(Array(56.0f, 56.0f), Array(57.0f, 57.0f), Array(58.0f, 58.0f), Array(59.0f, 59.0f),
        Array(60.0f, 60.0f), Array(61.0f, 61.0f), Array(62.0f, 62.0f), Array(63.0f, 63.0f)))))

  val input_b = Array(0.0f, 1.0f, 2.0f)

  val input_K = Array(Array(Array(Array(1.0f, 0.0f, 1.0f), Array(0.0f, 1.0f, 0.0f)),
    Array(Array(3.0f, 0.0f, 3.0f), Array(0.0f, 3.0f, 0.0f)),
    Array(Array(5.0f, 0.0f, 5.0f), Array(0.0f, 5.0f, 0.0f))),
    Array(Array(Array(7.0f, 0.0f, 7.0f), Array(0.0f, 7.0f, 0.0f)),
      Array(Array(9.0f, 0.0f, 9.0f), Array(0.0f, 9.0f, 0.0f)),
      Array(Array(11.0f, 0.0f, 11.0f), Array(0.0f, 11.0f, 0.0f))),
    Array(Array(Array(13.0f, 0.0f, 13.0f), Array(0.0f, 13.0f, 0.0f)),
      Array(Array(15.0f, 0.0f, 15.0f), Array(0.0f, 15.0f, 0.0f)),
      Array(Array(17.0f, 0.0f, 17.0f), Array(0.0f, 17.0f, 0.0f))))

  val gold = Array(Array(Array(
    Array(Array(1029f, 1029f+1, 1029f+2), Array(1110f, 1110f+1, 1110f+2), Array(1191f, 1191f+1, 1191f+2),
      Array(1272f, 1272f+1, 1272f+2), Array(1353f, 1353f+1, 1353f+2), Array(1434f, 1434f+1, 1434f+2)),
    Array(Array(1677f, 1677f+1, 1677f+2), Array(1758f, 1758f+1, 1758f+2), Array(1839f, 1839f+1, 1839f+2),
      Array(1920f, 1920f+1, 1920f+2), Array(2001f, 2001f+1, 2001f+2), Array(2082f, 2082f+1, 2082f+2)),
    Array(Array(2325f, 2325f+1, 2325f+2), Array(2406f, 2406f+1, 2406f+2), Array(2487f, 2487f+1, 2487f+2),
      Array(2568f, 2568f+1, 2568f+2), Array(2649f, 2649f+1, 2649f+2), Array(2730f, 2730f+1, 2730f+2)),
    Array(Array(2973f, 2973f+1, 2973f+2), Array(3054f, 3054f+1, 3054f+2), Array(3135f, 3135f+1, 3135f+2),
      Array(3216f, 3216f+1, 3216f+2), Array(3297f, 3297f+1, 3297f+2), Array(3378f, 3378f+1, 3378f+2)),
    Array(Array(3621f, 3621f+1, 3621f+2), Array(3702f, 3702f+1, 3702f+2), Array(3783f, 3783f+1, 3783f+2),
      Array(3864f, 3864f+1, 3864f+2), Array(3945f, 3945f+1, 3945f+2), Array(4026f, 4026f+1, 4026f+2)),
    Array(Array(4269f, 4269f+1, 4269f+2), Array(4350f, 4350f+1, 4350f+2), Array(4431f, 4431f+1, 4431f+2),
      Array(4512f, 4512f+1, 4512f+2), Array(4593f, 4593f+1, 4593f+2), Array(4674f, 4674f+1, 4674f+2))),
    Array(
      Array(Array(1029f, 1029f+1, 1029f+2), Array(1110f, 1110f+1, 1110f+2), Array(1191f, 1191f+1, 1191f+2),
        Array(1272f, 1272f+1, 1272f+2), Array(1353f, 1353f+1, 1353f+2), Array(1434f, 1434f+1, 1434f+2)),
      Array(Array(1677f, 1677f+1, 1677f+2), Array(1758f, 1758f+1, 1758f+2), Array(1839f, 1839f+1, 1839f+2),
        Array(1920f, 1920f+1, 1920f+2), Array(2001f, 2001f+1, 2001f+2), Array(2082f, 2082f+1, 2082f+2)),
      Array(Array(2325f, 2325f+1, 2325f+2), Array(2406f, 2406f+1, 2406f+2), Array(2487f, 2487f+1, 2487f+2),
        Array(2568f, 2568f+1, 2568f+2), Array(2649f, 2649f+1, 2649f+2), Array(2730f, 2730f+1, 2730f+2)),
      Array(Array(2973f, 2973f+1, 2973f+2), Array(3054f, 3054f+1, 3054f+2), Array(3135f, 3135f+1, 3135f+2),
        Array(3216f, 3216f+1, 3216f+2), Array(3297f, 3297f+1, 3297f+2), Array(3378f, 3378f+1, 3378f+2)),
      Array(Array(3621f, 3621f+1, 3621f+2), Array(3702f, 3702f+1, 3702f+2), Array(3783f, 3783f+1, 3783f+2),
        Array(3864f, 3864f+1, 3864f+2), Array(3945f, 3945f+1, 3945f+2), Array(4026f, 4026f+1, 4026f+2)),
      Array(Array(4269f, 4269f+1, 4269f+2), Array(4350f, 4350f+1, 4350f+2), Array(4431f, 4431f+1, 4431f+2),
        Array(4512f, 4512f+1, 4512f+2), Array(4593f, 4593f+1, 4593f+2), Array(4674f, 4674f+1, 4674f+2)))))

  def main(args: Array[String]): Unit = {
    val expr = Seq(3, 3, id)
    println(Compile(expr))
    //val (output, runtime) = Execute(1)[Array[Float]](expr, input_K, input_b, input_X)
    //println(output)
  }
}
