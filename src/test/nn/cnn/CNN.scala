package nn.cnn

/**
  * Created by nm on 09/01/17.
  */

import ir.ArrayType
import ir.ast.{Join, ReorderWithStride, Scatter, Slide2D, Split, Transpose, TransposeW, UserFun, Zip, λ}
import lift.arithmetic.SizeVar
import nn.TestUtils
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object CNN {
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

class CNN {
  /* SizeVars */
  val kernel_xdim = SizeVar("kernel_xdim")
  val kernel_ydim = SizeVar("kernel_ydim")
  val input_xdim = SizeVar("input_xdim")
  val input_ydim = SizeVar("input_ydim")
  val layer_idim = SizeVar("layer_idim")
  val layer_odim = SizeVar("layer_odim")
  val input_channels = SizeVar("input_channels")
  val output_channels = SizeVar("output_channels")
  val n_inputs = SizeVar("n_inputs")

  /* User functions */
  val LReLU = UserFun("ReLU", "x", "{ return(max(0.0f, x)); }", Float, Float)
  val Linear = id

  /* Sequential layer */
  def LLayerSeq(activation_f: UserFun) = λ(
    ArrayType(ArrayType(Float, layer_idim), layer_odim),
    ArrayType(Float, layer_odim),
    ArrayType(Float, layer_idim),
    (W_layer, b_layer, X) => {
      Join() o MapSeq(λ((ws_per_neuron, b_per_neuron) => {
        toGlobal(MapSeq(activation_f)) o
          ReduceSeq(add, id(b_per_neuron)) o
          MapSeq(mult) $
          Zip(X, ws_per_neuron)
      })) $ Zip(W_layer, b_layer)
    }
  )

  /* Parallel layer */
  def LLayer(activation_f: UserFun, tile_of_mults_size: Int, tile_of_inputs_size: Int,
             tile_of_neurons_size: Int, odim: Int, idim: Int) = λ(
    ArrayType(ArrayType(Float, layer_idim), odim),
    ArrayType(Float, odim),
    ArrayType(ArrayType(Float, layer_idim), n_inputs),
    (W, B, X) => {
      Join() o
        MapWrg(1)(λ((X_tile) => {
          Scatter(ReorderWithStride(odim / tile_of_neurons_size)) o Join() o
            MapWrg(0)(λ((ws_per_tile, bs_per_tile) => {
              MapLcl(1)(λ((X_single) => {
                MapLcl(0)(λ((b_per_neuron, partial_sums_per_neuron) => {
                  TransposeW() o
                    MapSeq(toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_per_neuron)) ) o
                    Transpose() $ partial_sums_per_neuron})) o
                  λ((partial_sums) => Zip(bs_per_tile, partial_sums)) o
                  Split(idim / tile_of_mults_size) o
                  MapLcl(0)(
                    toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
                  Split(tile_of_mults_size) o Join() o
                  MapSeq(λ((ws_per_neuron) => {ReorderStride(tile_of_mults_size) $ // or idim / tile_of_mults_size
                    Zip(X_single, ws_per_neuron)})) $ ws_per_tile})) $
                X_tile})) $ Zip(Split(tile_of_neurons_size) $ W, Split(tile_of_neurons_size) $ B)})) o
        Split(tile_of_inputs_size) $ X
    }
  )

  def LConvolute(kernel_h: Int, kernel_w: Int, activation_f: UserFun) = λ(
    ArrayType(ArrayType(ArrayType(ArrayType(Float, output_channels), input_channels), kernel_w), kernel_h),
    ArrayType(Float, output_channels),
    ArrayType(ArrayType(ArrayType(ArrayType(Float, input_channels), input_xdim), input_ydim), n_inputs),
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
  )

  val precision: Float = 1f

  //@Test
  def Sanity_CNN(): Unit = {
    val (lift_result: Array[Float], runtime) = Execute(1,1)(
      LConvolute(2, 3, Linear), input_K, input_b, input_X)

    println(f"\n1. Convolution sanity check.\n" +
            f"Runtime: $runtime%1.5f ms")

    val lift_result3d = TestUtils.group(lift_result, (gold.length, gold.head.length, gold.head.head.length))
    for ((gold2d, lift_result2d) <- gold zip lift_result3d) {
      println(lift_result2d.flatten.mkString(", "))
      println(gold2d.flatten.mkString(", "))
      for ((gold1d, lift_result1d) <- gold2d zip lift_result2d) {
        assertArrayEquals(gold1d, lift_result1d, precision)
      }
    }
  }

  //@Test
  def CNN_Seq(): Unit = {
    val (tf_X, tf_Wc, tf_Bc, tf_Wd, tf_Bd, tf_result) = load_datasets("/home/nm/tf_cnn/experiment/", 1)
    val (output_conv1_flat: Array[Float], runtime_conv1) =
      Execute(1, 1)(
        LConvolute(tf_Wc(0).length, tf_Wc(0).head.length, LReLU), tf_Wc(0), tf_Bc(0),
        TestUtils.group(tf_X(0), (28, 28, 1)))


    val (output_conv2_flat: Array[Float], runtime_conv2) =
      Execute(1, 1)(
        LConvolute(tf_Wc(1).length, tf_Wc(1).head.length, LReLU), tf_Wc(1), tf_Bc(1),
        TestUtils.group(output_conv1_flat, (28 - tf_Wc(0).length + 1, 28 - tf_Wc(0).head.length + 1,
          tf_Wc(0).head.head.head.length)))

    val (output_mlp1_flat: Array[Float], runtime_mlp1) =
      Execute(1, 1)(
        LLayerSeq(LReLU), tf_Wd(0), tf_Bd(0), output_conv2_flat)

    val (output_mlp2_flat: Array[Float], runtime_mlp2) =
      Execute(1, 1)(
        LLayerSeq(Linear), tf_Wd(1), tf_Bd(1), output_mlp1_flat)
    println(output_mlp2_flat.mkString(", "))
    println(tf_result.flatten.mkString(", "))
    val output_mlp2 = TestUtils.group(output_mlp2_flat, (tf_result.length, tf_result.head.length))
    for ((tf_result_1d, output_mlp2_1d) <- tf_result zip output_mlp2) {
      assertArrayEquals(tf_result_1d, output_mlp2_1d, precision)
    }
  }

  @Test
  def CNN(): Unit = {
    val (tf_X, tf_Wc, tf_Bc, tf_Wd, tf_Bd, tf_result) = load_datasets("/home/nm/tf_cnn/experiment/", 1)
    val (output_conv1_flat: Array[Float], runtime_conv1) =
      Execute(1, 1)(
        LConvolute(tf_Wc(0).length, tf_Wc(0).head.length, LReLU), tf_Wc(0), tf_Bc(0),
        TestUtils.group(tf_X(0), (28, 28, 1)))


    val (output_conv2_flat: Array[Float], runtime_conv2) =
      Execute(1, 1)(
        LConvolute(tf_Wc(1).length, tf_Wc(1).head.length, LReLU), tf_Wc(1), tf_Bc(1),
        TestUtils.group(output_conv1_flat, (28 - tf_Wc(0).length + 1, 28 - tf_Wc(0).head.length + 1,
          tf_Wc(0).head.head.head.length)))

    val (output_mlp1_flat: Array[Float], runtime_mlp1) =
      Execute(1, 1)(
        LLayerSeq(LReLU), tf_Wd(0), tf_Bd(0), output_conv2_flat)

    val (output_mlp2_flat: Array[Float], runtime_mlp2) =
      Execute(1, 1)(
        LLayerSeq(Linear), tf_Wd(1), tf_Bd(1), output_mlp1_flat)
    println(output_mlp2_flat.mkString(", "))
    println(tf_result.flatten.mkString(", "))
    val output_mlp2 = TestUtils.group(output_mlp2_flat, (tf_result.length, tf_result.head.length))
    for ((tf_result_1d, output_mlp2_1d) <- tf_result zip output_mlp2) {
      assertArrayEquals(tf_result_1d, output_mlp2_1d, precision)
    }
  }
}
