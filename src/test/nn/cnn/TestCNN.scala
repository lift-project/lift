package lift.nn.cnn

/**
  * Created by nm on 09/01/17.
  */

import ir.ArrayType
import ir.ast.{Join, Slide2D, Transpose, UserFun, Zip, fun, λ}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

import scala.util.parsing.json.JSON

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
  val layer_idim = SizeVar("layer_idim")
  val layer_odim = SizeVar("layer_odim")

  val LReLU = UserFun("ReLU", "x", "{ return(max(0.0f, x)); }", Float, Float)
  val Linear = id

  /* Sequential layer */
  def LLayerSeq(activation_f: UserFun) = fun(
    ArrayType(ArrayType(Float, layer_idim), layer_odim),
    ArrayType(Float, layer_odim),
    ArrayType(Float, layer_idim),
    (W_layer, b_layer, X) => {
      Join() o MapSeq(fun((ws_per_neuron, b_per_neuron) => {
        toGlobal(MapSeq(activation_f)) o
          ReduceSeq(add, id(b_per_neuron)) o
          MapSeq(mult) $
          Zip(X, ws_per_neuron)
      })) $ Zip(W_layer, b_layer)
    }
  )

  def LConvolute2D(kernel_w: Int, kernel_h: Int, input_channels: Int,
                   output_channels: Int, activation_f: UserFun) = fun(
    ArrayType(ArrayType(ArrayType(ArrayType(Float, kernel_w), kernel_h), input_channels), output_channels), //TODO: change order
    ArrayType(Float, output_channels),
    ArrayType(ArrayType(ArrayType(Float, input_xdim), input_ydim), input_channels),
    (K, b, X) => {
      MapSeq(fun((K_output_channel, b_channel) => {
        MapSeq(toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_channel)) o Join() o Join() o Transpose()
        ) o Transpose() o
        MapSeq(fun((K_input_channel, X_channel) => {
          MapSeq(fun((pass_y) => {
            MapSeq(fun((pass_x) => {
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join() o
                MapSeq(fun((window_row, kernel_row) => {
                  toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(window_row, kernel_row)
                })) $ Zip(pass_x, K_input_channel)
            })) $ pass_y
          })) o Slide2D(kernel_h, 1, kernel_w, 1) $ X_channel
        })) $ Zip(K_output_channel, X)
      })) $ Zip(K, b)
    }
  )

  val input_channels = SizeVar("input_channels")
  val output_channels = SizeVar("output_channels")

  def LConvolute2D3(kernel_h: Int, kernel_w: Int, activation_f: UserFun) = fun(
    ArrayType(ArrayType(ArrayType(ArrayType(Float, output_channels), input_channels), kernel_w), kernel_h),
    ArrayType(Float, output_channels),
    ArrayType(ArrayType(ArrayType(Float, input_channels), input_xdim), input_ydim),
    (K, b, X) => {
      MapSeq(λ((pass_strip) => {
        MapSeq(λ((pass_window) => { Join() o
          MapSeq(λ((weighted_window_per_out_ch, b_per_out_ch) => { // Reduce weighted pass window separately for each output
            MapSeq(toGlobal(id)) o ReduceSeq(add, id(b_per_out_ch)) $ weighted_window_per_out_ch
          })) o λ((weighted_window_across_out_chs) => Zip(weighted_window_across_out_chs, b)) o Transpose() o
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
      })) o Slide2D(kernel_h, 1, kernel_w, 1) $ X
    }
  )

  def LCNN(c1_kernel_w: Int, c1_kernel_h: Int, c1_n_inputs: Int, c1_n_kernels: Int,
           c2_kernel_w: Int, c2_kernel_h: Int, c2_n_inputs: Int, c2_n_kernels: Int,
           l1_n_inputs: Int, l1_n_outputs: Int, l2_n_inputs: Int, l2_n_outputs: Int) = fun(
    ArrayType(ArrayType(Float, input_xdim), input_ydim),
    ArrayType(ArrayType(ArrayType(ArrayType(Float, c1_n_kernels), c1_n_inputs), c1_kernel_w), c1_kernel_h),
    ArrayType(Float, c1_n_kernels),
    ArrayType(ArrayType(ArrayType(ArrayType(Float, c2_n_kernels), c2_n_inputs), c2_kernel_w), c2_kernel_h),
    ArrayType(Float, c2_n_kernels),
    ArrayType(ArrayType(Float, l1_n_inputs), l1_n_outputs),
    ArrayType(Float, l1_n_outputs),
    ArrayType(ArrayType(Float, l2_n_inputs), l2_n_outputs),
    ArrayType(Float, l2_n_outputs),

    (X, K1, bc1, K2, bc2, W1, b1, W2, b2) => {
      LLayerSeq(LReLU)(W2, b2,
        LLayerSeq(LReLU)(W1, b1,
          LConvolute2D(c1_kernel_w, c1_kernel_h, c2_n_inputs, c2_n_kernels, LReLU)(K2, bc2,
            LConvolute2D(c1_kernel_w, c1_kernel_h, c1_n_inputs, c1_n_kernels, LReLU)(K1, bc1, X))))
    }
  )


  // Test values
  val input_X = Array(
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
          Array(60.0f, 60.0f), Array(61.0f, 61.0f), Array(62.0f, 62.0f), Array(63.0f, 63.0f)))

  val input_b = Array(0.0f, 0.0f, 0.0f)

  val input_K = Array(Array(Array(Array(1.0f, 0.0f, 1.0f), Array(0.0f, 1.0f, 0.0f)),
                            Array(Array(3.0f, 0.0f, 3.0f), Array(0.0f, 3.0f, 0.0f)),
                            Array(Array(5.0f, 0.0f, 5.0f), Array(0.0f, 5.0f, 0.0f))),
                      Array(Array(Array(7.0f, 0.0f, 7.0f), Array(0.0f, 7.0f, 0.0f)),
                            Array(Array(9.0f, 0.0f, 9.0f), Array(0.0f, 9.0f, 0.0f)),
                            Array(Array(11.0f, 0.0f, 11.0f), Array(0.0f, 11.0f, 0.0f))))
  //shape = (2, 3, 2, 3)

  val gold =
    Array(Array(Array(260.0f, 260.0f, 260.0f),    Array(296.0f, 296.0f, 296.0f),    Array(332.0f, 332.0f, 332.0f),
                Array(368.0f, 368.0f, 368.0f),    Array(404.0f, 404.0f, 404.0f),    Array(440.0f, 440.0f, 440.0f)),
          Array(Array(548.0f, 548.0f, 548.0f),    Array(584.0f, 584.0f, 584.0f),    Array(620.0f, 620.0f, 620.0f),
                Array(656.0f, 656.0f, 656.0f),    Array(692.0f, 692.0f, 692.0f),    Array(728.0f, 728.0f, 728.0f)),
          Array(Array(836.0f, 836.0f, 836.0f),    Array(872.0f, 872.0f, 872.0f),    Array(908.0f, 908.0f, 908.0f),
                Array(944.0f, 944.0f, 944.0f),    Array(980.0f, 980.0f, 980.0f),    Array(1016.0f, 1016.0f, 1016.0f)),
          Array(Array(1124.0f, 1124.0f, 1124.0f), Array(1160.0f, 1160.0f, 1160.0f), Array(1196.0f, 1196.0f, 1196.0f),
                Array(1232.0f, 1232.0f, 1232.0f), Array(1268.0f, 1268.0f, 1268.0f), Array(1304.0f, 1304.0f, 1304.0f)),
          Array(Array(1412.0f, 1412.0f, 1412.0f), Array(1448.0f, 1448.0f, 1448.0f), Array(1484.0f, 1484.0f, 1484.0f),
                Array(1520.0f, 1520.0f, 1520.0f), Array(1556.0f, 1556.0f, 1556.0f), Array(1592.0f, 1592.0f, 1592.0f)),
          Array(Array(1700.0f, 1700.0f, 1700.0f), Array(1736.0f, 1736.0f, 1736.0f), Array(1772.0f, 1772.0f, 1772.0f),
                Array(1808.0f, 1808.0f, 1808.0f), Array(1844.0f, 1844.0f, 1844.0f), Array(1880.0f, 1880.0f, 1880.0f)),
          Array(Array(1988.0f, 1988.0f, 1988.0f), Array(2024.0f, 2024.0f, 2024.0f), Array(2060.0f, 2060.0f, 2060.0f),
                Array(2096.0f, 2096.0f, 2096.0f), Array(2132.0f, 2132.0f, 2132.0f), Array(2168.0f, 2168.0f, 2168.0f)))

  val precision: Float = 0.0001f

  def load_4d_float_json(json_file_path: String): Array[Array[Array[Array[Float]]]] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[List[List[List[Double]]]] = json.get.asInstanceOf[List[List[List[List[Double]]]]]

    // Convert from List[List[Double]] to Array[Array[Float]]
    val w_arr = Array.fill[Array[Array[Array[Float]]]](w_list.length)(
      Array.fill[Array[Array[Float]]](w_list.head.length)(
        Array.fill[Array[Float]](w_list.head.head.length)(
          Array.fill[Float](w_list.head.head.head.length)(0))))

    var aline = Array.fill[Double](w_list.head.length)(0)
    for (i <- w_list.indices) {
      for (j <- w_list(i).indices) {
        for (k <- w_list(i)(j).indices) {
          aline = w_list(i)(j)(k).to[Array]
          for (l <- aline.indices) {
            w_arr(i)(j)(k)(l) = aline(l).toFloat
          }
        }
      }
    }
    w_arr
  }

  def load_3d_float_json(json_file_path: String): Array[Array[Array[Float]]] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[List[List[Double]]] = json.get.asInstanceOf[List[List[List[Double]]]]

    // Convert from List[List[Double]] to Array[Array[Float]]
    val w_arr = Array.fill[Array[Array[Float]]](w_list.length)(
      Array.fill[Array[Float]](w_list.head.length)(
        Array.fill[Float](w_list.head.head.length)(0)))
    var aline = Array.fill[Double](w_list.head.length)(0)
    for (i <- w_list.indices) {
      for (j <- w_list(i).indices) {
        aline = w_list(i)(j).to[Array]
        for (k <- aline.indices) {
          w_arr(i)(j)(k) = aline(k).toFloat
        }
      }
    }
    w_arr
  }

  def load_2d_float_json(json_file_path: String): Array[Array[Float]] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[List[Double]] = json.get.asInstanceOf[List[List[Double]]]

    // Convert from List[List[Double]] to Array[Array[Float]]
    val w_arr = Array.fill[Array[Float]](w_list.length)(Array.fill[Float](w_list.head.length)(0))
    var aline = Array.fill[Double](w_list.head.length)(0)
    for (i <- w_list.indices) {
      aline = w_list(i).to[Array]
      for (j <- aline.indices) {
        w_arr(i)(j) = aline(j).toFloat
      }
    }
    w_arr
  }

  def load_1d_float_json(json_file_path: String): Array[Float] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(json_file_path)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    val w_list: List[Double] = json.get.asInstanceOf[List[Double]]

    // Convert from List[List[Double]] to Array[Array[Float]]
    val w_arr = Array.fill[Float](w_list.length)(0)
    var aline = Array.fill[Double](w_list.length)(0)
    aline = w_list.to[Array]
    for (i <- aline.indices) {
      w_arr(i) = aline(i).toFloat
    }
    w_arr
  }

  def load_experiment(experiment_dir: String, n_inputs: Int) = {
    var tf_Wc = Array(load_4d_float_json(experiment_dir + "wc1.json"))
    var tf_Bc = Array(load_1d_float_json(experiment_dir + "bc1.json"))
    tf_Wc = tf_Wc :+ load_4d_float_json(experiment_dir + "wc2.json")
    tf_Bc = tf_Bc :+ load_1d_float_json(experiment_dir + "bc2.json")
    var tf_Wd = Array(load_2d_float_json(experiment_dir + "wd1.json"))
    tf_Wd = tf_Wd :+ load_2d_float_json(experiment_dir + "wout.json")
    var tf_Bd = Array(load_1d_float_json(experiment_dir + "bd1.json"))
    tf_Bd = tf_Bd :+ load_1d_float_json(experiment_dir + "bout.json")

    val tf_X = load_2d_float_json(experiment_dir + "test_images_n" + n_inputs + ".json")
    val tf_result = load_2d_float_json(experiment_dir + "test_tf_results_n" + n_inputs + ".json")

    (tf_X, tf_Wc, tf_Bc, tf_Wd, tf_Bd, tf_result)
  }

  def array_1d_to_3d(arr1d: Array[Float], shape: (Int, Int, Int)) = {
    val arr3d = Array.fill[Array[Array[Float]]](shape._1)(
      Array.fill[Array[Float]](shape._2)(
        Array.fill[Float](shape._3)(0)))
    for (i <- 0 until shape._1; j <- 0 until shape._2; k <- 0 until shape._3) {
      arr3d(i)(j)(k) = arr1d(i * shape._2 * shape._3 + j * shape._3 + k)
    }
    arr3d
  }

  @Test
  def Sanity_CNN(): Unit = {
    val (lift_result: Array[Float], runtime) = Execute(1,1)(
      LConvolute2D3(2, 3, Linear), input_K, input_b, input_X)

    println(f"\n1. Convolution sanity check.\n" +
            f"Runtime: $runtime%1.5f ms")

    val lift_result3d = array_1d_to_3d(lift_result, (gold.length, gold.head.length, gold.head.head.length))
    for ((gold2d, lift_result2d) <- gold zip lift_result3d) {
      println(lift_result2d.flatten.mkString(", "))
      println(gold2d.flatten.mkString(", "))
      for ((gold1d, lift_result1d) <- gold2d zip lift_result2d) {
        assertArrayEquals(gold1d, lift_result1d, precision)
      }
    }
  }
/*
  @Test
  def MNIST_CNN(): Unit = {
    val (tf_X, tf_Wc, tf_Bc, tf_Wd, tf_Bd, tf_result, dir_name) =
      load_experiment("/home/nm/tf_cnn/experiment/", 1)

  }*/
}
