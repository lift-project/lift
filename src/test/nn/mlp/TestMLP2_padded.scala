package nn.mlp

/**
  * Created by Naums Mogers in July 2016.
  * This file implements the Lift version of the Multilayer Perceptron forward-propagation.
  */

import apart.arithmetic.{NotEvaluableException, SizeVar}
import ir.ArrayType
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import opencl.executor.{DeviceCapabilityException, Execute, Executor}
import org.junit.{AfterClass, BeforeClass, Test}

import scala.collection.mutable.{Map => DictMap}
import scala.util.parsing.json._
import java.io._
import java.util.Calendar

import opencl.executor.Executor.ExecutorFailureException

import scala.collection.mutable

object TestMLP2_padded {
  @BeforeClass def before(): Unit = {

    Executor.loadLibrary()
    println("Initialize the executor")
    val intellij_path = System.getProperty("user.dir") + "/../../src/test/nn/mlp"
    if (java.nio.file.Files.exists(java.nio.file.Paths.get(intellij_path)))
    // Use GPU on the development machine
      Executor.init(1, 1)
    else
    // Use GPU on the testing machine
      Executor.init(1, 1)
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestMLP2_padded {

  val precision: Float = 0.01f
  var runnerIsConsole: Boolean = false

  @Test
  def testSuite(): Unit = {
    val reruns = 1
    val append_results: Boolean = true
    val experiments = Array(
      /* Parallel neuron, a lot of inputs */
      DictMap("mults_per_thread" -> Array.range(start=1, end=16+1, step=1),
        "neurons_per_wrg" -> Array.range(start=1, end=16+1, step=1),
        "hidden_layer_0_range" -> Array(224, 228, 230, 231, 234),
        "n_inputs_range" -> Array(416)))

    for (i <- 0 until reruns) {
      for (e <- experiments) {
        for (hidden_layer_0_size <- e("hidden_layer_0_range")) {
          val experiment_dir_path = current_dir + f"/experiment.784-$hidden_layer_0_size%d-32-10"
          val lift_results_dir_path = experiment_dir_path + "/results_lift"
          if (java.nio.file.Files.exists(java.nio.file.Paths.get(experiment_dir_path))) {
            val runAll = if (java.nio.file.Files.exists(java.nio.file.Paths.get(lift_results_dir_path)))
              false else true

            val lift_results_dir = new File(lift_results_dir_path)
            for (n_inputs <- e("n_inputs_range")) {
              for (mults_per_thread <- e("mults_per_thread")) {
                for (neurons_per_wrg <- e("neurons_per_wrg")) {
                  // Ensures that there is only one set of results per experiment if append_results == false
                  if (append_results || runAll || lift_results_dir.listFiles.toList.count {
                    file => file.getName.endsWith("_n%d.csv".format(n_inputs))
                  } == 0) {
                    println(f"Starting the experiment (mults_per_thread=${mults_per_thread.asInstanceOf[Int]}%d, " +
                      f"neurons_per_wrg=${neurons_per_wrg.asInstanceOf[Int]}%d, " +
                      f"hidden_layer_0_size=$hidden_layer_0_size%d, " +
                      f"n_inputs=$n_inputs%d)")
                    try {
                      MNIST_MLP_in_2d_MrgdGrps_in_2d(Array(hidden_layer_0_size, 32), n_inputs,
                        mults_per_thread.asInstanceOf[Int],neurons_per_wrg.asInstanceOf[Int])
                    } catch {
                      case e: DeviceCapabilityException =>
                        println("ERROR: Not enough OpenCL memory. Skipping the experiment.")
                      case e: NotEvaluableException =>
                        println("ERROR: Not enough OpenCL memory. Skipping the experiment.")
                      case e: ExecutorFailureException  =>
                        println("ERROR: OpenCL error: CL_OUT_OF_RESOURCES.")
                      case e: AssertionError =>
                        println(e.getMessage)
                    }
                    try {
                      MNIST_MLP_in_2d_MrgdGrps_in_2d_coalesced(Array(hidden_layer_0_size, 32), n_inputs,
                        mults_per_thread.asInstanceOf[Int],neurons_per_wrg.asInstanceOf[Int])
                    } catch {
                      case e: DeviceCapabilityException =>
                        println("ERROR: Not enough OpenCL memory. Skipping the experiment.")
                      case e: NotEvaluableException =>
                        println("ERROR: Not enough OpenCL memory. Skipping the experiment.")
                      case e: ExecutorFailureException  =>
                        println("ERROR: OpenCL error: CL_OUT_OF_RESOURCES.")
                      case e: AssertionError =>
                        println(e.getMessage)
                    }
                  }
                  else {
                    println(f"Skipping the experiment (mults_per_thread=${mults_per_thread.asInstanceOf[Int]}%d, " +
                      f"neurons_per_wrg=${neurons_per_wrg.asInstanceOf[Int]}%d, " +
                      f"hidden_layer_0_size=$hidden_layer_0_size%d, " +
                      f"n_inputs=$n_inputs%d)")
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  val current_dir = {
    // Launching from IntelliJ or from console?
    val intellij_path = System.getProperty("user.dir") + "/../../src/test/nn/mlp"
    val console_path = System.getProperty("user.dir") + "/src/test/nn/mlp"
    if (java.nio.file.Files.exists(java.nio.file.Paths.get(intellij_path)))
      intellij_path
    else {
      runnerIsConsole = true
      console_path
    }
  }

  def results_filename(exp_dir_name: String, n_inputs: Int) = {
    val now = Calendar.getInstance()
    new String(current_dir + "/" + exp_dir_name + "/results_lift/" +
      "%02d.%02d.%04d-%02d.%02d.%02d.%03d_n%d.csv".format(
        now.get(Calendar.DATE), now.get(Calendar.MONTH), now.get(Calendar.YEAR),
        now.get(Calendar.HOUR_OF_DAY), now.get(Calendar.MINUTE), now.get(Calendar.SECOND),
        now.get(Calendar.MILLISECOND), n_inputs))
  }

  def load_2d_float_json(json_file_name: String): Array[Array[Float]] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(current_dir + "/" + json_file_name)
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

  def load_1d_float_json(json_file_name: String): Array[Float] = {
    /* Load an array from a JSON file */
    val source = scala.io.Source.fromFile(current_dir + "/" + json_file_name)
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

  def load_experiment(hidden_layers: Array[Int], n_inputs: Int) = {
    var dir_name = "experiment.784"
    for (hidden_layer_n <- hidden_layers)
      dir_name = dir_name + "-" + hidden_layer_n.toString
    dir_name = dir_name + "-10"

    // Temporary hack:
    var n_replacement = n_inputs
    if (!java.nio.file.Files.exists(java.nio.file.Paths.get(current_dir + "/" + dir_name +
      "/W1_n" + n_inputs + ".json"))) {
      n_replacement = 32
      println("Using Wx_n32...")
    }

    var tf_W = Array(load_2d_float_json(dir_name + "/W1_n" + n_replacement + ".json"))
    var tf_B = Array(load_1d_float_json(dir_name + "/b1_n" + n_replacement + ".json"))
    for (i <- Range(2, hidden_layers.length + 1)) {
      tf_W = tf_W :+ load_2d_float_json(dir_name + "/W" + i.toString + "_n" + n_replacement + ".json")
      tf_B = tf_B :+ load_1d_float_json(dir_name + "/b" + i.toString + "_n" + n_replacement + ".json")
    }
    tf_W = tf_W :+ load_2d_float_json(dir_name + "/Wout_n" + n_replacement + ".json")
    tf_B = tf_B :+ load_1d_float_json(dir_name + "/bout_n" + n_replacement + ".json")
    val tf_X = load_2d_float_json(dir_name + "/test_images_n" + n_inputs + ".json")
    val tf_result = load_2d_float_json(dir_name + "/test_tf_results_n" + n_inputs + ".json")

    (tf_X, tf_W, tf_B, tf_result, dir_name)
  }

  val floatSize = 4
  val localMemSize = Executor.getDeviceLocalMemSize.toInt
  val maxWorkGroupSize = Executor.getDeviceMaxWorkGroupSize.toInt
  val deviceName = Executor.getDeviceName

  val layer_idim = SizeVar("layer_idim")
  val layer_odim = SizeVar("layer_odim")
  val layer_ninputs = SizeVar("layer_ninputs")
  val idim = SizeVar("idim")
  val hdim1 = SizeVar("hdim1")
  val hdim2 = SizeVar("hdim2")
  val odim = SizeVar("odim")

  val ReLU = UserFun("ReLU", "x", "{ return(max(0.0f, x)); }", Float, Float)
  val Linear = id

  /* Sequential layer */
  def f_layer_seq(activation_f: UserFun) = fun(
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

  /* Sequential MLP */
  def mlp_fprop_seq(activation_f: UserFun) = fun(
    ArrayType(ArrayType(Float, idim), hdim1), ArrayType(Float, hdim1),
    ArrayType(ArrayType(Float, hdim1), hdim2), ArrayType(Float, hdim2),
    ArrayType(ArrayType(Float, hdim2), odim), ArrayType(Float, odim),
    ArrayType(Float, idim),
    (W1, b1, W2, b2, Wout, bout, X) => {
      f_layer_seq(activation_f)(Wout, bout,
        f_layer_seq(activation_f)(W2, b2,
          f_layer_seq(activation_f)(W1, b1, X)))}
  )

  /* Parallel layer */
  def f_layer_par(activation_f: UserFun,
                  tile_of_neurons_size: Int,
                  tile_of_mults_size: Int) = fun(
    ArrayType(ArrayType(Float, layer_idim), layer_odim),
    ArrayType(Float, layer_odim),
    ArrayType(Float, layer_idim),
    (W, B, X) => {
      Join() o MapWrg(MapSeq(fun((ws_per_neuron, b_per_neuron) => {
        toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_per_neuron)) o Join() o MapLcl(
          toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
          Split(tile_of_mults_size) $ Zip(X, ws_per_neuron)
      }))) o Split(tile_of_neurons_size) $ Zip(W, B)
    }
  )

  /* Parallel layer (across inputs as well), 1-dimensional */
  def f_layer_par_across_inp_1d(activation_f: UserFun,
                                tile_of_neurons_size: Int,
                                tile_of_mults_size: Int,
                                n_inputs: Int) = fun(
    ArrayType(ArrayType(Float, layer_idim), layer_odim),
    ArrayType(Float, layer_odim),
    ArrayType(ArrayType(Float, layer_idim), layer_ninputs),
    (W, B, X) => {
      Split(n_inputs) o Join() o
        MapWrg(MapSeq(fun((neuron_params/*:(ws_per_neuron, b_per_neuron, X_per_input)*/) => {
          toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(Get(neuron_params, 1)/*b_per_neuron*/)) o Join() o MapLcl(
            toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
            Split(tile_of_mults_size) $ Zip(Get(neuron_params, 2)/*X_per_input*/,
            Get(neuron_params, 0)/*ws_per_neuron*/)
        })))o Split(tile_of_neurons_size) o Join() o MapSeq(fun((X_per_input) => {
        MapSeq(fun((ws_per_neuron, b_per_neuron) =>
          Tuple(ws_per_neuron, b_per_neuron, X_per_input))) $
          Zip(W, B)})) $ X
    }
  )

  /* Parallel layer (across inputs as well), 2-dimensional, multiple threads per neuron */
  /* For the case of a bit of neurons and a lot of inputs */
  def f_layer_complex_neuron(activation_f: UserFun, tile_of_mults_size: Int) = fun(
    ArrayType(ArrayType(Float, layer_idim), layer_odim),
    ArrayType(Float, layer_odim),
    ArrayType(ArrayType(Float, layer_idim), layer_ninputs),
    (W, B, X) => {
      MapWrg(1)(fun((tup_per_inp) => {
        MapWrg(0)(fun((ws_per_neuron, b_per_neuron) => {
          toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_per_neuron)) o Join() o
            MapLcl(0)(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
            Split(tile_of_mults_size) $ Zip(Get(tup_per_inp, 2)/*X*/, ws_per_neuron)})) $
          Zip(Get(tup_per_inp, 0)/*W*/, Get(tup_per_inp, 1)/*B*/)})) o
        MapSeq(fun((X_per_input) => {Tuple(W, B, X_per_input)})) $ X
    }
  )

  /* Parallel layer (across inputs as well), 2-dimensional, multiple threads per neuron */
  /* Inputs are stored in local memory for faster access. */
  /* For the case of a bit of neurons and a lot of inputs */
  def f_layer_complex_neuron_local(activation_f: UserFun, tile_of_mults_size: Int) = fun(
    ArrayType(ArrayType(Float, layer_idim), layer_odim),
    ArrayType(Float, layer_odim),
    ArrayType(ArrayType(Float, layer_idim), layer_ninputs),
    (W, B, X) => {
      MapWrg(1)(fun((tup_per_inp) => {
        MapWrg(0)(fun((ws_per_neuron, b_per_neuron) => {
          toGlobal(MapSeq(activation_f)) o ReduceSeq(add, toPrivate(id) $ b_per_neuron) o Join() o
            MapLcl(0)(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
            Split(tile_of_mults_size) o fun((X_local) => Zip(X_local, ws_per_neuron)) o
            toLocal(MapLcl(1)(id)) $ Get(tup_per_inp, 2)/*X*/})) $
          Zip(Get(tup_per_inp, 0)/*W*/, Get(tup_per_inp, 1)/*B*/)})) o
        MapSeq(fun((X_per_input) => {Tuple(W, B, X_per_input)})) $ X
    }
  )

  /* Parallel layer (across inputs as well), 2-dimensional, multiple threads per neuron *
  * Each workgroup processes a tile of inputs.
  * For the case of a bit of neurons and a lot of inputs */
  def f_layer_complex_neuron_mrgd_wrgs_in_1d(activation_f: UserFun,
                                             tile_of_mults_size: Int,
                                             tile_of_inputs_size: Int,
                                             odim: Int) = fun(
    ArrayType(ArrayType(Float, layer_idim), layer_odim),
    ArrayType(Float, layer_odim),
    ArrayType(ArrayType(Float, layer_idim), layer_ninputs),
    (W, B, X) => {
      Join() o
        MapWrg(1)(fun((X_tile) => {
          Scatter(ReorderWithStride(odim)) o Join() o
            MapWrg(0)(fun((ws_per_neuron, b_per_neuron) => {
              MapLcl(1)(fun((X_single) => {
                TransposeW() o MapLcl(0)(toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_per_neuron)) ) o Transpose() o
                  MapLcl(0)(
                    toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
                  Split(tile_of_mults_size) $ Zip(X_single, ws_per_neuron)})) $
                X_tile})) $
            Zip(W, B)})) o Split(tile_of_inputs_size) $ X
    }
  )

  /* Parallel layer (across inputs as well), 2-dimensional, multiple threads per neuron
  * TODO */
  def f_layer_complex_neuron_mrgd_wrgs_in_2d(activation_f: UserFun,
                                             tile_of_mults_size: Int,
                                             tile_of_inputs_size: Int,
                                             tile_of_neurons_size: Int,
                                             odim: Int, idim: Int, ninputs: Int) = fun(
    ArrayType(ArrayType(Float, idim), odim),
    ArrayType(Float, odim),
    ArrayType(ArrayType(Float, idim), ninputs),
    (W, B, X) => {
      Join() o
        MapWrg(1)(fun((X_tile) => {
          Scatter(ReorderWithStride(odim / tile_of_neurons_size)) o Join() o
            MapWrg(0)(fun((ws_per_tile, bs_per_tile) => {
              MapLcl(1)(fun((X_single) => {
                MapLcl(0)(fun((b_per_neuron, partial_sums_per_neuron) => {
                  TransposeW() o
                    MapSeq(toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_per_neuron)) ) o
                    Transpose() $ partial_sums_per_neuron})) o
                  fun((partial_sums) => Zip(bs_per_tile, partial_sums)) o
                  Split(idim / tile_of_mults_size) o
                  MapLcl(0)(
                    toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
                  Split(tile_of_mults_size) o Join() o
                  MapSeq(fun((ws_per_neuron) => {Zip(X_single, ws_per_neuron)})) $ ws_per_tile})) $
                X_tile})) $ Zip(Split(tile_of_neurons_size) $ W, Split(tile_of_neurons_size) $ B)})) o
        Split(tile_of_inputs_size) $ X
    }
  )

  /* Parallel layer (across inputs as well), 2-dimensional, multiple threads per neuron
  * TODO */
  def f_layer_complex_neuron_mrgd_wrgs_in_2d_coalesced(activation_f: UserFun,
                                                       tile_of_mults_size: Int,
                                                       tile_of_inputs_size: Int,
                                                       tile_of_neurons_size: Int,
                                                       odim: Int, idim: Int, ninputs: Int) = fun(
    ArrayType(ArrayType(Float, idim), odim),
    ArrayType(Float, odim),
    ArrayType(ArrayType(Float, idim), ninputs),
    (W, B, X) => {
      Join() o
        MapWrg(1)(fun((X_tile) => {
          Scatter(ReorderWithStride(odim / tile_of_neurons_size)) o Join() o
            MapWrg(0)(fun((ws_per_tile, bs_per_tile) => {
              MapLcl(1)(fun((X_single) => {
                MapLcl(0)(fun((b_per_neuron, partial_sums_per_neuron) => {
                  TransposeW() o
                    MapSeq(toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_per_neuron)) ) o
                    Transpose() $ partial_sums_per_neuron})) o
                  fun((partial_sums) => Zip(bs_per_tile, partial_sums)) o
                  Split(idim / tile_of_mults_size) o
                  MapLcl(0)(
                    toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
                  Split(tile_of_mults_size) o Join() o
                  MapSeq(fun((ws_per_neuron) => {ReorderStride(tile_of_mults_size) $
                    Zip(X_single, ws_per_neuron)})) $ ws_per_tile})) $
                X_tile})) $ Zip(Split(tile_of_neurons_size) $ W, Split(tile_of_neurons_size) $ B)})) o
        Split(tile_of_inputs_size) $ X
    }
  )

  /* Parallel layer (across inputs as well), 2-dimensional, single thread per neuron */
  /* For the case of a lot of neurons and a bit of inputs */
  def f_layer_simplex_neuron(activation_f: UserFun, tile_of_mults_size: Int) = fun(
    ArrayType(ArrayType(Float, layer_idim), layer_odim),
    ArrayType(Float, layer_odim),
    ArrayType(ArrayType(Float, layer_idim), layer_ninputs),
    (W, B, X) => {
      MapWrg(1)(fun((tup_per_inp) => {
        MapWrg(0)(fun((ws_per_neuron, b_per_neuron) => {
          toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_per_neuron)) o Join() o
            MapLcl(0)(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
            Split(tile_of_mults_size) $ Zip(Get(tup_per_inp, 2)/*X*/, ws_per_neuron)})) $
          Zip(Get(tup_per_inp, 0)/*W*/, Get(tup_per_inp, 1)/*B*/)})) o
        MapSeq(fun((X_per_input) => {Tuple(W, B, X_per_input)})) $ X
    }
  )


  // Test values
  val input_W1 = Array(Array(0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f),
    Array(0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.0f),
    Array(0.2f, 0.3f, 0.4f, 0.5f, 0.0f, 0.1f),
    Array(0.3f, 0.4f, 0.5f, 0.0f, 0.1f, 0.2f))

  val input_b1 = Array(0.1f, 0.1f, 0.1f, 0.1f)
  val input_W2 = Array(Array(0.0f, 0.1f, 0.2f, 0.3f),
    Array(0.1f, 0.2f, 0.3f, 0.0f),
    Array(0.2f, 0.3f, 0.0f, 0.1f),
    Array(0.3f, 0.0f, 0.1f, 0.2f),
    Array(0.0f, 0.1f, 0.2f, 0.3f),
    Array(0.1f, 0.2f, 0.3f, 0.0f),
    Array(0.2f, 0.3f, 0.0f, 0.1f),
    Array(0.3f, 0.0f, 0.1f, 0.2f))
  val input_b2 = Array(0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f)
  val input_Wout = Array(Array(0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f),
    Array(0.1f, 0.2f, 0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f),
    Array(0.2f, 0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f),
    Array(0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.0f, 0.1f, 0.2f),
    Array(0.4f, 0.5f, 0.6f, 0.7f, 0.0f, 0.1f, 0.2f, 0.3f),
    Array(0.5f, 0.6f, 0.7f, 0.0f, 0.1f, 0.2f, 0.3f, 0.4f))
  val input_bout = Array(0.1f, 0.1f, 0.1f, 0.1f, 0.1f, 0.1f)
  val input_X = Array(6f, 7f, 8f, 9f, 5f, 4f)
  val input_X2 = Array(3f, 4f, 5f, 0f, 1f, 2f)
  val gold = Array(17.492f, 11.356f, 14.406f, 17.636f, 17.492f, 17.732f)
  val gold2 = Array(6.884f, 4.486f, 5.784f, 7.244f, 6.884f, 6.86f)

  //@Test
  def Sanity_MLPSequential(): Unit = {
    val (lift_result: Array[Float], runtime) = Execute(1,1)(
      mlp_fprop_seq(Linear), input_W1, input_b1, input_W2, input_b2, input_Wout, input_bout, input_X)
    println(f"\n1. Everything sequential, single kernel.\n" +
      f"Runtime: $runtime%1.5f ms")
    //println(lift_result.mkString(", "))
    //println(gold.mkString(", "))

    assertArrayEquals(gold, lift_result, precision)
  }

  //@Test
  def Sanity_MLPThreeSeqKernels(): Unit = {
    val (output_layer1: Array[Float], runtime_layer1) = Execute(1,1)(
      f_layer_seq(Linear), input_W1, input_b1, input_X)
    val (output_layer2: Array[Float], runtime_layer2) = Execute(1,1)(
      f_layer_seq(Linear), input_W2, input_b2, output_layer1)
    val (lift_result: Array[Float], runtime_layerout) = Execute(1,1)(
      f_layer_seq(Linear), input_Wout, input_bout, output_layer2)
    println(f"\n2. x3 sequential kernels.\n" +
      f"Runtime: $runtime_layer1%1.5f + $runtime_layer2%1.5f + $runtime_layerout%1.5f = " +
      f"${runtime_layer1 + runtime_layer2 + runtime_layerout}%1.5f ms")

    assertArrayEquals(gold, lift_result, precision)
  }

  //@Test
  def Sanity_MLP(): Unit = {
    val (output_layer1: Array[Float], runtime_layer1) =
      Execute(2, 12)(
        f_layer_par(Linear, tile_of_neurons_size=1, tile_of_mults_size=2), input_W1, input_b1, input_X)
    val (output_layer2: Array[Float], runtime_layer2) =
      Execute(2, 8)(
        f_layer_par(Linear, tile_of_neurons_size=2, tile_of_mults_size=2), input_W2, input_b2, output_layer1)
    val (lift_result: Array[Float], runtime_layerout) =
      Execute(2, 12)(
        f_layer_par(Linear, tile_of_neurons_size=2, tile_of_mults_size=2), input_Wout, input_bout, output_layer2)

    println(f"\n3. x3 parallel kernels.\n" +
      f"Runtime: $runtime_layer1%1.5f + $runtime_layer2%1.5f + $runtime_layerout%1.5f = " +
      f"${runtime_layer1 + runtime_layer2 + runtime_layerout}%1.5f ms")
    println(output_layer1.mkString(", "))
    println(output_layer2.mkString(", "))
    println(lift_result.mkString(", "))

    assertArrayEquals(gold, lift_result, precision)
  }

  //@Test
  def Sanity_MLP_in_1d(): Unit = {
    val (output_layer1_flat: Array[Float], runtime_layer1) =
      Execute(2, 12)(f_layer_par_across_inp_1d(Linear, tile_of_neurons_size=1, tile_of_mults_size=2, n_inputs = 2),
        input_W1, input_b1, Array(input_X, input_X2))
    val output_layer1 = output_layer1_flat.grouped(input_W1.length).toArray

    val (output_layer2_flat: Array[Float], runtime_layer2) =
      Execute(2, 8)(f_layer_par_across_inp_1d(Linear, tile_of_neurons_size=2, tile_of_mults_size=2, n_inputs = 2),
        input_W2, input_b2, output_layer1)
    val output_layer2 = output_layer2_flat.grouped(input_W2.length).toArray

    val (lift_result_flat: Array[Float], runtime_layerout) =
      Execute(2, 12)(f_layer_par_across_inp_1d(Linear, tile_of_neurons_size=2, tile_of_mults_size=2, n_inputs = 2),
        input_Wout, input_bout, output_layer2)
    val lift_result = lift_result_flat.grouped(input_Wout.length).toArray

    println(f"\n4. x3 1D-parallel kernels (across inputs).\n" +
      f"Runtime: $runtime_layer1%1.5f + $runtime_layer2%1.5f + $runtime_layerout%1.5f = " +
      f"${runtime_layer1 + runtime_layer2 + runtime_layerout}%1.5f ms")
    println("Input 0: ")
    println(output_layer1(0).mkString(", "))
    println(output_layer2(0).mkString(", "))
    println(lift_result(0).mkString(", "))
    println("Input 1: ")
    println(output_layer1(1).mkString(", "))
    println(output_layer2(1).mkString(", "))
    println(lift_result(1).mkString(", "))

    assertArrayEquals(gold, lift_result(0), precision)
    assertArrayEquals(gold2, lift_result(1), precision)
  }

  //@Test
  def Sanity_MLP_in_2d(): Unit = {
    val N_inputs = 2
    var input_len = input_W1(0).length
    val N_neurons_h1 = input_W1.length
    var mults_per_thread = 2
    val (output_layer1_flat: Array[Float], runtime_layer1) =
      Execute(input_len / mults_per_thread, 1, N_neurons_h1 * input_len / mults_per_thread,
        N_inputs, (false, false))(
        f_layer_complex_neuron(Linear, tile_of_mults_size=mults_per_thread),
        input_W1, input_b1, Array(input_X, input_X2))
    val output_layer1 = output_layer1_flat.grouped(N_neurons_h1).toArray

    input_len = N_neurons_h1
    val N_neurons_h2 = input_W2.length
    mults_per_thread = 2
    val (output_layer2_flat: Array[Float], runtime_layer2) =
      Execute(input_len / mults_per_thread, 1, N_neurons_h2 * input_len / mults_per_thread,
        N_inputs, (false, false))(
        f_layer_complex_neuron(Linear, tile_of_mults_size=mults_per_thread),
        input_W2, input_b2, output_layer1)
    val output_layer2 = output_layer2_flat.grouped(input_W2.length).toArray

    input_len = N_neurons_h2
    val N_neurons_hout = input_Wout.length
    mults_per_thread = 2
    val (lift_result_flat: Array[Float], runtime_layerout) =
      Execute(input_len / mults_per_thread, 1, N_neurons_hout * input_len / mults_per_thread,
        N_inputs, (false, false))(
        f_layer_complex_neuron(Linear, tile_of_mults_size=mults_per_thread),
        input_Wout, input_bout, output_layer2)
    val lift_result = lift_result_flat.grouped(input_Wout.length).toArray

    println(f"\n5. x3 2D-parallel kernels (across inputs).\n" +
      f"Runtime: $runtime_layer1%1.5f + $runtime_layer2%1.5f + $runtime_layerout%1.5f = " +
      f"${runtime_layer1 + runtime_layer2 + runtime_layerout}%1.5f ms")
    println("Input 0: ")
    println(output_layer1(0).mkString(", "))
    println(output_layer2(0).mkString(", "))
    println(lift_result(0).mkString(", "))
    println("Input 1: ")
    println(output_layer1(1).mkString(", "))
    println(output_layer2(1).mkString(", "))
    println(lift_result(1).mkString(", "))

    assertArrayEquals(gold, lift_result(0), precision)
    assertArrayEquals(gold2, lift_result(1), precision)
  }

  //@Test
  def Sanity_MLP_in_2d_Local(): Unit = {
    val N_inputs = 2
    var input_len = input_W1(0).length
    val N_neurons_h1 = input_W1.length
    var mults_per_thread = 2
    val (output_layer1_flat: Array[Float], runtime_layer1) =
      Execute(input_len / mults_per_thread, 1, N_neurons_h1 * input_len / mults_per_thread,
        N_inputs, (false, false))(
        f_layer_complex_neuron_local(Linear, tile_of_mults_size=mults_per_thread),
        input_W1, input_b1, Array(input_X, input_X2))
    val output_layer1 = output_layer1_flat.grouped(N_neurons_h1).toArray

    input_len = N_neurons_h1
    val N_neurons_h2 = input_W2.length
    mults_per_thread = 2
    val (output_layer2_flat: Array[Float], runtime_layer2) =
      Execute(input_len / mults_per_thread, 1, N_neurons_h2 * input_len / mults_per_thread,
        N_inputs, (false, false))(
        f_layer_complex_neuron_local(Linear, tile_of_mults_size=mults_per_thread),
        input_W2, input_b2, output_layer1)
    val output_layer2 = output_layer2_flat.grouped(input_W2.length).toArray

    input_len = N_neurons_h2
    val N_neurons_hout = input_Wout.length
    mults_per_thread = 2
    val (lift_result_flat: Array[Float], runtime_layerout) =
      Execute(input_len / mults_per_thread, 1, N_neurons_hout * input_len / mults_per_thread,
        N_inputs, (false, false))(
        f_layer_complex_neuron_local(Linear, tile_of_mults_size=mults_per_thread),
        input_Wout, input_bout, output_layer2)
    val lift_result = lift_result_flat.grouped(input_Wout.length).toArray

    println(f"\n6. x3 2D-parallel kernels (across inputs). Inputs are stored in local memory\n" +
      f"Runtime: $runtime_layer1%1.5f + $runtime_layer2%1.5f + $runtime_layerout%1.5f = " +
      f"${runtime_layer1 + runtime_layer2 + runtime_layerout}%1.5f ms")
    println("Input 0: ")
    println(output_layer1(0).mkString(", "))
    println(output_layer2(0).mkString(", "))
    println(lift_result(0).mkString(", "))
    println("Input 1: ")
    println(output_layer1(1).mkString(", "))
    println(output_layer2(1).mkString(", "))
    println(lift_result(1).mkString(", "))

    assertArrayEquals(gold, lift_result(0), precision)
    assertArrayEquals(gold2, lift_result(1), precision)
  }




  abstract class MLP_test_generic() {
    var _Activation_fs: Array[UserFun] = Array[UserFun]()
    val _mults_per_thread: Int = 0
    val _neurons_per_wrg: Int = 0
    var _n_inputs: Int = 0
    var _n_neurons: Int = 0
    var _input_len: Int = 0
    var _local_size_0: Int = 0
    var _local_size_1: Int = 0
    var _global_size_0: Int = 0
    var _global_size_1: Int = 0

    def call_layer_f(layer_i: Int): Lambda
    def get_local_size_0(): Int =
      (_input_len.toFloat / _mults_per_thread).toInt
    def get_local_size_1(): Int = 1
    def get_global_size_0(): Int =
      _n_neurons * _local_size_0
    def get_global_size_1(): Int = _n_inputs

    def apply(f_name: String, description: String, exp_dir_name: String,
              Inputs: Array[Array[Float]],
              Weights: Array[Array[Array[Float]]], Biases: Array[Array[Float]],
              Targets: Array[Array[Float]],
              Activation_fs: Array[UserFun]): Unit = {
      _Activation_fs = Activation_fs
      _n_inputs = Inputs.length


      val n_layers = Biases.length
      val runtimes = Array.fill[Double](n_layers)(0)
      var Input = Inputs
      // Size of the last two dimensions will be different for each layer, and determined in the for loop
      val outputs = Array.ofDim[Float](n_layers, _n_inputs, 1)
      var input_no = 0

      print(f"\n" + description + "\nRuntime:\n")
      _input_len = Weights(0)(0).length
      _n_neurons = Biases(0).length


      /* Padding */
      val _n_neurons_new: Int = _neurons_per_wrg * Math.ceil(_n_neurons.toFloat / _neurons_per_wrg).toInt
      val new_Weights0 : Array[Array[Float]] = Weights(0) ++ Array.fill[Array[Float]](_n_neurons_new - _n_neurons)(
        Array.fill[Float](_input_len)(0))
      val new_Biases0 = Biases(0) ++ Array.fill[Float](_n_neurons_new - _n_neurons)(0)
      if (_n_neurons != _n_neurons_new)
        println(f"Changed _n_neurons from ${_n_neurons}%d to ${_n_neurons_new}%d.")
      _n_neurons = _n_neurons_new

      val _input_len_new: Int = _mults_per_thread * Math.ceil(_input_len.toFloat / _mults_per_thread).toInt
      val arr: Array[Float] = Array.fill[Float](_input_len_new - _input_len)(0)
      var new_Input = Array.fill[Array[Float]](_n_inputs)(Array.fill[Float](_input_len_new)(0))
      for {i <- 0 until _n_inputs
           j <- 0 until _input_len}
        new_Input(i)(j) = Input(i)(j)
      if (_input_len != _input_len_new)
        println(f"Changed _input_len from ${_input_len}%d to ${_input_len_new}%d.")
      _input_len = _input_len_new

      _local_size_0 = get_local_size_0()
      assert(_local_size_0 <= maxWorkGroupSize,
        f"Local size 0 must be equal or less than maxWorkGroupSize ($maxWorkGroupSize%d).")
      _local_size_1 = get_local_size_1()

      var changed_ls1 = false
      val original_ls1 = _local_size_1
      while (_n_inputs % _local_size_1 != 0) {
        _local_size_1 = _local_size_1 - 1
        changed_ls1 = true
      }
      if (changed_ls1)
        println(f"Changed local size 1 from $original_ls1%d to ${_local_size_1}%d.")

      _global_size_0 = get_global_size_0()
      _global_size_1 = get_global_size_1()


      assert(_n_inputs % _local_size_1 == 0,
        f"If the number of inputs (${_n_inputs}%d) is not a multiple of work group size in the " +
          f"respective dimension (${_local_size_1}%d), slide() will leave out some inputs.")
      assert(_n_neurons >= _neurons_per_wrg,
        f"_n_neurons(${_n_neurons}%d) must be bigger or equal to _neurons_per_wrg(${_neurons_per_wrg}%d).")


      val (output_layer_flat: Array[Float], runtime) =
        Execute(_local_size_0, _local_size_1, _global_size_0, _global_size_1, (true, true))(
          call_layer_f(0), new_Weights0, new_Biases0, Input)

      runtimes(0) = runtime
      outputs(0) = output_layer_flat.grouped(_n_neurons).toArray

      println(f"Layer 0: $runtime%1.5f ms")
      println()

      val file = new File(results_filename(exp_dir_name, _n_inputs))
      file.getParentFile.mkdirs()
      val pw = new PrintWriter(file)
      var finished_without_errors = false
      try {
        pw.write("device_name,f_name,n_inputs,mults_per_thread,neurons_per_wrg,layer_len0,activation_f0,runtime_l0,padded\n")
        pw.write(deviceName + "," + f_name + f",${_n_inputs}%d,${_mults_per_thread}%d,${_neurons_per_wrg}%d,")
        pw.write(f"${new_Biases0.length}%d,")
        pw.write(_Activation_fs(0).toString + ",")
        pw.write(f"${runtimes(0)}%1.5f,")
        pw.write(f"1\n")

        // TODO: do something about such big error
        //println(outputs(0)(0).mkString("\t"))
        finished_without_errors = true
        println(f"Done. Processed ${_n_inputs}%d inputs.")
      }
      finally {
        pw.close()
        if (!finished_without_errors) {
          new File(results_filename(exp_dir_name, _n_inputs)).delete()
          print(f"Input $input_no%d: ")
          println(new_Input(input_no).mkString("\t"))
          println(f"Output $input_no%d: ")
          print("\nWeights L0, N0: ")
          println(new_Weights0(0).mkString("\t"))
          print("Layer 0: ")
          println(outputs(0)(input_no).mkString("\t"))
          println(f"Target output $input_no%d: ")
          print("Layer X: ")
          println(Targets(input_no).mkString("\t"))
        }
      }
    }
  }


  class MLP_test(layer_f: (UserFun, Int) => Lambda,
                 mults_per_thread: Int) extends MLP_test_generic() {
    override val _mults_per_thread: Int = mults_per_thread

    override def call_layer_f(layer_i: Int): Lambda =
      layer_f(_Activation_fs(layer_i), _mults_per_thread)
  }


  class MLP_test2(layer_f: (UserFun, Int, Int, Int) => Lambda,
                  mults_per_thread: Int) extends MLP_test_generic() {
    override val _mults_per_thread: Int = mults_per_thread

    override def get_local_size_1(): Int =
      Math.min(_n_inputs, Math.floor(maxWorkGroupSize.toFloat / _local_size_0).toInt)

    override def get_global_size_1(): Int =
      _local_size_1 * Math.ceil(_n_inputs.toFloat / _local_size_1).toInt

    override def call_layer_f(layer_i: Int): Lambda =
      layer_f(_Activation_fs(layer_i), _mults_per_thread, _local_size_1, _n_neurons)
  }


  class MLP_test3(layer_f: (UserFun, Int, Int, Int, Int, Int, Int) => Lambda,
                  mults_per_thread: Int, neurons_per_wrg: Int) extends MLP_test_generic() {
    override val _mults_per_thread: Int = mults_per_thread
    override val _neurons_per_wrg: Int = neurons_per_wrg

    override def get_local_size_0(): Int =
      ((_neurons_per_wrg * _input_len).toFloat / _mults_per_thread).toInt
    override def get_local_size_1(): Int =
      Math.min(_n_inputs, Math.floor(maxWorkGroupSize.toFloat / _local_size_0).toInt)
    override def get_global_size_0(): Int =
      (_n_neurons.toFloat / _neurons_per_wrg).toInt * _local_size_0
    override def get_global_size_1(): Int =
      _local_size_1 * Math.ceil(_n_inputs.toFloat / _local_size_1).toInt

    override def call_layer_f(layer_i: Int): Lambda = {
      if (layer_i == 0) {
        assert((_input_len * _neurons_per_wrg) % _mults_per_thread == 0,
          f"(_input_len(${_input_len}%d) * _neurons_per_wrg(${_neurons_per_wrg}%d))" +
            f"(=(${_input_len * _neurons_per_wrg}%d)) %% _mults_per_thread(${_mults_per_thread}%d) must be equal to 0.")
        assert(_n_neurons % _neurons_per_wrg == 0,
          f"_n_neurons(${_n_neurons}%d) %% _neurons_per_wrg(${_neurons_per_wrg}%d) must be equal to 0.")
        layer_f(_Activation_fs(layer_i), _mults_per_thread, _local_size_1, _neurons_per_wrg,
          _n_neurons, _input_len, _n_inputs)
      }
      else
        layer_f(_Activation_fs(layer_i), 1, _local_size_1, 1, _n_neurons, _input_len, _n_inputs)
    }
  }

  //@Test
  def Sanity_MLP_in_2d_MrgdGrps_in_1d(): Unit = {
    new MLP_test2(f_layer_complex_neuron_mrgd_wrgs_in_1d, mults_per_thread=2)("f_layer_complex_neuron_mrgd_wrgs_in_1d",
      f"7. x3 2D-parallel kernels (across inputs). Each workgroup processes all inputs.", "experiment.sanity_check",
      Array(input_X, input_X2), Array(input_W1, input_W2, input_Wout),
      Array(input_b1, input_b2, input_bout), Array(gold, gold2),
      Array(ReLU, ReLU, Linear))
  }

  //@Test
  def Sanity_MLP_in_2d_MrgdGrps_in_2d(): Unit = {
    new MLP_test3(f_layer_complex_neuron_mrgd_wrgs_in_2d, mults_per_thread=2, neurons_per_wrg=2)("f_layer_complex_neuron_mrgd_wrgs_in_1d",
      f"\n8. x3 2D-parallel kernels (across inputs). Each workgroup processes all inputs and " +
        f"inputs are stored in local memory.", "experiment.sanity_check",
      Array(input_X, input_X2), Array(input_W1, input_W2, input_Wout),
      Array(input_b1, input_b2, input_bout), Array(gold, gold2),
      Array(ReLU, ReLU, Linear))
  }


  //@Test
  def MNIST_MLP_in_2d_Local(hidden_layers: Array[Int], n_inputs: Int, mults_per_thread: Int): Unit = {
    val (tf_X, tf_W, tf_B, tf_result, dir_name) = load_experiment(hidden_layers, n_inputs)
    new MLP_test(f_layer_complex_neuron_local, mults_per_thread)("f_layer_complex_neuron_local",
      "9. (MNIST dataset) x3 2D-parallel kernels (across inputs). Workgroup per neuron per input.",
      dir_name, tf_X, tf_W, tf_B, tf_result, Array(ReLU, ReLU, Linear))
  }

  //@Test
  def MNIST_MLP_in_2d(hidden_layers: Array[Int], n_inputs: Int, mults_per_thread: Int): Unit = {
    val (tf_X, tf_W, tf_B, tf_result, dir_name) = load_experiment(hidden_layers, n_inputs)
    new MLP_test(f_layer_complex_neuron, mults_per_thread)("f_layer_complex_neuron",
      "9.2. (MNIST dataset) x3 2D-parallel kernels (across inputs). Workgroup per neuron per input.",
      dir_name, tf_X, tf_W, tf_B, tf_result, Array(ReLU, ReLU, Linear))
  }

  //@Test
  def MNIST_MLP_in_2d_MrgdGrps_in_1d(hidden_layers: Array[Int], n_inputs: Int, mults_per_thread: Int): Unit = {
    val (tf_X, tf_W, tf_B, tf_result, dir_name) = load_experiment(hidden_layers, n_inputs)
    new MLP_test2(f_layer_complex_neuron_mrgd_wrgs_in_1d, mults_per_thread)("f_layer_complex_neuron_mrgd_wrgs_in_1d",
      f"10. (MNIST dataset) x3 2D-parallel kernels (across inputs). Workgroup per neuron per partition of inputs.",
      dir_name, tf_X, tf_W, tf_B, tf_result, Array(ReLU, ReLU, Linear))
  }

  //@Test
  def MNIST_MLP_in_2d_MrgdGrps_in_2d(hidden_layers: Array[Int], n_inputs: Int,
                                     mults_per_thread: Int, neurons_per_wrg: Int): Unit = {
    val (tf_X, tf_W, tf_B, tf_result, dir_name) = load_experiment(hidden_layers, n_inputs)
    new MLP_test3(f_layer_complex_neuron_mrgd_wrgs_in_2d, mults_per_thread, neurons_per_wrg)(
      f"f_layer_complex_neuron_mrgd_wrgs_in_2d",
      f"11. (MNIST dataset) x3 2D-parallel kernels (across inputs). " +
        f"Workgroup per partition of neurons per partition of inputs.", dir_name,
      tf_X, tf_W, tf_B, tf_result, Array(ReLU, ReLU, Linear))
  }

  //@Test
  def MNIST_MLP_in_2d_MrgdGrps_in_2d_coalesced(hidden_layers: Array[Int], n_inputs: Int,
                                               mults_per_thread: Int, neurons_per_wrg: Int): Unit = {
    val (tf_X, tf_W, tf_B, tf_result, dir_name) = load_experiment(hidden_layers, n_inputs)
    new MLP_test3(f_layer_complex_neuron_mrgd_wrgs_in_2d_coalesced, mults_per_thread, neurons_per_wrg)(
      f"f_layer_complex_neuron_mrgd_wrgs_in_2d_coalesced",
      f"12. (MNIST dataset) x3 2D-parallel kernels (across inputs). " +
        f"Workgroup per partition of neurons per partition of inputs." +
        "Memory accesses are coalesced.", dir_name,
      tf_X, tf_W, tf_B, tf_result, Array(ReLU, ReLU, Linear))
  }
}
