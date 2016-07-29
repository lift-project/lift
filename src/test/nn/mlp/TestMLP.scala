package nn.mlp

/**
  * Created by Naums Mogers in July 2016.
  * This file implements the Lift version of the Multilayer Perceptron forward-propagation.
  */

import apart.arithmetic.SizeVar
import ir.ArrayType
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import opencl.executor.{Execute, Executor}
import org.junit.{AfterClass, BeforeClass, Test}

import scala.util.parsing.json._

object TestMLP {
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

class TestMLP {

  def load_2d_float_json(json_file_name: String): Array[Array[Float]] = {
    /* Load an array from a JSON file */

    val source = scala.io.Source.fromFile(System.getProperty("user.dir") + "/../../src/test/nn/mlp/" + json_file_name)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    var w_list: List[List[Double]] = json.get.asInstanceOf[List[List[Double]]]

    // Convert from List[List[Double]] to Array[Array[Float]]
    var i = 0
    var w_arr = Array.fill[Array[Float]](w_list.length)(Array.fill[Float](w_list.head.length)(0))
    var aline = Array.fill[Double](w_list.head.length)(0)
    var j = 0
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

    val source = scala.io.Source.fromFile(System.getProperty("user.dir") + "/../../src/test/nn/mlp/" + json_file_name)
    val jsonString = source.getLines mkString "\n"
    source.close()
    val json:Option[Any] = JSON.parseFull(jsonString)
    var w_list: List[Double] = json.get.asInstanceOf[List[Double]]

    // Convert from List[List[Double]] to Array[Array[Float]]
    var i = 0
    var w_arr = Array.fill[Float](w_list.length)(0)
    var aline = Array.fill[Double](w_list.length)(0)
    aline = w_list.to[Array]
    for (i <- aline.indices) {
      w_arr(i) = aline(i).toFloat
    }
    w_arr
  }

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
          toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_per_neuron)) o Join() o
          MapLcl(0)(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
          Split(tile_of_mults_size) o fun((X_local) => Zip(X_local, ws_per_neuron)) o
          toLocal(MapLcl(1)(id)) $ Get(tup_per_inp, 2)/*X*/})) $
        Zip(Get(tup_per_inp, 0)/*W*/, Get(tup_per_inp, 1)/*B*/)})) o
      MapSeq(fun((X_per_input) => {Tuple(W, B, X_per_input)})) $ X
    }
  )

  /* Parallel layer (across inputs as well), 2-dimensional, multiple threads per neuron *
  * Each workgroup processes all inputs. This is done so that the X array can be shared in local memory
  *  for faster access times (in f_layer_complex_neuron_mrgd_wrgs_local()).
  * For the case of a bit of neurons and a lot of inputs */
  def f_layer_complex_neuron_mrgd_wrgs_in_1d(activation_f: UserFun, tile_of_mults_size: Int) = fun(
    ArrayType(ArrayType(Float, layer_idim), layer_odim),
    ArrayType(Float, layer_odim),
    ArrayType(ArrayType(Float, layer_idim), layer_ninputs),
    (W, B, X) => {
      MapWrg(1)(fun((X_) => {
        MapWrg(0)(fun((ws_per_neuron, b_per_neuron) => {
          MapLcl(1)(fun((X_per_input) => {
            toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_per_neuron)) o Join() o
            MapLcl(0)(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
            Split(tile_of_mults_size) $ Zip(X_per_input, ws_per_neuron)})) $ X_})) $
        Zip(W, B)})) o Split(1) $ X
    }
  )

  /* Parallel layer (across inputs as well), 2-dimensional, multiple threads per neuron *
  * Each workgroup processes all inputs. This is done so that weights can be shared in
  * local memory for faster access times.
  * For the case of a bit of neurons and a lot of inputs */
  def f_layer_complex_neuron_mrgd_wrgs_in_1d_local(activation_f: UserFun, tile_of_mults_size: Int) = fun(
    ArrayType(ArrayType(Float, layer_idim), layer_odim),
    ArrayType(Float, layer_odim),
    ArrayType(ArrayType(Float, layer_idim), layer_ninputs),
    (W, B, X) => {
      MapWrg(1)(fun((X_unwrapped) => {
        MapWrg(0)(fun((ws_per_neuron, b_per_neuron) => {
          MapLcl(1)(fun((X_per_input) => {
            toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(b_per_neuron)) o Join() o
            MapLcl(0)(toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
            Split(tile_of_mults_size) $ Zip(X_per_input, ws_per_neuron)})) $ X_unwrapped})) $
        Zip(toLocal(MapLcl(1)(MapSeq(id))) $ W,
            toLocal(MapLcl(1)(id)) $ B)})) o /* Wrap: */Split(1) $ X
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

  val tf_W1 = load_2d_float_json("W1.json")
  val tf_W2 = load_2d_float_json("W2.json")
  val tf_Wout = load_2d_float_json("Wout.json")
  val tf_W = Array(tf_W1, tf_W2, tf_Wout)
  val tf_B1 = load_1d_float_json("b1.json")
  val tf_B2 = load_1d_float_json("b2.json")
  val tf_Bout = load_1d_float_json("bout.json")
  val tf_B = Array(tf_B1, tf_B2, tf_Bout)
  val tf_X = load_2d_float_json("test_images.json")
  val tf_result = load_2d_float_json("test_tf_results.json")

  //@Test
  def MLPSequential(): Unit = {
    val (lift_result: Array[Float], runtime) = Execute(1,1)(
      mlp_fprop_seq(Linear), input_W1, input_b1, input_W2, input_b2, input_Wout, input_bout, input_X)
    println(f"\n1. Everything sequential, single kernel.\n" +
            f"Runtime: $runtime%1.5f ms")
    //println(lift_result.mkString(", "))
    //println(gold.mkString(", "))

    assertArrayEquals(gold, lift_result, 0.0001f)
  }

  //@Test
  def MLPThreeSeqKernels(): Unit = {
    val (output_layer1: Array[Float], runtime_layer1) = Execute(1,1)(
      f_layer_seq(Linear), input_W1, input_b1, input_X)
    val (output_layer2: Array[Float], runtime_layer2) = Execute(1,1)(
      f_layer_seq(Linear), input_W2, input_b2, output_layer1)
    val (lift_result: Array[Float], runtime_layerout) = Execute(1,1)(
      f_layer_seq(Linear), input_Wout, input_bout, output_layer2)
    println(f"\n2. x3 sequential kernels.\n" +
            f"Runtime: $runtime_layer1%1.5f + $runtime_layer2%1.5f + $runtime_layerout%1.5f = " +
            f"${runtime_layer1 + runtime_layer2 + runtime_layerout}%1.5f ms")

    assertArrayEquals(gold, lift_result, 0.0001f)
  }

  //@Test
  def MLP(): Unit = {
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

    assertArrayEquals(gold, lift_result, 0.0001f)
  }

  //@Test
  def MLP_in_1d(): Unit = {
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

    assertArrayEquals(gold, lift_result(0), 0.0001f)
    assertArrayEquals(gold2, lift_result(1), 0.0001f)
  }

  //@Test
  def MLP_in_2d(): Unit = {
    val N_inputs = 2
    var input_size = input_W1(0).length
    val N_neurons_h1 = input_W1.length
    var mults_per_thread = 2
    val (output_layer1_flat: Array[Float], runtime_layer1) =
      Execute(input_size / mults_per_thread, 1, N_neurons_h1 * input_size / mults_per_thread,
              N_inputs, (false, false))(
                f_layer_complex_neuron(Linear, tile_of_mults_size=mults_per_thread),
                                       input_W1, input_b1, Array(input_X, input_X2))
    val output_layer1 = output_layer1_flat.grouped(N_neurons_h1).toArray

    input_size = N_neurons_h1
    val N_neurons_h2 = input_W2.length
    mults_per_thread = 2
    val (output_layer2_flat: Array[Float], runtime_layer2) =
      Execute(input_size / mults_per_thread, 1, N_neurons_h2 * input_size / mults_per_thread,
              N_inputs, (false, false))(
                f_layer_complex_neuron(Linear, tile_of_mults_size=mults_per_thread),
                input_W2, input_b2, output_layer1)
    val output_layer2 = output_layer2_flat.grouped(input_W2.length).toArray

    input_size = N_neurons_h2
    val N_neurons_hout = input_Wout.length
    mults_per_thread = 2
    val (lift_result_flat: Array[Float], runtime_layerout) =
      Execute(input_size / mults_per_thread, 1, N_neurons_hout * input_size / mults_per_thread,
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

    assertArrayEquals(gold, lift_result(0), 0.0001f)
    assertArrayEquals(gold2, lift_result(1), 0.0001f)
  }

  //@Test
  def MLP_in_2d_Local(): Unit = {
    val N_inputs = 2
    var input_size = input_W1(0).length
    val N_neurons_h1 = input_W1.length
    var mults_per_thread = 2
    val (output_layer1_flat: Array[Float], runtime_layer1) =
      Execute(input_size / mults_per_thread, 1, N_neurons_h1 * input_size / mults_per_thread,
              N_inputs, (false, false))(
                f_layer_complex_neuron_local(Linear, tile_of_mults_size=mults_per_thread),
                                       input_W1, input_b1, Array(input_X, input_X2))
    val output_layer1 = output_layer1_flat.grouped(N_neurons_h1).toArray

    input_size = N_neurons_h1
    val N_neurons_h2 = input_W2.length
    mults_per_thread = 2
    val (output_layer2_flat: Array[Float], runtime_layer2) =
      Execute(input_size / mults_per_thread, 1, N_neurons_h2 * input_size / mults_per_thread,
              N_inputs, (false, false))(
                f_layer_complex_neuron_local(Linear, tile_of_mults_size=mults_per_thread),
                input_W2, input_b2, output_layer1)
    val output_layer2 = output_layer2_flat.grouped(input_W2.length).toArray

    input_size = N_neurons_h2
    val N_neurons_hout = input_Wout.length
    mults_per_thread = 2
    val (lift_result_flat: Array[Float], runtime_layerout) =
      Execute(input_size / mults_per_thread, 1, N_neurons_hout * input_size / mults_per_thread,
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

    assertArrayEquals(gold, lift_result(0), 0.0001f)
    assertArrayEquals(gold2, lift_result(1), 0.0001f)
  }

  //@Test
  def MLP_in_2d_MrgdGrps_in_1d(): Unit = {
    val N_inputs = 2
    var input_size = input_W1(0).length
    val N_neurons_h1 = input_W1.length
    var mults_per_thread = 2
    val (output_layer1_flat: Array[Float], runtime_layer1) =
      Execute(input_size / mults_per_thread, N_inputs, N_neurons_h1 * input_size / mults_per_thread,
              N_inputs, (false, false))(
                f_layer_complex_neuron_mrgd_wrgs_in_1d(Linear, tile_of_mults_size = mults_per_thread),
                input_W1, input_b1, Array(input_X, input_X2))
    val output_layer1 = output_layer1_flat.grouped(N_neurons_h1).toArray

    input_size = N_neurons_h1
    val N_neurons_h2 = input_W2.length
    mults_per_thread = 2
    val (output_layer2_flat: Array[Float], runtime_layer2) =
      Execute(input_size / mults_per_thread, N_inputs, N_neurons_h2 * input_size / mults_per_thread,
              N_inputs, (false, false))(
                f_layer_complex_neuron_mrgd_wrgs_in_1d(Linear, tile_of_mults_size = mults_per_thread),
                input_W2, input_b2, output_layer1)
    val output_layer2 = output_layer2_flat.grouped(input_W2.length).toArray

    input_size = N_neurons_h2
    val N_neurons_hout = input_Wout.length
    mults_per_thread = 2
    val (lift_result_flat: Array[Float], runtime_layerout) =
      Execute(input_size / mults_per_thread, N_inputs, N_neurons_hout * input_size / mults_per_thread,
              N_inputs, (false, false))(
                f_layer_complex_neuron_mrgd_wrgs_in_1d(Linear, tile_of_mults_size = mults_per_thread),
                input_Wout, input_bout, output_layer2)
    val lift_result = lift_result_flat.grouped(input_Wout.length).toArray

    println(f"\n7. x3 2D-parallel kernels (across inputs). Each workgroup processes all inputs.\n" +
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

    assertArrayEquals(gold, lift_result(0), 0.0001f)
    assertArrayEquals(gold2, lift_result(1), 0.0001f)
  }

  //@Test
  def MLP_in_2d_MrgdGrps_in_1d_Local(): Unit = {
    val N_inputs = 2
    var input_size = input_W1(0).length
    val N_neurons_h1 = input_W1.length
    var mults_per_thread = 2
    val (output_layer1_flat: Array[Float], runtime_layer1) =
      Execute(input_size / mults_per_thread, N_inputs, N_neurons_h1 * input_size / mults_per_thread,
        N_inputs, (false, false))(
        f_layer_complex_neuron_mrgd_wrgs_in_1d_local(Linear, tile_of_mults_size=mults_per_thread),
        input_W1, input_b1, Array(input_X, input_X2))
    val output_layer1 = output_layer1_flat.grouped(N_neurons_h1).toArray

    input_size = N_neurons_h1
    val N_neurons_h2 = input_W2.length
    mults_per_thread = 2
    val (output_layer2_flat: Array[Float], runtime_layer2) =
      Execute(input_size / mults_per_thread, N_inputs, N_neurons_h2 * input_size / mults_per_thread,
        N_inputs, (false, false))(
        f_layer_complex_neuron_mrgd_wrgs_in_1d_local(Linear, tile_of_mults_size=mults_per_thread),
        input_W2, input_b2, output_layer1)
    val output_layer2 = output_layer2_flat.grouped(input_W2.length).toArray

    input_size = N_neurons_h2
    val N_neurons_hout = input_Wout.length
    mults_per_thread = 2
    val (lift_result_flat: Array[Float], runtime_layerout) =
      Execute(input_size / mults_per_thread, N_inputs, N_neurons_hout * input_size / mults_per_thread,
        N_inputs, (false, false))(
        f_layer_complex_neuron_mrgd_wrgs_in_1d_local(Linear, tile_of_mults_size=mults_per_thread),
        input_Wout, input_bout, output_layer2)
    val lift_result = lift_result_flat.grouped(input_Wout.length).toArray

    println(f"\n8. x3 2D-parallel kernels (across inputs). Each workgroup processes all inputs and " +
            f"inputs are stored in local memory.\n" +
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

    assertArrayEquals(gold, lift_result(0), 0.0001f)
    assertArrayEquals(gold2, lift_result(1), 0.0001f)
  }

  @Test
  def MLP_MNIST_in_2d_Local(): Unit = {
    MLP_test("9. (MNIST dataset) x3 2D-parallel kernels (across inputs). Each workgroup processes all inputs and " +
             f"inputs are stored in local memory.",
             f_layer_complex_neuron_local, mults_per_thread=2, tf_X, tf_W, tf_B, Array(ReLU, ReLU, Linear))
  }

  def MLP_test(description: String, layer_f: (UserFun, Int) => Lambda3,
               mults_per_thread: Int, Inputs: Array[Array[Float]],
               Weights: Array[Array[Array[Float]]], Biases: Array[Array[Float]],
               Activation_fs: Array[UserFun]): Unit = {
    var input_size = 0
    val N_inputs = Inputs.length
    val N_layers = Biases.length
    var N_neurons = 0
    val runtimes = Array.fill[Double](N_layers)(0)
    var Input = Inputs
    //var outputs = Array.fill[Array[Float]](N_layers)(Array.fill[Float](N_layers)(0))
    // Size of the last two dimensions will be different for each layer, and determined in the for loop
    var outputs = Array.ofDim[Float](N_layers, N_inputs, 1)

    print(f"\n" + description + "\nRuntime:\n")
    for (layer_i <- 0 until N_layers) {
      input_size = Weights(layer_i)(0).length
      N_neurons = Biases(layer_i).length

      val (output_layer_flat: Array[Float], runtime) =
        Execute(input_size / mults_per_thread, 1, N_neurons * input_size / mults_per_thread,
                N_inputs, (false, false))(
                layer_f(Activation_fs(layer_i), mults_per_thread),
                  Weights(layer_i), Biases(layer_i), Input)

      runtimes(layer_i) = runtime
      outputs(layer_i) = output_layer_flat.grouped(N_neurons).toArray

      if (layer_i != N_layers - 1)
        Input = outputs(layer_i)

      println(f"Layer $layer_i%d: $runtime%1.5f ms")
    }
    println()

    var i = 0
    for ((lift_single_result, tf_single_result) <- outputs.last zip tf_result) {
      assertArrayEquals(tf_single_result, lift_single_result, 0.002f) // TODO: do something about such big error
    }
  }
}