package mlp

/**
  * Created by Naums Mogers.
  */

import apart.arithmetic.SizeVar
import ir.ArrayType
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import opencl.executor.{Execute, Executor}
import org.junit.{AfterClass, BeforeClass, Test}

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
  //def propagate(S0: Array[Float], W: Array[Array[Float]], X: Array[Array[Float]]): Array[Array[Float]] = {
  def propagate(S0: Array[Float], W: Array[Array[Float]], X: Array[Array[Float]]): Array[Array[Float]] = {
    /*
    Scala implementation of the Multi-Layer Perceptron
    * returns: Propagation results of all time units
    */
    // Spread across time units
    X.scanLeft(S0){case (s_prev: Array[Float], x_current: Array[Float]) =>
      // Spread across neurons
      (W zip x_current).map{case (ws_per_neuron: Array[Float], x_per_neuron: Float) =>
        // Associate outputs from previous state with corresponding weights
        (s_prev zip ws_per_neuron).map{case (s_per_neuron: Float, w_per_synapse: Float) =>
          // Multiply previous state outputs by corresponding weights
          s_per_neuron * w_per_synapse}.
          // Perform signal summation
          foldLeft(x_per_neuron){case (a: Float, b: Float) => a + b}
      }
    }
  }

  @Test
  def mlpScalaVsLift(): Unit = {
    //val input_S0 = Array(0.0f, 1f, 2f)
    //val input_W = Array(Array(0.0f, 1f, 2f), Array(3f, 0.0f, 5f), Array(6f, 7f, 0.0f))
    //val input_X = Array(Array(3f, 4f, 5f), Array(6f, 7f, 8f))
    //val input_X = Array(3f, 4f, 5f)
    //val gold = Array(Array(0.0f, 1f, 2f), Array(8f, 14f, 12f), Array(44f, 91f, 154f))
    //wb, w0, w1, w2, w3, ...
    val input_W1 = Array(Array(0.0f, 0.1f, 0.2f, 0.3f, 0.4f),
                         Array(0.1f, 0.2f, 0.3f, 0.4f, 0.0f),
                         Array(0.2f, 0.3f, 0.4f, 0.0f, 0.1f),
                         Array(0.3f, 0.4f, 0.0f, 0.1f, 0.2f))
    val input_b1 = Array(0.1f, 0.1f, 0.1f, 0.1f)
    val input_W2 = Array(Array(0.0f, 0.1f, 0.2f, 0.3f),
                         Array(0.1f, 0.2f, 0.3f, 0.0f),
                         Array(0.2f, 0.3f, 0.0f, 0.1f))
    val input_b2 = Array(0.1f, 0.1f, 0.1f)
    val input_Wout = Array(Array(0.0f, 0.1f, 0.2f),
                           Array(0.1f, 0.2f, 0.0f),
                           Array(0.2f, 0.0f, 0.1f),
                           Array(0.0f, 0.1f, 0.2f),
                           Array(0.1f, 0.2f, 0.0f))
    val input_bout = Array(0.1f, 0.1f, 0.1f, 0.1f, 0.1f)
    val input_X = Array(6f, 7f, 8f, 9f, 5f)
    //val gold = Array(Array(0.0f, 1f, 2f), Array(8f, 14f, 12f))
    val gold = Array(1.478f, 1.443f, 1.423f, 1.478f, 1.443f)

    //val scala_result = propagate(input_S0, input_W, Array(input_X))

    // Test the Scala baseline version of RNN
    /*(gold zip scala_result).foreach{case (gold_iter_result: Array[Float], scala_iter_result: Array[Float]) =>
      assertArrayEquals(gold_iter_result, scala_iter_result, 0.0f)}*/

    // N: number of neurons
    //val N = SizeVar("N")
    // D: number of iterations
    //val D = 3

    /*val f = fun(
      ArrayType(Float, N),
      ArrayType(ArrayType(Float, N), N),
      ArrayType(Float, N),
      (S0, W, X) => {
        MapGlb(id) o
        Iterate(1)(
          // y0=weighted_sum_neuron0, y1=weighted_sum_neuron1, .., yn=weighted_sum_neuronN
          fun(ArrayType(Float, N), s_prev =>
            Join() o MapGlb(
              // ws_per_neuron = (wi0, ..., win)
              // x_per_neuron = x
              // (x0 + s_0*w00 + ... + s_0*w0n), ..., (xn + s_n*wn0 + ... + s_n*wnn)
              fun(ArrayType(Float, N), Float, (ws_per_neuron, x_per_neuron) =>
                // x + s_i*wi0 + s_i*wi1 + ... + s_i*win
                ReduceSeq(add, x_per_neuron) o
                // s_i*wi0, ..., s_i*win
                MapSeq(mult) $
                // (s_i, wi0), ..., (s_i, win)
                Zip(s_prev, ws_per_neuron))) $
            // ((w00, ..., w0n), x0), ((w10, ..., w1n), x1), .., ((wn0, ..., wnn), xn)
            Zip(W, X))) $
        S0})*/
    val layer_idim = SizeVar("layer_idim")
    val layer_odim = SizeVar("layer_odim")
    def f_layer = fun(
      ArrayType(ArrayType(Float, layer_idim), layer_odim),
      ArrayType(Float, layer_odim),
      ArrayType(Float, layer_idim),
      (W_layer, b_layer, X) => {
        Join() o MapSeq(fun((ws_per_neuron, b_per_neuron) => {
          toGlobal(MapSeq(id)) o
          ReduceSeq(add, id(b_per_neuron)) o
          MapSeq(mult) $
          Zip(X, ws_per_neuron)
        })) $ Zip(W_layer, b_layer)
      }
    )
    val idim = SizeVar("idim")
    val hdim1 = SizeVar("hdim1")
    val hdim2 = SizeVar("hdim2")
    val odim = SizeVar("odim")
    val f_mlp = fun(
      ArrayType(ArrayType(Float, idim), hdim1), ArrayType(Float, hdim1),
      ArrayType(ArrayType(Float, hdim1), hdim2), ArrayType(Float, hdim2),
      ArrayType(ArrayType(Float, hdim2), odim), ArrayType(Float, odim),
      ArrayType(Float, idim),
      (W1, b1, W2, b2, Wout, bout, X) => {
        toGlobal(MapGlb(id)) $ f_layer(Wout, bout, f_layer(W2, b2, f_layer(W1, b1, X)))}
    )

    val (lift_result: Array[Float], _) = Execute(256)(f_mlp, input_W1, input_b1, input_W2,
                                                      input_b2, input_Wout, input_bout, input_X)
    println(lift_result.mkString(", "))

    assertArrayEquals(gold, lift_result, 0.0001f)
  }
}
