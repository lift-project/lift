package nn.rnn

/**
  * Created by Naums Mogers in July 2016.
  * This file implements two versions of the Hopfield Recurrent Neural network propagation.
  * Scala version propagate_scala() is based on Scan() and supports multiple iterations and different inputs.
  * Lift version propagate_lift() is based on Iterate(), supports multiple iterations, but same input for all iterations, and
  * it returns the output only of the last iteration.
  * Further development of Lift RNN requires implementing the Lift version of Scan().
  * To be completed.
  */

import lift.arithmetic.SizeVar
import ir.ArrayType
import ir.ast._
import nn.Array2D
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import opencl.executor.{Execute, Executor}
import org.junit.{AfterClass, BeforeClass, Test}

object TestRNN {
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

class TestRNN {
  def propagate_scala(S0: Array[Float], W: Array2D[Float], X: Array2D[Float]): Array2D[Float] = {
    /*
    Scala implementation of the Hopfield Recurrent Neural network (multiple iterations, different inputs per iteration)
    * returns: Propagation results of all time units: Array(Array(Float, N_neurons), N_iterations)
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
  def rnnScalaVsLift(): Unit = {
    // N: number of neurons
    val N = SizeVar("N")
    // D: number of iterations
    val D = 1

    // TODO: fix a bug, which causes f_rnn to return wrong results if D != 1
    val propagate_lift = fun(
      ArrayType(Float, N),
      ArrayType(ArrayType(Float, N), N),
      ArrayType(Float, N),
      (S0, W, X) => {
        MapGlb(id) o
        Iterate(D)(
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
        S0})

    val input_S0 = Array(0.0f, 1f, 2f)
    val input_W = Array(Array(0.0f, 1f, 2f), Array(3f, 0.0f, 5f), Array(6f, 7f, 0.0f))
    //val input_X = Array(Array(3f, 4f, 5f), Array(6f, 7f, 8f))
    val input_X = Array(3f, 4f, 5f)
    val gold = Array(Array(0.0f, 1f, 2f), Array(8f, 14f, 12f), Array(44f, 91f, 154f))

    val scala_result = propagate_scala(input_S0, input_W, Array(input_X))

    // Test the Scala baseline version of RNN
    (gold zip scala_result).foreach{case (gold_iter_result: Array[Float], scala_iter_result: Array[Float]) =>
      assertArrayEquals(gold_iter_result, scala_iter_result, 0.0f)}

    val (lift_result: Array[Float], _) = Execute(128)(propagate_lift, input_S0, input_W, input_X)
    //println(lift_result.mkString(", "))

    assertArrayEquals(gold(1), lift_result, 0.0001f)
  }
}
