package rnn

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

class TestRNN {
  def propagate(S0: Array[Float], W: Array[Array[Float]], X: Array[Array[Float]]): Array[Array[Float]] = {
    /*
    Scala implementation of RNN network (across multiple time units and corresponding different inputs)
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
  def rnnScalaVsLift(): Unit = {
    val S0 = Array(0f, 1f, 2f)
    val W = Array(Array(0f, 1f, 2f), Array(3f, 0f, 5f), Array(6f, 7f, 0f))
    val X = Array(Array(3f, 4f, 5f), Array(6f, 7f, 8f))
    val gold = Array(Array(0f, 1f, 2f), Array(8f, 14f, 12f), Array(44f, 91f, 154f))

    val actual_result = propagate(S0, W, X)

    // Test the Scala baseline version of RNN
    (gold zip actual_result).foreach{case (gold_iter_result: Array[Float], actual_iter_result: Array[Float]) =>
      assertArrayEquals(gold_iter_result, actual_iter_result, 0f)}

    // N: number of neurons
    val N = SizeVar("N")
    // D: simulation duration
    val D = SizeVar("D")

    // Sequential RNN that outputs computation result only at the last time unit
    // Throws an error:
    /*val f = fun(
      ArrayType(Float, N),
      ArrayType(ArrayType(Float, N), N),
      ArrayType(ArrayType(Float, N), D),
      (S0, W, X) => {
        Iterate(D)(
          ReduceSeq(
            fun(ArrayType(Float, N),
              ArrayType(Float, N),
              (s_prev, x_current) => {
                MapSeq(fun(ArrayType(Float, N),
                  ArrayType(Float, N),
                  (ws_per_neuron, x_per_neuron) => {
                    ReduceSeq(add, x_per_neuron) o
                      MapSeq(mult) $
                      Zip(s_prev, ws_per_neuron)})) $ Zip(W, x_current)}), S0)) $ (S0, W, X)})*/

    // Reconstructing the code above...
    val f = fun(
      ArrayType(Float, N),
      Float,
      (S0, X) =>
        ReduceSeq(add, X) $ S0)
  }
}
