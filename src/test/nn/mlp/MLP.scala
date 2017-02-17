package nn.mlp

/**
  * Created by Naums Mogers.
  * This file implements the Lift version of the Multilayer Perceptron forward-propagation.
  */

import ir.ArrayType
import ir.ast._
import lift.arithmetic.SizeVar
import nn.TestUtils
import opencl.ir._
import opencl.ir.pattern._

object MLP {
  val floatSize = 4

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
  def Seq(activation_f: UserFun) = fun(
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

  /* Parallel layer (across inputs as well) */
  def Par(activation_f: UserFun,
          tile_of_mults_size: Int,
          tile_of_inputs_size: Int,
          tile_of_neurons_size: Int,
          odim: Int, idim: Int, nInputs: Int) = fun(
    ArrayType(ArrayType(Float, idim), odim),
    ArrayType(Float, odim),
    ArrayType(ArrayType(Float, idim), nInputs),
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
                  MapSeq(fun((ws_per_neuron) => {ReorderStride(tile_of_mults_size) $ //Coalescing: or idim / tile_of_mults_size
                    Zip(X_single, ws_per_neuron)})) $ ws_per_tile})) $
                X_tile})) $ Zip(Split(tile_of_neurons_size) $ W, Split(tile_of_neurons_size) $ B)})) o
        Split(tile_of_inputs_size) $ X
    }
  )
}

class MLP(/* MLP architectural parameters */
          val liftMLP: (UserFun, Int, Int, Int, Int, Int, Int) => Lambda,
          val activationFun: Array[UserFun],
          /* MLP parallelisation parameters */
          val multsPerThread: Int, val neuronsPerWrg: Int,
          /* MLP application parameters */
          val inputsNonPadded: Array[Array[Float]],
          val weights: Array[Array[Array[Float]]], val biases: Array[Array[Float]],
          val targets: Array[Array[Float]],
          val liftFunName: String, val testDescription: String, val pathToResults: String) {
  /**
    * Initializes variables, computes workgroup sizes.
    */

  var nInputs: Int = inputsNonPadded.length
  var nInputsPadded: Int = nInputs
  val nLayers: Int = biases.length
  val nNeurons: Array[Int] = Array.fill[Int](nLayers)(0)
  val inputLen: Array[Int] = Array.fill[Int](nLayers)(0)
  val runTimes: Array[Double] = Array.fill[Double](nLayers)(0)
  val localSize0: Array[Int] = Array.fill[Int](nLayers)(0)
  val localSize1: Array[Int] = Array.fill[Int](nLayers)(0)
  val globalSize0: Array[Int] = Array.fill[Int](nLayers)(0)
  val globalSize1: Array[Int] = Array.fill[Int](nLayers)(0)

  var outputs = scala.collection.mutable.Buffer[Array[Array[Float]]]()
  def layerInputs(layerNo: Int): Array[Array[Float]] = if (layerNo == 0) inputsPadded else inputsPadded//outputs(layerNo - 1)

  var inputsPadded: Array[Array[Float]] = _

  for (layerNo <- 0 until nLayers) {
    inputLen(layerNo) = weights(layerNo)(0).length
    if (inputLen(layerNo) % multsPerThread != 0)
      throw new java.lang.IllegalArgumentException(
        f"Layer $layerNo%d input length (${inputLen(layerNo)}%d) must be divisible " +
          f"by multsPerThread ($multsPerThread%d).")

    nNeurons(layerNo) = biases(layerNo).length

    // Padding
    //val (next_Weights, next_Biases) = padParams(Weights(layerNo), Biases(layerNo))

    /* Compute workgroup and global sizes */
    // Layer size 0 = the total size of the workgroup job / the size of the job per thread
    localSize0(layerNo) = Math.ceil((neuronsPerWrg * inputLen(layerNo)).toFloat / multsPerThread).toInt
    if (localSize0(layerNo) > TestUtils.maxWorkGroupSize)
      throw new java.lang.IllegalArgumentException(
      f"Layer $layerNo%d: local size 0 must be less or equal to maxWorkGroupSize" +
        f"(${TestUtils.maxWorkGroupSize}%d).\nDecrase neuronsPerWrg or increase multsPerThread.")

    // Layer size 1 = the minimum of the maximum allowed layer size 1 considering layer size 0 and
    // the number of inputs (the size of the workgroup job)
    localSize1(layerNo) = Math.floor(TestUtils.maxWorkGroupSize.toFloat / localSize0(layerNo)).toInt
    if (localSize1(layerNo) > nInputs)
      localSize1(layerNo) = nInputs
  }
  // Pad and compute global sizes
  inputsPadded = padInputs(inputsNonPadded)
  for (layerNo <- 0 until nLayers) {

    // Global size 0 = (the total number of neurons / neurons per workgroup) * the number of threads per workgroup
    globalSize0(layerNo) = (nNeurons(layerNo).toFloat / neuronsPerWrg).toInt * localSize0(layerNo)
    // Global size 1 = mapping of workgroups across all inputs
    globalSize1(layerNo) = localSize1(layerNo) * Math.ceil(nInputsPadded.toFloat / localSize1(layerNo)).toInt

    // Padding
    //var next_Input = padInputs(Input)

    /*assert(nInputsPadded % localSize1(layerNo) == 0,
      f"Layer $layerNo%d: if the number of inputs (${nInputsPadded}%d) is not a multiple of work group size in the " +
        f"respective dimension (${localSize1}%d), slide() will leave out some inputs.")*/
    assert(nNeurons(layerNo) >= neuronsPerWrg,
      f"Layer $layerNo%d: nNeurons(${nNeurons(layerNo)}%d) must be bigger or equal to neuronsPerWrg($neuronsPerWrg%d).")
  }

  // TODO: investigate if same parallelization
  /*def call_layer_f(layer_i: Int): Lambda = {
    if (layer_i == 0) {
      assert((inputLen * neuronsPerWrg) % multsPerThread == 0,
        f"(_input_len(${inputLen}%d) * _neurons_per_wrg(${neuronsPerWrg}%d))" +
          f"(=(${inputLen * neuronsPerWrg}%d)) %% _mults_per_thread(${multsPerThread}%d) must be equal to 0.")
      assert(nNeurons % neuronsPerWrg == 0,
        f"_n_neurons(${nNeurons}%d) %% _neurons_per_wrg(${neuronsPerWrg}%d) must be equal to 0.")
      liftMLP(activationFunctions(layer_i), multsPerThread, localSize1, neuronsPerWrg,
        nNeurons, inputLen, nInputs)
    }
    else
      liftMLP(activationFunctions(layer_i), 1, localSize1, 1, nNeurons, inputLen, nInputs)
  }*/


  def padInputs(inputs: Array[Array[Float]]): Array[Array[Float]] = {
    // Make sure that all threads have equal amount of input samples to process
    def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
    def lcm(a: Int, b: Int): Int = (a * b).abs / gcd(a, b)
    def getLCMOfSizes(layerN: Int): Int =
      if (layerN == localSize1.length) 1 else lcm(getLCMOfSizes(layerN + 1), localSize1(layerN))
    val LCMOfSizes: Float = getLCMOfSizes(0).toFloat
    nInputsPadded = (LCMOfSizes * Math.ceil(nInputs.toFloat / LCMOfSizes)).toInt
    if (nInputs != nInputsPadded) {
      val inputsNew = inputs ++ Array.fill[Array[Float]](nInputsPadded - nInputs)(Array.fill[Float](inputLen(0))(0.0f))
      println(f"Changed _n_inputs from ${nInputs}%d to ${nInputsPadded}%d.")
      inputsNew
    } else inputs
  }
/*
  def padInput(Inputs: Array[Array[Float]]): Array[Array[Float]] = {
    // Make sure that all threads have equal amount of input bits to process
    val input_len_new: Int = multsPerThread * Math.ceil(inputLen.toFloat / multsPerThread).toInt
    if (inputLen != input_len_new) {
      var new_Input = Array.fill[Array[Float]](nInputs)(Array.fill[Float](input_len_new)(0))
      for {i <- 0 until nInputs
           j <- 0 until inputLen}
        new_Input(i)(j) = Inputs(i)(j)
      println(f"Changed _input_len from ${inputLen}%d to ${input_len_new}%d.")
      inputLen = input_len_new
      new_Input
    } else Inputs
  }

  def padParams(Weights: Array[Array[Float]], Biases: Array[Float]):
  (Array[Array[Float]], Array[Float]) = {
    // Make sure that all workgroups have equal amount of inputs to process
    val n_neurons_new: Int = neuronsPerWrg * Math.ceil(nNeurons.toFloat / neuronsPerWrg).toInt
    // Padding in both dimensions, to account for padded inputs as well
    var new_Weights = Array.fill[Array[Float]](n_neurons_new)(Array.fill[Float](inputLen)(0))
    for {i <- 0 until n_neurons_new
         j <- 0 until inputLen} {
      new_Weights(i)(j) = Weights(i)(j)
    }

    if (nNeurons != n_neurons_new) {
      //val new_Weights: Array[Array[Float]] =
      //  Weights ++ Array.fill[Array[Float]](n_neurons_new - _n_neurons)(Array.fill[Float](_input_len)(0))
      val new_Biases = Biases ++ Array.fill[Float](n_neurons_new - nNeurons)(0)
      println(f"Changed _n_neurons from ${nNeurons}%d to ${n_neurons_new}%d.")
      nNeurons = n_neurons_new
      (new_Weights, new_Biases)
    }
    else (new_Weights, Biases)
  }*/

}
