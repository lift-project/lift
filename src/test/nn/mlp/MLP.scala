package nn.mlp

/**
  * Created by Naums Mogers.
  * This file implements the Lift version of the Multilayer Perceptron forward-propagation.
  */

import ir.ArrayType
import ir.ast.{FunDecl, Join, ReorderWithStride, Scatter, Split, Transpose, TransposeW, UserFun, Zip, λ}
import lift.arithmetic.SizeVar
import opencl.ir._
import opencl.ir.pattern._
import nn.{Array2D, Array3D, Shape}

object MLP {
  val layer_idim = SizeVar("layer_idim")
  val layer_odim = SizeVar("layer_odim")
  val layer_ninputs = SizeVar("layer_ninputs")
  val idim = SizeVar("idim")
  val hdim1 = SizeVar("hdim1")
  val hdim2 = SizeVar("hdim2")
  val odim = SizeVar("odim")


  /* Sequential layer */
  def Seq(activation_f: UserFun): FunDecl = λ(
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
  def Par(activation_f: UserFun,
          tile: Tile,
          dim: Shape, nInputs: Int): FunDecl = λ(
    ArrayType(ArrayType(Float, dim.in), dim.out),
    ArrayType(Float, dim.out),
    ArrayType(ArrayType(Float, dim.in), nInputs),
    (W, B, X) => {
      Join() o
        MapWrg(1)(λ((xTile) => {
          Scatter(ReorderWithStride(dim.out / tile.neurons)) o Join() o
            MapWrg(0)(λ((wTile, bTile) => {
              MapLcl(1)(λ((xSingle) => {
                MapLcl(0)(λ((neuronBias, neuronPartialSums) => {
                  TransposeW() o
                    MapSeq(toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(neuronBias)) ) o
                    Transpose() $ neuronPartialSums})) o
                  λ((partialSums) => Zip(bTile, partialSums)) o
                  Split(dim.in / tile.mults) o
                  MapLcl(0)(
                    toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
                  Split(tile.mults) o Join() o
                  MapSeq(λ((ws_per_neuron) => { ReorderStride(tile.mults) $ //Coalescing: or dim.in / tile_of_mults_size
                    Zip(xSingle, ws_per_neuron)})) $ wTile})) $
                xTile})) $ Zip(Split(tile.neurons) $ W, Split(tile.neurons) $ B)})) o
        Split(tile.inputs) $ X
    }
  )
}

class MLP(/* MLP architectural parameters */
          val liftMLP: (UserFun, Tile, Shape, Int) => FunDecl,
          val activationFun/*per layer*/: Array[UserFun],
          /* MLP parallelisation parameters */
          val multsPerThread/*per layer*/: Array[Int],
          val neuronsPerWrg/*per layer*/: Array[Int],
          /* MLP application parameters */
          val inputsNonPadded: Array2D[Float], val weightsNonPadded: Array3D[Float],
          val biasesNonPadded: Array2D[Float], val targets: Array2D[Float],
          val liftFunName: String, val testDescription: String, val pathToResults: String) {
  /**
    * Initializes variables, computes workgroup sizes.
    */

  var weightsPadded: Array3D[Float] = _
  var biasesPadded: Array2D[Float] = _
  val nLayers: Int = biasesNonPadded.length
  var layerInputsPadded: Array3D[Float] = _
  var nInputsNonPadded: Int = inputsNonPadded.length
  var nLayerInputsPadded: Array[Int] = Array.fill[Int](nLayers)(0)
  val inputLenNonPadded: Array[Int] = Array.fill[Int](nLayers)(0)
  val inputLenPadded: Array[Int] = Array.fill[Int](nLayers)(0)
  val nNeuronsNonPadded: Array[Int] = Array.fill[Int](nLayers)(0)
  val nNeuronsPadded: Array[Int] = Array.fill[Int](nLayers)(0)
  val runTimes: Array[Double] = Array.fill[Double](nLayers)(0)
  val localSize0: Array[Int] = Array.fill[Int](nLayers)(0)
  val localSize1: Array[Int] = Array.fill[Int](nLayers)(0)
  val globalSize0: Array[Int] = Array.fill[Int](nLayers)(0)
  val globalSize1: Array[Int] = Array.fill[Int](nLayers)(0)
  var outputsPadded: Array3D[Float] = _
  var outputsNonPadded: Array2D[Float] = _

  def getLayerInputs(layerNo: Int): Array2D[Float] =
    if (layerNo == 0) inputsNonPadded else outputsPadded(layerNo - 1)

  for (layerNo <- 0 until nLayers) {
    inputLenNonPadded(layerNo) = weightsNonPadded(layerNo).head.length
    nNeuronsNonPadded(layerNo) = biasesNonPadded(layerNo).length

    // Padding: calculate how many neurons will need to be added
    nNeuronsPadded(layerNo) = getNeuronPaddingSize(neuronsPerWrg(layerNo), nNeuronsNonPadded(layerNo))
    // Padding: calculate how many input bits will need to be added
    inputLenPadded(layerNo) = getInputBitPaddingSize(multsPerThread(layerNo), inputLenNonPadded(layerNo))

    // Check that padding didn't change dataset size too much
    if (inputLenPadded(layerNo) > inputLenNonPadded(layerNo).toFloat * 1.5)
      throw new java.lang.IllegalArgumentException(
        f"Padding with input bits increased the data set size too much, which might skew the experiment purity: " +
          f"inputLenPadded(0)(==${inputLenPadded(0)}%d) > inputLen(0).toFloat (==${inputLenNonPadded(0).toFloat}%.02f) * 1.5")

    // Check neuronsPerWrg. Padding should ensure that this never happens, but for test coverage this check remains
    if (nNeuronsPadded(layerNo) % neuronsPerWrg(layerNo) != 0)
      throw new java.lang.IllegalArgumentException(
        f"nNeurons(layerNo)(==${nNeuronsNonPadded(layerNo)}%d) must be divisible by neuronsPerWrg" +
          f"(==${neuronsPerWrg(layerNo)}%d) to split the neuron dimension evenly among threads.")
    if (nNeuronsNonPadded(layerNo) < neuronsPerWrg(layerNo))
      throw new java.lang.IllegalArgumentException(f"Layer $layerNo%d: nNeurons(${nNeuronsNonPadded(layerNo)}%d) " +
        f"must be bigger or equal to neuronsPerWrg(${neuronsPerWrg(layerNo)}%d).")

    /* Compute local sizes */
    // Layer size 0 = the total size of the workgroup job / the size of the job per thread
    localSize0(layerNo) =
      Math.ceil((neuronsPerWrg(layerNo) * inputLenPadded(layerNo)).toFloat / multsPerThread(layerNo)).toInt
    if (localSize0(layerNo) > nn.maxWorkGroupSize)
      throw new java.lang.IllegalArgumentException(
      f"Layer $layerNo%d: local size 0 (==${localSize0(layerNo)}%d)(==inputLen * neuronsPerWrg) must be less " +
        f"or equal to maxWorkGroupSize(${nn.maxWorkGroupSize}%d).\nDecrease neuronsPerWrg or " +
        f"increase multsPerThread.")

    // Layer size 1 = the minimum of the maximum allowed layer size 1 considering layer size 0 and
    // the number of inputs (the size of the workgroup job)
    localSize1(layerNo) = Math.floor(nn.maxWorkGroupSize.toFloat / localSize0(layerNo)).toInt
    if (localSize1(layerNo) > nInputsNonPadded)
      localSize1(layerNo) = nInputsNonPadded

    // Padding: calculate how many inputs will need to be added
    nLayerInputsPadded(layerNo) = getInputPaddingSize(localSize1(layerNo), nInputsNonPadded)
    // (nInputsNonPadded is always the same)

    // Check new memory consumption
    if (4.toLong * nLayerInputsPadded(layerNo) * inputLenPadded(layerNo) * nNeuronsPadded(layerNo) > Integer.MAX_VALUE)
      throw new java.lang.IllegalArgumentException(
        f"Excessive memory consumption: 4 * nInputsPadded($layerNo%d)(==${nLayerInputsPadded(layerNo)}%d) * " +
          f"inputLenPadded($layerNo%d)(==${inputLenPadded(layerNo)}%d) * nNeurons($layerNo%d)(==" +
          f"${nNeuronsPadded(layerNo)}%d) > Integer.MAX_VALUE(${Integer.MAX_VALUE}%d)")
    // Check that padding didn't change dataset size too much
    if (nLayerInputsPadded(layerNo) > nInputsNonPadded.toFloat * 1.5)
      throw new java.lang.IllegalArgumentException(
        f"Padding with inputs increased the data set size too much, which might skew the experiment purity: " +
          f"nInputsPadded($layerNo%d)(==${nLayerInputsPadded(layerNo)}%d) > nInputsNonPadded.toFloat " +
          f"(==${nInputsNonPadded.toFloat}%.02f) * 1.5")

    /* Compute global sizes */
    // Check multsPerThread. Padding should ensure that this never happens, but for test coverage this check remains
    if (inputLenPadded(layerNo) % multsPerThread(layerNo) != 0)
      throw new java.lang.IllegalArgumentException(
        f"Layer $layerNo%d input length (${inputLenPadded(layerNo)}%d) must be divisible " +
          f"by multsPerThread (${multsPerThread(layerNo)}%d).")

    // Global size 0 = (the total number of neurons / neurons per workgroup) * the number of threads per workgroup
    globalSize0(layerNo) = (nNeuronsPadded(layerNo).toFloat / neuronsPerWrg(layerNo)).toInt * localSize0(layerNo)
    // Global size 1 = mapping of workgroups across all inputs
    globalSize1(layerNo) = localSize1(layerNo) * Math.ceil(nLayerInputsPadded(layerNo).toFloat /
      localSize1(layerNo)).toInt
  }


  def getInputPaddingSize(localSize1: Int, nInputsNonPadded: Int): Int =
    (localSize1 * Math.ceil(nInputsNonPadded.toFloat / localSize1)).toInt

  def getInputBitPaddingSize(multsPerThread: Int, inputLen: Int): Int =
    multsPerThread * Math.ceil(inputLen.toFloat / multsPerThread).toInt

  def getNeuronPaddingSize(neuronsPerWrg: Int, nNeurons: Int): Int =
    neuronsPerWrg * Math.ceil(nNeurons.toFloat / neuronsPerWrg).toInt


  /* Ensures that inputs can be evenly split among workgroups in dimension 1;
   * Ensures that a single input can be evenly split among threads in dimension 0;
   * Ensures that neurons can be evenly split among workgroups in dimension 0.
   */
  def padInputsAndNeurons(layerNo: Int): Unit = {
    /* Pad inputs (inputs and weights) and neurons (weights and biases) */
    def getNewPaddedInputs = Array.fill[Array[Float]](nLayerInputsPadded(layerNo))(
      Array.fill[Float](inputLenPadded(layerNo))(0))
    layerInputsPadded = if (layerInputsPadded == null) Array(getNewPaddedInputs) else
      layerInputsPadded :+ getNewPaddedInputs

    def getNewPaddedWeights = Array.fill[Array[Float]](nNeuronsPadded(layerNo))(
      Array.fill[Float](inputLenPadded(layerNo))(0))
    weightsPadded = if (weightsPadded == null) Array(getNewPaddedWeights) else weightsPadded :+ getNewPaddedWeights

    def getNewPaddedBiases = Array.fill[Float](nNeuronsPadded(layerNo))(0)
    biasesPadded = if (biasesPadded == null) Array(getNewPaddedBiases) else biasesPadded :+ getNewPaddedBiases

    // Copy the non-padded data. The rest will remain zeros.
    val layerInputs = getLayerInputs(layerNo)
    for {i <- 0 until nInputsNonPadded; j <- 0 until inputLenNonPadded(layerNo)}
      layerInputsPadded(layerNo)(i)(j) = layerInputs(i)(j)

    for {i <- 0 until nNeuronsNonPadded(layerNo); j <- 0 until inputLenNonPadded(layerNo)}
      weightsPadded(layerNo)(i)(j) = weightsNonPadded(layerNo)(i)(j)

    for {i <- 0 until nNeuronsNonPadded(layerNo)}
      biasesPadded(layerNo)(i) = biasesNonPadded(layerNo)(i)

    if (nLayerInputsPadded(layerNo) != nInputsNonPadded)
      println(f"Changed nInputs(layer $layerNo%d) from $nInputsNonPadded%d to ${nLayerInputsPadded(layerNo)}%d.")
    if (inputLenPadded(layerNo) != inputLenNonPadded(layerNo))
      println(f"Changed inputLen(layer $layerNo%d) from ${inputLenNonPadded(layerNo)}%d to ${inputLenPadded(layerNo)}%d.")
    if (nNeuronsNonPadded(layerNo) != nNeuronsPadded(layerNo))
      println(f"Changed nNeurons(layer $layerNo%d) from ${nNeuronsNonPadded(layerNo)}%d to ${nNeuronsPadded(layerNo)}%d.")
  }


  /* Removes padded neurons and (input) samples from the final layer output
   */
  def unPadOutputs(): Unit = {
    outputsNonPadded = Array.fill[Array[Float]](nInputsNonPadded)(Array.fill[Float](nNeuronsNonPadded.last)(0))
    for {i <- 0 until nInputsNonPadded; j <- 0 until nNeuronsNonPadded.last}
      outputsNonPadded(i)(j) = outputsPadded.last(i)(j)
  }
}
