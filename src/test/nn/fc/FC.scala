package nn.fc

/**
  * Created by Naums Mogers.
  * This file implements the Lift version of the Multilayer Perceptron forward-propagation.
  */

import ir.ArrayType
import ir.ast.{FunDecl, Join, ReorderWithStride, Scatter, Split, Transpose, TransposeW, UserFun, Zip, λ}
import nn._
import opencl.ir._
import opencl.ir.pattern._

/**
  * Case class for storing the layer configuration.
  * Configuration is to be preprocessed and verified by the companion object below.
  * @param liftFProp
  * @param inputShape
  * @param outputShape
  * @param neuronShape
  * @param multsPerThread
  * @param neuronsPerWrg
  * @param localSize
  * @param globalSize
  */
case class FC(liftFProp: FunDecl,
              inputShape: Shape, outputShape: Shape, neuronShape: Shape,
              multsPerThread: Int, neuronsPerWrg: Int,
              localSize: Array[Int], globalSize: Array[Int]) extends Layer {
  val configToString: String = nn.fc.configToString(neuronShape.size, multsPerThread, neuronsPerWrg)
  var runtime: Double = 0

  /* Removes padded neurons from the final layer output
   */
  def groupAndUnpad(outputsFlat: Array[Float], datasets: NetDatasets): Unit = {
    datasets.asInstanceOf[FCDatasets].outputs.nonPadded =
      nn.group(outputsFlat, (outputShape.nInputs, outputShape.sizePadded)).map(
        input => input.slice(0, outputShape.size))
  }
}

/**
  * The companion object that contains the Lift expressions, configuration preprocessing and
  * verification, and helper functions.
  */
object FC {
  /*val layer_idim = SizeVar("layer_idim")
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
  )*/

  /* Parallel layer */
  def Par(activation_f: UserFun, input_shape: Shape, neuron_shape: Shape, tile: Tile): FunDecl = λ(
    ArrayType(ArrayType(Float, input_shape.sizePadded), neuron_shape.sizePadded),
    ArrayType(Float, neuron_shape.sizePadded),
    ArrayType(ArrayType(Float, input_shape.sizePadded), input_shape.nInputs),
    (W, B, X) => {
      Join() o
        MapWrg(1)(λ((xTile) => {
          Scatter(ReorderWithStride(neuron_shape.sizePadded / tile.neurons)) o Join() o
            MapWrg(0)(λ((wTile, bTile) => {
              MapLcl(1)(λ((xSingle) => {
                MapLcl(0)(λ((neuronBias, neuronPartialSums) => {
                  TransposeW() o
                    MapSeq(toGlobal(MapSeq(activation_f)) o ReduceSeq(add, id(neuronBias)) ) o
                    Transpose() $ neuronPartialSums})) o
                  λ((partialSums) => Zip(bTile, partialSums)) o
                  Split(input_shape.sizePadded / tile.mults) o
                  MapLcl(0)(
                    toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult)) o
                  Split(tile.mults) o Join() o
                  MapSeq(λ((ws_per_neuron) => { ReorderStride(tile.mults) $ //Coalescing: or dim.in / tile_of_mults_size
                    Zip(xSingle, ws_per_neuron)})) $ wTile})) $
                xTile})) $ Zip(Split(tile.neurons) $ W, Split(tile.neurons) $ B)})) o
        Split(tile.inputs) $ X
    }
  )

  def apply(liftFPropGenerator: (UserFun, Shape, Shape, Tile) => FunDecl,
            activationFun: UserFun,
            inputShape: Shape, neuronShape: Shape,
            multsPerThread: Int, neuronsPerWrg: Int): FC = {
    /**
      * Class factory: verifies that an object can be created,
      * initializes variables, computes workgroup sizes.
      */

    val exceptionMsgPrefix: String = "In the FC layer with the following configuration:\n" +
      configToString(neuronShape.size, multsPerThread, neuronsPerWrg)

    // Padding: calculate how many neurons will need to be added
    neuronShape.sizePadded = neuronsPerWrg * Math.ceil(neuronShape.size.toFloat / neuronsPerWrg).toInt
    // Padding: calculate how many input bits will need to be added
    inputShape.sizePadded = multsPerThread * Math.ceil(inputShape.size.toFloat / multsPerThread).toInt

    val outputShape: Shape = { Shape(
      nInputs = inputShape.nInputs,
      size = neuronShape.size,
      sizePadded = neuronShape.sizePadded)
    }

    // Check that padding didn't change dataset size too much
    if (inputShape.sizePadded > inputShape.size * 1.5)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"padding with input bits increased the data set size too much, which might skew the experiment purity: " +
        f"inputShape.sizePadded(==${inputShape.sizePadded}%d) > " +
        f"inputShape.size (${inputShape.size.toFloat}%.02f) * 1.5")

    // Check neuronsPerWrg. Padding should ensure that this never happens, but for test coverage this check remains
//    if (neuronShape.sizePadded % neuronsPerWrg != 0)
//      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
//        f"neuronShape.sizePadded(${neuronShape.sizePadded}%d) must be divisible by neuronsPerWrg" +
//          f"($neuronsPerWrg%d) to split the neuron dimension evenly among threads.")
    if (neuronShape.size < neuronsPerWrg)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"neuronShape.size(${neuronShape.size}%d) must be bigger or equal to neuronsPerWrg($neuronsPerWrg%d).")

    val localSize: Array[Int] = Array.fill[Int](3)(0)
    // Layer size 0 = the total size of the workgroup job / the size of the job per thread
    localSize(0) = Math.ceil((neuronsPerWrg * neuronShape.sizePadded).toFloat / multsPerThread).toInt
    if (localSize(0) > nn.maxWorkGroupSize)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"local size 0 (==${localSize(0)}%d)(==inputLen * neuronsPerWrg) must be less " +
          f"or equal to maxWorkGroupSize(${nn.maxWorkGroupSize}%d).\nDecrease neuronsPerWrg or " +
          f"increase multsPerThread.")

    // Layer size 0 = the minimum of the maximum allowed layer size 1 considering layer size 0 and
    // the number of inputs (the size of the workgroup job)
    localSize(0) = Math.min(Math.floor(nn.maxWorkGroupSize.toFloat / localSize(0)).toInt, inputShape.nInputs)

    // Layer size 1 = the batch size
    localSize(1) = Math.min(Math.floor(nn.maxWorkGroupSize.toFloat / localSize(0)).toInt, inputShape.nInputs)

    localSize(2) = 1

    // Check new memory consumption
    if (4.toLong * inputShape.nInputs * inputShape.sizePadded * neuronShape.sizePadded > Integer.MAX_VALUE)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"excessive memory consumption: 4 * inputShape.nInputs(${inputShape.nInputs}%d) * " +
          f"inputShape.sizePadded(${inputShape.sizePadded}%d) * ${neuronShape.sizePadded}%d)(" +
          f"${neuronShape.sizePadded}%d) > Integer.MAX_VALUE(${Integer.MAX_VALUE}%d)")
    {
      val groupSize: Int = localSize(0) * localSize(1) * localSize(2)
      if (groupSize > nn.maxWorkGroupSize)
        throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          f"group size (==$groupSize%d) must be less or equal to maxWorkGroupSize (${nn.maxWorkGroupSize}%d)")
    }

    val globalSize: Array[Int] = Array.fill[Int](3)(0)
    // Check multsPerThread. Padding should ensure that this never happens, but for test coverage this check remains
//    if (inputLenPadded(layerNo) % multsPerThread(layerNo) != 0)
//      throw new java.lang.IllegalArgumentException(
//        f"Layer $layerNo%d input length (${inputLenPadded(layerNo)}%d) must be divisible " +
//          f"by multsPerThread (${multsPerThread(layerNo)}%d).")

    // Global size 0 = (the number of threads per workgroup) * (the total number of neurons / neurons per workgroup)
    globalSize(0) = localSize(0) * (neuronShape.sizePadded.toFloat / neuronsPerWrg).toInt
    // Global size 1 = mapping of workgroups across all inputs
    globalSize(1) = localSize(1) * Math.ceil(inputShape.nInputs.toFloat / localSize(1)).toInt
    globalSize(2) = 1

    /* Now that all parameters are calculated and verified, build the layer */

    new FC(liftFPropGenerator(activationFun, inputShape, neuronShape,
        Tile(mults = multsPerThread,inputs = localSize(1), neurons = neuronsPerWrg)),
      inputShape, outputShape, neuronShape, multsPerThread, neuronsPerWrg, localSize, globalSize)
  }

/* Ensures that a single input can be evenly split among threads in dimension 0;
 * Ensures that neurons can be evenly split among workgroups in dimension 0.
 */
  def pad(inputs: PaddedArray[Array2D[Float]], inputShape: Shape,
          weights: PaddedArray[Array2D[Float]], biases: PaddedArray[Array[Float]],
          neuronShape: Shape): Unit = {
    /* Pad inputs (inputs and weights) and neurons (weights and biases) */
    inputs.padded =
      Array.fill[Array[Float]](inputShape.nInputs)(
        Array.fill[Float](inputShape.sizePadded)(0))
    for {i <- 0 until inputShape.nInputs}
      inputs.padded(i) = inputs.nonPadded(i).padTo(inputShape.sizePadded, 0.toFloat)

    weights.padded =
      Array.fill[Array[Float]](neuronShape.sizePadded)(
        Array.fill[Float](inputShape.sizePadded)(0))
    weights.padded = weights.nonPadded.padTo(
      neuronShape.sizePadded, Array.fill[Float](inputShape.sizePadded)(0))

    biases.padded = biases.nonPadded.padTo(neuronShape.sizePadded, 0.toFloat)

    if (inputShape.sizePadded != inputShape.size)
      println(f"Changed inputLen from ${inputShape.size}%d to ${inputShape.sizePadded}%d.")
    if (neuronShape.sizePadded != neuronShape.size)
      println(f"Changed nNeurons from ${neuronShape.size}%d to ${neuronShape.sizePadded}%d.")
  }
}
