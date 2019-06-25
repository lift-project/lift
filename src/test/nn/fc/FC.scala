package nn.fc

/**
  * Created by Naums Mogers.
  * This file implements the Lift version of the Multilayer Perceptron forward-propagation.
  */

import ir.ArrayType
import ir.ast._
import lift.arithmetic.SizeVar
import nn._
import opencl.ir._
import opencl.ir.pattern._
import patterns.nn._

/**
  * Case class for storing the layer configuration.
  * Configuration is to be preprocessed and verified by the companion object below.
  * @param liftFProp        an activation function (e.g. Linear, ReLU, etc)
  * @param inputShape       configuration of input data
  * @param outputShape      configuration of output data
  * @param neuronShape      configuration of the layer (number of neurons, channels, etc)
  * @param multsPerThread   the number of input elements (pixels) to process (weigh) sequentially within a thread
  * @param neuronsPerWrg    the number of neurons to process within a single workgroup
  * @param inputTileSize    the size of the square input tiles in one dimension
  * @param localSize        number of work items in respective dimensions of work groups
  * @param globalSize       number of work items in respective dimension of the global space
  */
case class FC(liftFProp: Array[FunDecl],
              inputShape: Shape, outputShape: Shape, neuronShape: Shape,
              multsPerThread: Int, neuronsPerWrg: Int, inputTileSize: Int,
              localSize: Array[Int], globalSize: Array[Int]) extends Layer {
  val configToString: String = nn.fc.configToString(neuronShape.size, multsPerThread, neuronsPerWrg)
  var runtime: Double = 0

  /* Removes padded neurons from the final layer output
   */
  def groupAndUnpad(outputsFlat: Array[Float], datasets: NetDatasets): Unit = {
    datasets.asInstanceOf[FCDatasets].outputs.nonPadded =
      patterns.nn.group(outputsFlat, (outputShape.nInputs, outputShape.sizePadded)).map(
        input => input.slice(0, outputShape.size))
  }
}

/**
  * The companion object that contains the Lift expressions, configuration preprocessing and
  * verification, and helper functions.
  */
object FC {
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
  def Par(activation_f: UserFun, input_shape: Shape, input_tile_size: Int,
          neuron_shape: Shape, tile: Tile): FunDecl = λ(
    ArrayType(ArrayType(Float, input_shape.sizePadded), neuron_shape.sizePadded),
    ArrayType(Float, neuron_shape.sizePadded),
    ArrayType(ArrayType(Float, input_shape.sizePadded), input_shape.nInputs),
    (W, B, X) => {
      Join() o
        MapWrg(0)(λ((group_of_inputs) => {
          TransposeW() o Scatter(ReorderWithStride(neuron_shape.sizePadded / tile.neurons)) o
            MapWrg(1)(λ((w_tile2, b_tile2) => {Join() o
              MapWrg(2)(λ((inputs_wrapped) => {
                MapLcl(0)(λ((input) => { //TODO: Join() o
                  MapLcl(1)(λ((neuronBias, neuron_tiled_activations) => {
                    toGlobal(MapSeq(activation_f)) o ReduceSeq(add, toPrivate(id) $ neuronBias) $ neuron_tiled_activations
                  }))$ Zip(b_tile2, input)
                })) $ inputs_wrapped
              })) o
                /* (n_tiles_in_input, tile.inputsInGroup, tile.neurons) ->
                 * (1, tile.inputsInGroup, tile.neurons, n_tiles_in_input)
                 *
                 * Where n_tiles_in_input = input_len / input_tile_size
                 */
              Split(tile.inputsInGroup) o Map(Transpose()) o Transpose() o
              /* (n_tiles_in_input, tile.inputsInGroup, tile.neurons, input_tile_size, tuple(f, f)) ->
               * (n_tiles_in_input, tile.inputsInGroup, tile.neurons, input_tile_size / tile.seqEls) ->
               * (n_tiles_in_input, tile.inputsInGroup, tile.neurons)
               * Where n_tiles_in_input = input_len / input_tile_size
               */
              MapWrg(2)(λ((input_tile1) => {
                MapLcl(0)(λ((input) => {
                    Join() o
                    // Reduce tile
                    MapLcl(1)(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
                    /* Split by neurons */
                    Split(input_tile_size / tile.seqEls) o Join() o
                    MapLcl(1)(
                      toLocal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(toPrivate(mult))) o
                    Join() o Map(Split(tile.seqEls) o ReorderStride(tile.seqEls)) $  //Coalescing: or dim.in / tile_of_mults_size
                      input
                  })) $ input_tile1
              })) o
              /* (tile.inputs, input_len) ->
               * (n_tiles_in_input, tile.inputs, tile.neurons, input_tile_size[tuple(f, f)])
               * Where n_tiles_in_input = input_len / input_tile_size
               * */
                Transpose() o Map(λ((input) => {
                  Transpose() o
                    // Split each input and each neuron by input_tile_size
                    Map(λ((single_neuron_weights) => {
                      Split(input_tile_size) $ Zip(input, single_neuron_weights)
                    })) $ w_tile2
                })) $ group_of_inputs
            })) $ Zip(Split(tile.neurons) $ W, Split(tile.neurons) $ B)})) o
        Split(tile.inputsInGroup) $ X
    }
  )

  case class InitParameters(override val layerNo: Int,
                            liftFPropGenerator: (UserFun, Shape, Int, Shape, Tile) => Array[FunDecl],
                            activationFun: UserFun,
                            override val inputShape: Shape, neuronShape: Shape,
                            optParams: fc.Experiment.Config.OptimisationalParams) 
    extends Layer.InitParameters(layerNo, inputShape)

  def apply(iP: InitParameters): FC = {
    /**
      * Class factory: verifies that an object can be created,
      * initializes variables, computes workgroup sizes.
      */

    val exceptionMsgPrefix: String = "In the FC layer with the following configuration:\n" +
      configToString(iP.neuronShape.size, iP.optParams.multsPerThread, iP.optParams.neuronsPerWrg)

    val inputTileSize: Int = {
        val iTSNonPadded: Int = Math.min(iP.inputShape.size,
          Math.floor(nn.maxWorkGroupSize.toFloat * iP.optParams.multsPerThread / iP.optParams.neuronsPerWrg).toInt)
        // Make sure iTS is divisible by multsPerThread, but use floor instead of ceil to avoid breaking
        // the maxWrokGroupSize limit
         (iP.optParams.multsPerThread * Math.floor(iTSNonPadded.toFloat / iP.optParams.multsPerThread)).toInt
      }
    if (iP.optParams.multsPerThread > iP.inputShape.size)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"multsPerThread(==${iP.optParams.multsPerThread}%d) cannot be bigger than " +
        f"inputShape.size (${iP.inputShape.size.toFloat}%.02f)")

    // Padding: calculate how many neurons will need to be added
    iP.neuronShape.sizePadded = iP.optParams.neuronsPerWrg * Math.ceil(iP.neuronShape.size.toFloat / iP.optParams.neuronsPerWrg).toInt
    // Padding: calculate how many input bits will need to be added
    iP.inputShape.sizePadded = inputTileSize * Math.ceil(iP.inputShape.size.toFloat / inputTileSize).toInt

    val outputShape: Shape = { Shape(
      nInputs = iP.inputShape.nInputs,
      size = iP.neuronShape.size,
      sizePadded = iP.neuronShape.sizePadded)
    }

    // Check that padding didn't change dataset size too much
    if (iP.inputShape.sizePadded > iP.inputShape.size * 10)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"padding with input bits increased the data set size too much, which might skew the experiment purity: " +
        f"inputShape.sizePadded(==${iP.inputShape.sizePadded}%d) > " +
        f"inputShape.size (${iP.inputShape.size.toFloat}%.02f) * 10")

    // Check neuronsPerWrg. Padding should ensure that this never happens, but for test coverage this check remains
//    if (neuronShape.sizePadded % neuronsPerWrg != 0)
//      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
//        f"neuronShape.sizePadded(${neuronShape.sizePadded}%d) must be divisible by neuronsPerWrg" +
//          f"($neuronsPerWrg%d) to split the neuron dimension evenly among threads.")
    if (iP.neuronShape.size < iP.optParams.neuronsPerWrg)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"neuronShape.size(${iP.neuronShape.size}%d) must be bigger or equal to neuronsPerWrg(${iP.optParams.neuronsPerWrg}%d).")

    val localSize: Array[Int] = Array.fill[Int](3)(0)
    // Layer size 1 = the total size of the workgroup job / the size of the job per thread
    localSize(1) = Math.ceil((inputTileSize * iP.optParams.neuronsPerWrg).toFloat / iP.optParams.multsPerThread).toInt

//    if (localSize(1) > nn.maxWorkGroupSize)
//      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
//        f"local size 1 (==${localSize(1)}%d)(inputLen * neuronsPerWrg / multsPerThread) must be less " +
//          f"or equal to maxWorkGroupSize(${nn.maxWorkGroupSize}%d).\nDecrease neuronsPerWrg or " +
//          f"increase multsPerThread.")

    // Layer size 0 = the number of images in a batch or 1 if a single image cannot be processed by one group
    localSize(0) = Math.min(iP.inputShape.nInputs,
      Math.max(1, Math.floor(nn.maxWorkGroupSize.toFloat / localSize(1)).toInt))

    localSize(2) = 1

    // Check new memory consumption
    if (4.toLong * iP.inputShape.nInputs * iP.inputShape.sizePadded * iP.neuronShape.sizePadded > Integer.MAX_VALUE)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"excessive memory consumption: 4 * inputShape.nInputs(${iP.inputShape.nInputs}%d) * " +
          f"inputShape.sizePadded(${iP.inputShape.sizePadded}%d) * ${iP.neuronShape.sizePadded}%d)(" +
          f"${iP.neuronShape.sizePadded}%d) > Integer.MAX_VALUE(${Integer.MAX_VALUE}%d)")
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

    // Global size 0 = mapping of workgroups across all inputs
    globalSize(0) = localSize(0) * Math.ceil(iP.inputShape.nInputs.toFloat / localSize(0)).toInt
    // Global size 1 = (the number of threads per workgroup) * (the total number of neurons / neurons per workgroup)
    globalSize(1) = localSize(1) * (iP.neuronShape.size.toFloat / iP.optParams.neuronsPerWrg).toInt
    globalSize(2) = localSize(2) * Math.ceil(iP.inputShape.sizePadded / inputTileSize).toInt

    /* Now that all parameters are calculated and verified, build the layer */

    new FC(iP.liftFPropGenerator(/*activation_f*/iP.activationFun, /*input_shape*/iP.inputShape,
      /*input_tile_size*/inputTileSize, /*neuron_shape*/iP.neuronShape, /*tile*/
        Tile(seqEls = iP.optParams.multsPerThread, inputsInGroup = localSize(0),
          neurons = iP.optParams.neuronsPerWrg)),
      iP.inputShape, outputShape, iP.neuronShape, iP.optParams.multsPerThread,
      iP.optParams.neuronsPerWrg, inputTileSize, localSize, globalSize)
  }

/* Ensures that a single input can be evenly split among threads in dimension 0;
 * Ensures that neurons can be evenly split among workgroups in dimension 0.
 */
  def pad(inputs: PaddedArray[Array2D[Float]], inputShape: Shape,
          weights: PaddedArray[Array2D[Float]], biases: PaddedArray[Array[Float]],
          neuronShape: Shape): Unit = {
    /* Pad inputs (inputs and weights) and neurons (weights and biases) */
//    inputs.padded =
//      Array.fill[Array[Float]](inputShape.nInputs)(
//        Array.fill[Float](inputShape.sizePadded)(0))
    inputs.padded = Array.ofDim[Array[Float]](inputShape.nInputs)
    for {i <- 0 until inputShape.nInputs}
      inputs.padded(i) = inputs.nonPadded(i).padTo(inputShape.sizePadded, 0.toFloat)

//    weights.padded =
//      Array.fill[Array[Float]](neuronShape.sizePadded)(
//        Array.fill[Float](inputShape.sizePadded)(0))
//    weights.padded = weights.nonPadded.padTo(
//      neuronShape.sizePadded, Array.fill[Float](inputShape.sizePadded)(0))
    weights.padded = Array.ofDim[Array[Float]](neuronShape.sizePadded)
    for {i <- 0 until neuronShape.size}
      weights.padded(i) = weights.nonPadded(i).padTo(inputShape.sizePadded, 0.toFloat)
    for {i <- neuronShape.size until neuronShape.sizePadded}
      weights.padded(i) = Array.fill[Float](inputShape.sizePadded)(0)

    biases.padded = biases.nonPadded.padTo(neuronShape.sizePadded, 0.toFloat)

    if (inputShape.sizePadded != inputShape.size)
      println(f"Changed inputLen from ${inputShape.size}%d to ${inputShape.sizePadded}%d.")
    if (neuronShape.sizePadded != neuronShape.size)
      println(f"Changed nNeurons from ${neuronShape.size}%d to ${neuronShape.sizePadded}%d.")
  }
}
