package nn.cnn

/**
  * Created by nm on 09/01/17.
  */

import ir.ArrayType
import ir.ast.{FunDecl, Get, Join, Map, Slide2D, Split, Transpose, UserFun, Zip, λ}
import lift.arithmetic.SizeVar
import opencl.ir._
import opencl.ir.pattern._
import nn._

object CNN {
  val kernel_xdim = SizeVar("kernel_xdim")
  val kernel_ydim = SizeVar("kernel_ydim")
  val input_xdim = SizeVar("input_xdim")
  val input_ydim = SizeVar("input_ydim")
  val layer_idim = SizeVar("layer_idim")
  val layer_odim = SizeVar("layer_odim")
  val input_channels = SizeVar("input_channels")
  val output_channels = SizeVar("output_channels")
  val n_inputs = SizeVar("n_inputs")
  val n_batches = SizeVar("n_batches")

  /* Sequential layer */
  def Seq(kernel_h: Int, kernel_w: Int, activation_f: UserFun): FunDecl = λ(
    ArrayType(ArrayType(ArrayType(ArrayType(Float, output_channels), input_channels), kernel_w), kernel_h),
    ArrayType(Float, output_channels),
    ArrayType(ArrayType(ArrayType(ArrayType(Float, input_channels), input_xdim), input_ydim), n_inputs),
    (K, B, X) => {
      MapLcl(0)(λ((single_input) => {
        MapSeq(λ((pass_strip) => {
          MapSeq(λ((pass_window) => { Join() o
            MapSeq(λ((weighted_window_per_out_ch, b_per_out_ch) => { // Reduce weighted pass window separately for each output
              MapSeq(toGlobal(activation_f)) o ReduceSeq(add, id(b_per_out_ch)) $ weighted_window_per_out_ch
            })) o λ((weighted_window_across_out_chs) => Zip(weighted_window_across_out_chs, B)) o Transpose() o
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
        })) o Slide2D(kernel_h, 1, kernel_w, 1) $ single_input
      })) $ X
    }
  )

  /* Parallel layer */
  def Par(activation_f: UserFun, kernel_shape: Shape, output_shape: Shape, n_output_channels: Int): FunDecl = λ(
    ArrayType(ArrayType(ArrayType(ArrayType(Float,
      n_output_channels), input_channels), kernel_shape.w), kernel_shape.h),
    ArrayType(Float, n_output_channels),
    ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float,
      input_channels), input_xdim), input_ydim), n_inputs), n_batches),
    (K, B, X) => {
      MapWrg(1)(λ((input_batch) => {
        MapWrg(0)(λ((input_single) => {
          MapWrg(2)(λ((input_single_wrapped) => {
            MapLcl(1)(λ((pass_row) => {
              MapLcl(2)(λ((pass_window) => {
                Join() o
                  MapSeq(λ((weighted_window_per_out_ch, b_per_out_ch) => {
                    // Reduce weighted pass window separately for each output
                    MapSeq(toGlobal(activation_f)) o ReduceSeq(add, id(b_per_out_ch)) $ weighted_window_per_out_ch
                  })) o λ((weighted_window_across_out_chs) => Zip(weighted_window_across_out_chs, B)) o Transpose() o
                  MapLcl(1)(λ((window_row, kernel_row) => {
                    Join() o
                      MapSeq(λ((weighted_row_per_out_ch) => {
                        // Reduce weighted pass window rows separately for each output
                        MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) o Join() $ weighted_row_per_out_ch
                      })) o Transpose() o Split(n_output_channels) o // multiplied by width (==160)
                      MapLcl(0)(λ((el_in_chs) => {
                        /* Map below returns (n_out_chs, n_els) */
                        //MapLcl(0)(λ((k_el_out_ch) => {
                        MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) o
                          MapSeq(λ((el_in_ch) =>
                            mult(/*x_el_in_chs*/Get(el_in_ch, 0), /*k_el_in_ch*/Get(el_in_ch, 1)))) $ el_in_chs
                        //})) o
                      })) o Join() o
                    // Transpose: (n_in_chs, n_out_chs) -> (n_out_chs, n_in_chs)
                    // (x_el_in_chs0, (k_el_out_ch00, k_el_out_ch01)) ->
                    // ((x_el_in_chs0, k_el_out_ch00), (x_el_in_chs0, k_el_out_ch01))
                    Map(λ((/*x_el_in_chs, k_el_in_chs*/el_in_chs) =>
                      Map(λ((k_el_out_ch) => Zip(/*x_el_in_chs*/Get(el_in_chs, 0), k_el_out_ch))) o Transpose() $
                        /*k_el_in_chs*/Get(el_in_chs, 1)
                    )) $ Zip(window_row, kernel_row)
                  })) $ Zip(pass_window, K)
              })) $ pass_row
            })) o // (pass_strip_n, pass_window_n, pass_row_n) -> (pass_strip_n * pass_row_n, pass_window_n)
              Join() o Map(λ((pass_strip) => {
                Transpose() o
                  Map(λ((pass_window) => {
                    Zip(pass_window, K)
                  })) $ pass_strip
              })) $ input_single_wrapped
          })) o Split(output_shape.h) $ input_single // Wrap input in a one-element array
        })) $ input_batch
      })) o MapSeq(MapSeq(Slide2D(kernel_shape.h, 1, kernel_shape.w, 1))) $ X
    }
  )
}

class CNN(/* CNN architectural parameters */
          val liftCNN: (UserFun, Shape, Shape, Int) => FunDecl,
          val activationFun/*per layer*/: Array[UserFun],
          /* CNN application parameters*/
          val nKernels: Array[Int],
          val inputsNonPadded: Array5D[Float], val kernelWeightsNonPadded: Array5D[Float],
          val kernelBiasesNonPadded: Array2D[Float], val targets: Array2D[Float],
          val liftFunName: String, val testDescription: String, val pathToResults: String) {
  /**
    * Initializes variables, computes workgroup sizes.
    */

  var kernelWeightsPadded: Array5D[Float] = _
  var kernelBiasesPadded: Array2D[Float] = _
  var layerInputsPadded: Array6D[Float] = _
  var layerOutputsPadded: Array6D[Float] = _
  var outputsNonPadded: Array5D[Float] = _

  val nLayers: Int = kernelBiasesNonPadded.length
  val nBatches: Int = inputsNonPadded.length
  var nInputsNonPadded: Int = inputsNonPadded.head.length
  var nLayerInputsPadded: Array[Int] = Array.fill[Int](nLayers)(0)
  // TODO
  //val nNeuronsNonPadded: Array[Int] = Array.fill[Int](nLayers)(0)
  //val nNeuronsPadded: Array[Int] = Array.fill[Int](nLayers)(0)
  //val inputLenNonPadded: Array[Int] = Array.fill[Int](nLayers)(0)
  //val inputLenPadded: Array[Int] = Array.fill[Int](nLayers)(0)

  val inputShapeNonPadded: Shape = Shape(w=inputsNonPadded.head.head.length,
    h=inputsNonPadded.head.head.head.length)
  val layerInputShapePadded: Array[Shape] = Array.fill[Shape](nLayers)(Shape())
  val layerOutputShapePadded: Array[Shape] = Array.fill[Shape](nLayers)(Shape())
  val kernelShapeNonPadded: Array[Shape] = Array.fill[Shape](nLayers)(Shape())
  val kernelShapePadded: Array[Shape] = Array.fill[Shape](nLayers)(Shape())

  val runTimes: Array[Double] = Array.fill[Double](nLayers)(0)
  val localSize0: Array[Int] = Array.fill[Int](nLayers)(0)
  val localSize1: Array[Int] = Array.fill[Int](nLayers)(0)
  val localSize2: Array[Int] = Array.fill[Int](nLayers)(0)
  val globalSize0: Array[Int] = Array.fill[Int](nLayers)(0)
  val globalSize1: Array[Int] = Array.fill[Int](nLayers)(0)
  val globalSize2: Array[Int] = Array.fill[Int](nLayers)(0)

  def getLayerInputs(layerNo: Int): Array5D[Float] =
    if (layerNo == 0) inputsNonPadded else layerOutputsPadded(layerNo - 1)

  for (layerNo <- 0 until nLayers) {
    // TODO: padding

    kernelWeightsPadded = if (kernelWeightsPadded == null) Array(kernelWeightsNonPadded(layerNo)) else
      kernelWeightsPadded :+ kernelWeightsNonPadded(layerNo)
    kernelBiasesPadded = if (kernelBiasesPadded == null) Array(kernelBiasesNonPadded(layerNo)) else
      kernelBiasesPadded :+ kernelBiasesNonPadded(layerNo)
    kernelShapeNonPadded(layerNo) = Shape(w=kernelWeightsNonPadded(layerNo).head.length,
      h=kernelWeightsNonPadded(layerNo).length)
    kernelShapePadded(layerNo) = kernelShapeNonPadded(layerNo)

    /*inputLenNonPadded(layerNo) = weightsNonPadded(layerNo).head.length
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
      localSize1(layerNo)).toInt*/
    localSize0(layerNo) = 1
    localSize1(layerNo) = 1
    localSize2(layerNo) = 1

    globalSize0(layerNo) = 1
    globalSize1(layerNo) = 1
    globalSize2(layerNo) = 1
  }
}
