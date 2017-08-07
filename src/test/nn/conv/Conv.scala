package nn.conv

/**
  * Created by nm on 09/01/17.
  */

import ir.ast._
import ir.{ArrayType, TupleType}
import nn._
import opencl.ir._
import opencl.ir.pattern._

/**
  * Case class for storing the layer configuration.
  * Configuration is to be preprocessed and verified by the companion object below.
  * @param liftFProp
  * @param inputShape
  * @param outputShape* @param inputTiling
  * @param kernelSliding
  * @param elsPerThread
  * @param kernelsPerGroup
  * @param localSize
  * @param globalSize
  */
case class Conv(liftFProp: FunDecl,
                inputShape: Shape, outputShape: Shape,
                inputTiling: SlidingWindowConfig, kernelSliding: SlidingWindowConfig,
                elsPerThread: Int, kernelsPerGroup: Int,
                localSize: Array[Int], globalSize: Array[Int]) extends Layer {
  val configToString: String =
    nn.conv.configToString(elsPerThread, outputShape.nChannels,
      kernelsPerGroup, kernelSliding.size, kernelSliding.stride)
  var runtime: Double = 0

  def groupAndUnpad(outputsFlat: Array[Float], datasets: NetDatasets): Unit = {
      datasets.outputs = nn.group(outputsFlat, (outputShape.nBatches, outputShape.nInputs,
      outputShape.sizePadded, outputShape.sizePadded, outputShape.nChannels)).map(
      batch => batch.map(
        input => input.map(
          row => row.slice(0, outputShape.size)
        ).slice(0, outputShape.size)
      ))
  }
}

/**
  * The companion object that contains the Lift expressions, configuration preprocessing and
  * verification, and helper functions.
  */
object Conv {
//  val kernel_xdim_SV = SizeVar("kernel_xdim_SV")
//  val kernel_ydim_SV = SizeVar("kernel_ydim_SV")
//  val input_xdim_SV = SizeVar("input_xdim_SV")
//  val input_ydim_SV = SizeVar("input_ydim_SV")
//  val layer_idim_SV = SizeVar("layer_idim_SV")
//  val layer_odim_SV = SizeVar("layer_odim_SV")
//  val in_channels_SV = SizeVar("in_channels_SV")
//  val out_channels_SV = SizeVar("out_channels_SV")
//  val n_inputs_SV = SizeVar("n_inputs_SV")
//  val n_batches_SV = SizeVar("n_batches_SV")

  /* Sequential layer */
  /*def Seq(kernel_h: Int, kernel_w: Int, activation_f: UserFun): FunDecl = λ(
    ArrayType(ArrayType(ArrayType(ArrayType(Float, out_channels_SV), in_channels_SV), kernel_w), kernel_h),
    ArrayType(Float, out_channels_SV),
    ArrayType(ArrayType(ArrayType(ArrayType(Float, in_channels_SV), input_xdim_SV), input_ydim_SV), n_inputs_SV),
    (K, B, X) => {
      MapSeq(λ((single_input) => {
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
  )*/


  /* Parallel layer */
  def Par(activation_f: UserFun, input_shape: Shape, input_tiling: SlidingWindowConfig, n_kernels: Int, 
          kernel_sliding: SlidingWindowConfig,
          kernels_per_group: Int, els_per_thread: Int): FunDecl = {
    def Layer: FunDecl = λ(
      AT(AT(AT(AT(Float, n_kernels), input_shape.nChannels), kernel_sliding.size), kernel_sliding.size),
      AT(Float, n_kernels),
      AT(AT(AT(AT(AT(Float, input_shape.nChannels), input_shape.sizePadded), input_shape.sizePadded), 
        input_shape.nInputs), input_shape.nBatches),
      (K, B, X) => {
        MapWrg(0)(λ((inputs_batch) => {
          /*  (nImages, input_tiling.n, input_tiling.n, n_k_passes, n_k_windows, nKernels) ->
              (nImages, input_tiling.n, n_k_passes, input_tiling.n, n_k_windows, nKernels) ->
              (nImages, input_tiling.n, n_k_passes, input_tiling.n * n_k_windows, nKernels) ->
              (nImages, input_tiling.n * n_k_passes, input_tiling.n * n_k_windows, nKernels) ->
          *   (nImages, n_passes, n_windows, nKernels) */
          Map(/*input*/Join() o Map(/*tile_row*/Map(Join()) o TransposeW())) o
          Split(input_tiling.n) o Split(input_tiling.n) o
          /*  (nImages * input_tiling.n * input_tiling.n, nKernels, n_k_passes, n_k_windows) ->
           *  (nImages * input_tiling.n * input_tiling.n, n_k_passes, n_k_windows, nKernels) */
          λ(AT(AT(AT(AT(Float, kernel_sliding.n), kernel_sliding.n),
            n_kernels), input_shape.nInputs * input_tiling.n * input_tiling.n),
            (tiled_outputs) => Map(Map(TransposeW()) o TransposeW()) $ tiled_outputs) o
          MapWrg(1)(λ((input_tile) => {
            /* (nKernels / kernels_per_group, kernels_per_group, n_k_passes, n_k_windows) ->
            *  (nKernels, n_k_passes, n_k_windows) */
            Join() o MapWrg(2)(λ(TupleType(
              AT(AT(AT(AT(Float, input_shape.nChannels), kernel_sliding.size), kernel_sliding.size), kernels_per_group),
              AT(Float, kernels_per_group)),
              (kernels_tile) => {
                /* (kernels_per_group, n_k_passes*n_k_windows) ->
                 * (kernels_per_group, n_k_passes, n_k_windows) */
                Map(Split(kernel_sliding.n)) o
                /* (n_passes*n_windows, kernels_per_group) -> (kernels_per_group, n_k_passes*n_k_windows) */
                TransposeW() o
                MapLcl(0)(λ((pass_window) => {
                  λ(AT(AT(Float, kernel_sliding.size), kernels_per_group), (partially_reduced_window) =>
                    ReduceWindowAndAddBias()(partially_reduced_window, /* biases */Get(kernels_tile, 1))) o
                  /* (kernel_sliding.size, kernels_per_group) -> (kernels_per_group, kernel_sliding.size) */
                  TransposeW() o
                  MapLcl(2)(λ((window_row, kernels_row) => {
                    ReduceRow() o
                      // (kernels_per_group, kernel_sliding.size / els_per_thread)
                      Map(Join(/*tiles of elements*/)/* o
                        MapSeq(/* Dissolve one-element output of Reduce */Join())*/) o
                      Split(kernel_sliding.size / els_per_thread) o
                      MapLcl(1)(ReduceAndWeighInputChannels()) o
/* (kernels_per_group, kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels) ->
 * (kernels_per_group * kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels)*/
                      Join() o
/* (kernels_per_group, kernel_sliding.size, input_shape.nChannels) ->
 * (kernels_per_group, kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels) */
                      Map(/* for each kernel in the tile */
                        λ((kernel_row) => Split(els_per_thread) $ Zip(window_row, kernel_row))) o
                      /* (kernel_sliding.size, input_shape.nChannels, kernels_per_group) ->
                       * (kernels_per_group, kernel_sliding.size, input_shape.nChannels) */
                      Transpose() o Map(Transpose()) $ kernels_row
                  })) $ Zip(pass_window, RestoreKernelShape() $ /* weights */ Get(kernels_tile, 0))
                })) o toLocal(MapLcl(0)(λ((pass_window) =>
                MapLcl(2)(λ((window_row) => {
                  MapSeq(MapSeq(id)) $ window_row
                })) $ pass_window))) o
                  /* (n_passes, n_windows, n_rows) -> (n_passes*n_windows, n_rows) */
                  Join() $ input_tile
              })) $ ReshapeAndTileKernels()(K, B)
          })) $ inputs_batch
        })) o SlideX() $ X
      }
    )

    /* Produces a tiled slided version of X.
     * Returns:
     * AT(AT(AT(AT(AT(AT(AT(Float, input_channels), kernel_shape.s), kernel_shape.s), n_kernel_pass_windows_in_tile),
     * n_kernel_pass_strips_in_tile), n_tile_pass_windows * n_tile_pass_strips * input_shape.nInputs), input_shape.nBatches) */
    def SlideX(): FunDecl =
      λ(AT(AT(AT(AT(AT(Float, input_shape.nChannels), input_shape.sizePadded), input_shape.sizePadded), 
        input_shape.nInputs), input_shape.nBatches), (X) =>
        Map(Join() o Map(Join() o 
          TiledSlidedND(2)(kernel_sliding.size, kernel_sliding.stride, input_tiling.stride))) $ X)

    /* Reshapes kernels -- makes the output channels the outermost dimension -- and splits them into tiles.
     * Returns:
     * AT(TupleType(
     *   /* weights */ AT(AT(AT(AT(Float, input_channels), kernel_sliding.size), kernel_sliding.size), kernels_per_group),
     *   /* biases */ AT(Float, kernels_per_group)), n_kernel_tiles) */
    def ReshapeAndTileKernels(): FunDecl =
      λ(AT(AT(AT(AT(Float, n_kernels), input_shape.nChannels), kernel_sliding.size), kernel_sliding.size),
        AT(Float, n_kernels), (kernels, biases) =>
        Zip(
          Split(kernels_per_group) o
          /* (n_rows, n_columns, n_in_chs, n_out_chs) -> (n_out_chs, n_rows, n_columns, n_in_chs) */
          Transpose() o Map(Transpose() o Map(Transpose())) $ kernels,
          Split(kernels_per_group) $ biases))

    /* Reshapes the kernel back to the original shape, where output channels are the lowest dimension
     * of the tensor.
     * TODO: get rid of this - there is no advantage of having this
     * Returns:
     * AT(AT(AT(AT(Float, kernels_per_group), input_shape.nChannels), kernel_sliding.size), kernel_sliding.size) */
    def RestoreKernelShape(): FunDecl =
      λ(AT(AT(AT(AT(Float, input_shape.nChannels), kernel_sliding.size), kernel_sliding.size), kernels_per_group),
        (kernels_tile) =>
        Map(Map(Transpose()) o Transpose()) o Transpose() $ kernels_tile)

    /* Computes a weighted sum of all input channels of a batch of elements for one output channel.
     * Returns:
     * AT(Float, 1) */
    def ReduceAndWeighInputChannels(): FunDecl =
      λ(AT(  TupleType(AT(Float, input_shape.nChannels), AT(Float, input_shape.nChannels)),   els_per_thread),
        (tile_of_els) => {
        /* Compute a sum of the whole batch */
        MapSeq(toGlobal(id)) o ReduceSeq(add, toPrivate(id) $ 0.0f) o Join() o
        /* Compute sums of each element separately */
        MapSeq(λ(TupleType(
          /*x_el_in_chs*/ AT(Float, input_shape.nChannels), 
          /*k_el_in_chs*/ AT(Float, input_shape.nChannels)),
          (single_element) =>
            /*Join() o*/
            /*MapSeq(toGlobal(id)) o */ReduceSeq(add, toPrivate(id) $ 0.0f) o
            MapSeq(λ(TupleType(Float /*x_el_in_ch*/ , Float /*k_el_in_ch*/),
              (el_in_ch) =>
                mult(toPrivate(id) $ /*x_el_in_chs*/ Get(el_in_ch, 0),
                     toPrivate(id) $ /*k_el_in_ch*/ Get(el_in_ch, 1)))) $
            /* Zip across all input channels of the one element */
            Zip(Get(single_element, 0), Get(single_element, 1))
        )) $ tile_of_els
      })

    /* Reduces weighted pass window rows for each channel.
     * NB: Rows are already partially reduced by the factor of els_per_thread in ReduceAndWeighInputChannels()
     * Returns:
     * AT(Float, kernels_per_group) */
    def ReduceRow(): FunDecl =
      λ(AT(AT(Float, kernel_sliding.size / els_per_thread), kernels_per_group),
        (weighted_row) => {
          Join() o MapLcl(1)(λ((weighted_row_per_out_ch) => {
            MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) $ weighted_row_per_out_ch
          })) $ weighted_row
      })

    /* Reduces weighted pass windows for each channel.
     * Returns:
     * AT(Float, kernels_per_group) */
    def ReduceWindowAndAddBias(): FunDecl =
      λ(AT(AT(Float, kernel_sliding.size), kernels_per_group),
        AT(Float, kernels_per_group),
        (partially_reduced_windows, biases) => {
          Join() o Join() o
          MapLcl(2)(λ((reduced_rows_to_wrap, bias) => {
            MapLcl(1)(λ((reduced_rows) => {
              // Reduce weighted pass window separately for each output channel
              MapSeq(toGlobal(activation_f)) o ReduceSeq(add, id(bias)) $ reduced_rows
            })) o /* Wrap into an array of 1 element. This is to avoid race condition in MapLcl(0) by using 1 thread. */
            Split(kernel_sliding.size) $ reduced_rows_to_wrap
          })) $ Zip(partially_reduced_windows, biases)
      })

    Layer
  }
  

  def apply(liftFPropGenerator: (UserFun, Shape, SlidingWindowConfig, Int, SlidingWindowConfig, Int, Int) => FunDecl,
            activationFun: UserFun,
            elsPerThread: Int,
            kernelsPerGroup: Int,
            inputTileSize: Int,
            nKernels: Int,
            inputShape: Shape, kernelSize: Int, kernelStride: Int
            /*pathToInputs: String, pathToResults: String*/): Conv = {
    /**
    * Class factory: verifies that an object can be created,
    * initializes variables, computes workgroup sizes.
    */

    val exceptionMsgPrefix: String = "In the Conv layer with the following configuration:\n" +
      configToString(elsPerThread, nKernels, kernelsPerGroup, kernelSize, kernelStride)

    /* Tiles */
    val kernelSliding: SlidingWindowConfig = SlidingWindowConfig(
      size = kernelSize,
      stride = kernelStride,
      n = {
        val n: Float = (inputTileSize - (kernelSize - kernelStride)).toFloat / kernelStride
        if (n % 1 != 0) throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          f"input tiles ($inputTileSize%d) are not divisible by the chosen kernelSize ($kernelSize%d) " +
          f"and kernelStride ($kernelStride%d)")
        n.toInt
      }
    )
    val inputTiling: SlidingWindowConfig = {
      val stride = inputTileSize - (kernelSliding.size - kernelSliding.stride)
      SlidingWindowConfig(
        size = inputTileSize,
        stride = stride,
        n = 1 + Math.ceil((inputShape.size - inputTileSize).toFloat / stride).toInt)
    }

    /* Check parameters */
    if (nKernels % kernelsPerGroup != 0)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"the number of kernels ($nKernels%d) must be divisible by kernelsPerGroup ($kernelsPerGroup%d)")

    if (kernelSliding.size % elsPerThread != 0)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"kernel size in all dimensions (=${kernelSliding.size}%d) must be divisible by elsPerThread " +
        f"($elsPerThread%d)")

    /* Padding */
    // Calculate how much padding is required
    inputShape.sizePadded = inputTiling.size + inputTiling.stride * (inputTiling.n - 1)
    inputShape.sizePadded = inputShape.sizePadded

    val outputShape: Shape = { Shape(
      nBatches = inputShape.nBatches,
      nInputs = inputShape.nInputs,
      size = ((inputShape.size - (kernelSliding.size - kernelSliding.stride)).toFloat / kernelSliding.stride).toInt,
      sizePadded = {
        val sizePadded: Float = (inputShape.sizePadded - (kernelSliding.size - kernelSliding.stride)).toFloat /
          kernelSliding.stride
        if (sizePadded % 1 != 0) throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          "padded inputs are not divisible by the chosen kernelShape and kernelStride")
        sizePadded.toInt
      },
      nChannels = nKernels)
    }

    /* Parallelization parameters */
    val localSize: Array[Int] = Array.fill[Int](3)(0)
    localSize(0) = scala.math.pow((inputTiling.size - (kernelSliding.size - 1)) / kernelSliding.stride, 2).toInt

    localSize(1) = (kernelsPerGroup * Math.ceil(kernelSliding.size.toFloat / elsPerThread)).toInt

    localSize(2) = kernelSliding.size

    {
      val groupSize: Int = localSize(0) * localSize(1) * localSize(2)
      if (groupSize > nn.maxWorkGroupSize)
        throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          f"group size (==$groupSize%d) must be less or equal to maxWorkGroupSize (${nn.maxWorkGroupSize}%d).\n" +
          f"Decrease nKernels or inputTileSize or increase elsPerThread ($elsPerThread%d)")
    }

    val globalSize: Array[Int] = Array.fill[Int](3)(0)
    globalSize(0) = localSize(0) * inputShape.nBatches
    globalSize(1) = localSize(1) * inputShape.nInputs * inputTiling.n * inputTiling.n
    globalSize(2) = localSize(2) * Math.ceil(nKernels.toFloat / kernelsPerGroup).toInt

    /* Now that all parameters are calculated and verified, build the layer */

    new Conv(
      liftFPropGenerator(activationFun, inputShape, inputTiling,
        nKernels,kernelSliding, kernelsPerGroup, elsPerThread),
      inputShape, outputShape,
      inputTiling, kernelSliding,
      elsPerThread, kernelsPerGroup,
      localSize, globalSize)
  }

  /* Padding */
  def pad(inputs: PaddedArray[Array5D[Float]], inputShape: Shape): Unit = {
    inputs.padded =
      Array.fill[Array4D[Float]](inputShape.nBatches)(
        Array.fill[Array3D[Float]](inputShape.nInputs)(
          Array.fill[Array2D[Float]](inputShape.sizePadded)(
            Array.fill[Array[Float]](inputShape.sizePadded)(
              Array.fill[Float](inputShape.nChannels)(0)))))
    // Add empty lines
    for {b <- 0 until inputShape.nBatches; i <- 0 until inputShape.nInputs}
      inputs.padded(b)(i) = inputs.nonPadded(b)(i).padTo(
        inputShape.sizePadded,
        Array.fill[Array[Float]](inputShape.sizePadded)(
          Array.fill[Float](inputShape.nChannels)(0)))
    // Add empty elements to lines
    for {b <- 0 until inputShape.nBatches; i <- 0 until inputShape.nInputs; h <- 0 until inputShape.size}
      inputs.padded(b)(i)(h) = inputs.nonPadded(b)(i)(h).padTo(
        inputShape.sizePadded,
        Array.fill[Float](inputShape.nChannels)(0))
  }
}