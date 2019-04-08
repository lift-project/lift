package nn.conv.versions

/**
  * Created by nm on 09/01/17.
  * Input channels are parallelised across workgroups
  */

import ir.TupleType
import ir.ast._
import lift.arithmetic.SizeVar
import nn._
import nn.conv.{Conv, ConvCompanion, ConvDatasets, SlidingWindowConfig}
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import patterns.nn.{Array2D, Array3D, Array4D, Array5D}

object Conv1 extends ConvCompanion {

    val kernel_xdim_SV = SizeVar("kernel_xdim")
    val kernel_ydim_SV = SizeVar("kernel_ydim")
    val input_xdim_SV = SizeVar("input_xdim")
    val input_ydim_SV = SizeVar("input_ydim")
    val layer_idim_SV = SizeVar("layer_idim")
    val layer_odim_SV = SizeVar("layer_odim")
    val in_channels_SV = SizeVar("in_channels")
    val out_channels_SV = SizeVar("out_channels")
    val n_inputs_SV = SizeVar("n_inputs")
    val n_batches_SV = SizeVar("n_batches")

  /* Sequential layer */
  def Seq(activation_f: UserFun, n_inputs: Int, input_xdim: Int, input_ydim: Int, in_channels: Int,
         kernel_xdim: Int, kernel_ydim: Int, out_channels: Int): Array[FunDecl] = Array(λ(
    // Lift K: y, x, i, o
    // Caffe K: o, i, y, x
//    AT(AT(AT(AT(Float, out_channels_SV), in_channels_SV), kernel_xdim_SV), kernel_ydim_SV),
    AT(AT(AT(AT(Float, kernel_xdim), kernel_ydim), in_channels), out_channels),
    AT(Float, out_channels),
    // Lift X: n, y, x, c
    // Caffe X: n, c, y, x
//    AT(AT(AT(AT(Float, in_channels_SV), input_xdim_SV), input_ydim_SV), n_inputs_SV),
    AT(AT(AT(AT(Float, input_xdim), input_ydim), in_channels), n_inputs),
    (K, B, X) => { 
      // n, y, x, c -> n, c, y, x
      Map(TransposeW() o Map(TransposeW())) o
      MapSeq(λ((single_input) => {
        MapSeq(λ((pass_strip) => {
          MapSeq(λ((pass_window) => {
            Join() o
            ReduceSeq(fun((acc, element) => {
              val input_element = Get(element, 0)
              val kernel_element = Get(element, 1)

              Join() o
                MapSeq(fun((out_channel) => {
                  val acc_out_channel = Get(out_channel, 0)
                  val kernel_out_channel = Get(out_channel, 1)

                  ReduceSeq(add, acc_out_channel) o
                    MapSeq(fun((x, w) =>
                      mult(x, w)
                    )) $ Zip(input_element, kernel_out_channel)

                })) $ Zip(acc, kernel_element)
            }), MapSeq(id) $ B
            ) $ Zip(Join() $ pass_window,
            // o, i, y, x -> 
            // o, y, i, x -> 
            // o, y, x, i -> 
            // y, o, x, i -> 
            // y, x, o, i ->
            // y * x, o, i
            Join() o Map(/*Map(Transpose()) o */Transpose()) o Transpose() o Map(Map(Transpose()) o Transpose()) $ K)
          })) $ pass_strip
        })) o Slide2D(kernel_ydim, 1, kernel_xdim, 1) $ single_input
      })) o 
        // n, c, y, x ->
        // n, y, c, x ->
        // n, y, x, c 
      Map(Map(Transpose()) o Transpose()) $ X
    }
  ))
  
  def main(args: Array[String]): Unit = {
      Executor.loadLibrary()
      Executor.init(1, 0)
//      println(Compile(Seq(Linear)))
    val input_xdim = 227
    val input_ydim = 227
    val in_channels = 3
    val n_inputs = 5
    
    val kernel_xdim = 11
    val kernel_ydim = 11
    val out_channels = 96
    
    val K = Array.fill[Array3D[Float]](out_channels)(
      Array.fill[Array2D[Float]](in_channels)(
        Array.fill[Array[Float]](kernel_ydim)(
          Array.fill[Float](kernel_xdim)(1.0f)))) 
    
    val B = Array.fill[Float](out_channels)(1.0f)
    
    val X = Array.fill[Array3D[Float]](n_inputs)(
      Array.fill[Array2D[Float]](in_channels)(
        Array.fill[Array[Float]](input_ydim)(
          Array.fill[Float](input_xdim)(1.0f))))
    
      val (outputsFlat: Array[Float], runtime) = Execute(1, 1)[Array[Float]](
        Seq(Linear, n_inputs, input_xdim, input_ydim, in_channels,
          kernel_xdim, kernel_ydim, out_channels)(0), K, B, X)
    println(outputsFlat)
  }
  val locA: Int = 0
  val locB: Int = 1
  val locC: Int = 2


  /* Parallel layer */
  def Par(activationF: UserFun, inputShape: Shape, tiling: SlidingWindowConfig, nKernels: Int,
          sliding: SlidingWindowConfig,
          kernelsPerGroup: Int, seqElsPerThread: Int, vectorLen: Int,
          coalesce: Boolean, unrollReduce: Boolean): Array[FunDecl] = {
    val n_tiles_per_input: Int = tiling.n
    val n_kwindows_per_tile: Int = sliding.n
    val n_kwindows_per_input: Int =
      ((inputShape.sizePadded - (sliding.size - sliding.stride)).toFloat /
        sliding.stride).toInt
    /* TODO: enforce size checks */
    def Layer: FunDecl = λ(
      AT(AT(AT(AT(Float, sliding.size), sliding.size), inputShape.nChannels), nKernels),
      AT(Float, nKernels),
      AT(AT(AT(AT(AT(Float, inputShape.sizePadded), inputShape.sizePadded), inputShape.nChannels),
        inputShape.nInputs), inputShape.nBatches),
      (K, B, X) => {

        MapWrg(locC)(λ(AT(
          TupleType(AT(AT(AT(AT(Float, sliding.size), sliding.size), n_kwindows_per_tile),
            n_kwindows_per_tile),
            AT(AT(AT(Float, sliding.size), sliding.size), nKernels)),
          inputShape.nChannels * n_tiles_per_input * n_tiles_per_input * inputShape.nInputs),
          (inputs_batch_and_K) => {

            MapWrg(locB)(λ(AT(AT(AT(AT(AT(AT(AT(Float, inputShape.nChannels), kernelsPerGroup),
              n_kwindows_per_input), n_kwindows_per_input), n_tiles_per_input), n_tiles_per_input),
              inputShape.nInputs),
              (kGroup_inputs_tiles_tiles_kwins_kwins_ks_inChs) => {
              MapWrg(locA)(λ(AT(AT(AT(AT(Float, inputShape.nChannels), kernelsPerGroup),
                n_kwindows_per_input), n_kwindows_per_input),
                (tile_kwins_kwins_ks_inChs) => {
                MapLcl(locC)(λ(AT(AT(Float, inputShape.nChannels), kernelsPerGroup),
                  (kwin_ks_inChs_toWrap) => {
                    MapLcl(locB)(λ(AT(AT(Float, inputShape.nChannels), kernelsPerGroup),
                      (kwin_ks_inChs) => { Join() o
                      MapLcl(locA)(λ(AT(Float, inputShape.nChannels),
                        (k_inChs) => {
                          ReduceSeq(add, 0.0f) $ k_inChs
                      })) $ kwin_ks_inChs
                    })) o Split(kernelsPerGroup) $ kwin_ks_inChs_toWrap
                })) o Join() o Join() $ tile_kwins_kwins_ks_inChs
              })) o Join() o Join() $ kGroup_inputs_tiles_tiles_kwins_kwins_ks_inChs
            })) o
            // TODO: Transpose((0, 1, 3, 2))
            /*  DEPRECATED (n_kernel_groups, nInputs, n_tiles_per_input, n_k_passes, n_tiles_per_input,
             *   n_k_windows, kernels_per_group, nChannels) ->
             *
             *  (n_kernel_groups, nInputs, n_tiles_per_input * n_k_passes, n_tiles_per_input * n_k_windows,
             *  kernels_per_group, nChannels) ->
             *
             *  (n_kernel_groups, nInputs, n_kwindows_per_input, n_kwindows_per_input,
             *  kernels_per_group, nChannels) */
//            Map(Map(Join() o Map(Map(Join())))) o
            /*  DEPRECATED (n_kernel_groups, nInputs, n_tiles_per_input, n_tiles_per_input, n_k_passes,
             *   n_k_windows, kernels_per_group, nChannels) ->
             *
             *  (n_kernel_groups, nInputs, n_tiles_per_input, n_k_passes, n_tiles_per_input,
             *   n_k_windows, kernels_per_group, nChannels) */
//            Map(Map(Map(Transpose()))) o
            /*  (n_kernel_groups, nInputs, n_tiles_per_input, n_tiles_per_input, kernels_per_group,
             *   n_k_passes, n_k_windows, nChannels) ->
             *  (n_kernel_groups, nInputs, n_tiles_per_input, n_tiles_per_input, n_k_passes,
             *   n_k_windows, kernels_per_group, nChannels) */
            Map(Map(Map(Map(Map(Transpose()) o Transpose())))) o
            /*  (n_kernel_groups, nInputs, nChannels, n_tiles_per_input, n_tiles_per_input,
             *   kernels_per_group, n_k_passes, n_k_windows) ->
             *  (n_kernel_groups, nInputs, n_tiles_per_input, n_tiles_per_input, kernels_per_group,
             *   n_k_passes, n_k_windows, nChannels) */
            Map(Map(Map(Map(Map(Map(Transpose()) o Transpose()) o Transpose()) o Transpose()) o Transpose())) o
            /*  (n_kernel_groups, nInputs * nChannels * n_tiles_per_input * n_tiles_per_input,
             *   kernels_per_group, n_k_passes, n_k_windows) ->
             *  (n_kernel_groups, nInputs, nChannels, n_tiles_per_input, n_tiles_per_input,
             *   kernels_per_group, n_k_passes, n_k_windows) */
              Map(Split(inputShape.nChannels) o Split(n_tiles_per_input) o Split(n_tiles_per_input)) o
              //
              MapWrg(locA)(λ(TupleType(AT(AT(AT(AT(Float, sliding.size), sliding.size),
                n_kwindows_per_tile), n_kwindows_per_tile),
                AT(AT(AT(Float, sliding.size), sliding.size), nKernels)),
                (input_tile_and_K) => {
                  //
                  MapWrg(locB)(λ(TupleType(
                    AT(AT(AT(Float, sliding.size), sliding.size),
                      kernelsPerGroup), AT(Float, kernelsPerGroup)),
                    (kernels_group) => {
                    /* (kernels_per_group, n_k_passes * n_k_windows) ->
                     * (kernels_per_group, n_k_passes, n_k_windows) */
                    Map(Split(n_kwindows_per_tile)) o
                      /* (n_passes * n_windows, kernels_per_group) ->
                       * (kernels_per_group, n_k_passes * n_k_windows) */
                      TransposeW() o
                      MapLcl(locC)(λ((pass_window) => {
                        λ(AT(AT(Float, sliding.size), kernelsPerGroup), (partially_reduced_window) =>
                          // TODO: add if conditional to remove final reduce in case els_per_thread == kernel_size
                          ReduceWindowAndAddBias()(partially_reduced_window, /* biases */Get(kernels_group, 1))) o
                          /* (kernel_sliding.size, kernels_per_group) -> (kernels_per_group, kernel_sliding.size) */
                          TransposeW() o
                          MapLcl(locB)(λ((window_row, kernels_row) => {
                            ReduceRow() o
                              // (kernels_per_group, kernel_sliding.size / els_per_thread)
                              Map(Join(/*tiles of elements*/)/* o
                        MapSeq(/* Dissolve one-element output of Reduce */Join())*/) o
                              Split(sliding.size / seqElsPerThread) o
                              MapLcl(locA)(WeighElementSequence()) o
                              /* (kernels_per_group, kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels) ->
                               * (kernels_per_group * kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels)*/
                              Join() o
                              /* (kernels_per_group, kernel_sliding.size, input_shape.nChannels) ->
                               * (kernels_per_group, kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels) */
                              Map(/* for each kernel in the tile */
                                λ((kernel_row) => Split(seqElsPerThread) $
                                  Zip(
                                    /*ReorderStride(els_per_thread) $ */window_row,
                                    /*ReorderStride(input_shape.nChannels) $ */ debug.PrintTypeInConsole("kernel_row") $ kernel_row))) o
                              /* (kernel_sliding.size, input_shape.nChannels, kernels_per_group) ->
                               * (kernels_per_group, kernel_sliding.size, input_shape.nChannels) */
                              Transpose() o Map(Transpose()) $ kernels_row
                          })) $ Zip(pass_window, RestoreKernelShape() $ /* weights */ Get(kernels_group, 0))
                      })) o toLocal(MapLcl(locC)(λ((pass_window) =>
                      MapLcl(locA)(λ((window_row) => {
                        MapLcl(locB)(id) $ window_row
                      })) $ pass_window))) o
                      /* (n_passes, n_windows, n_rows) -> (n_passes*n_windows, n_rows) */
                      Join() $ Get(input_tile_and_K, 0)
                  })) $ ReshapeAndGroupKernels()(Get(input_tile_and_K, 1), B)
                })) $ inputs_batch_and_K
          })) $ MatchXandK()(SlideX() $ X, K)
      }
    )

    /* Produces a tiled slided version of X.
     * Returns:
     * AT(AT(AT(AT(AT(AT(AT(Float, kernel_sliding.size), kernel_sliding.size), n_kwindows_per_tile),
     * n_kwindows_per_tile), input_shape.nChannels),
     * n_tiles_per_input * n_tiles_per_input * input_shape.nInputs), input_shape.nBatches) */
    def SlideX(): FunDecl =
      λ(AT(AT(AT(AT(AT(Float, inputShape.sizePadded), inputShape.sizePadded), inputShape.nChannels),
        inputShape.nInputs), inputShape.nBatches), (X) =>
        debug.PrintTypeInConsole("After SLIDE") o Map(Join() o Map(Transpose() o Map(Join() o
          TiledSlidedND(2)(sliding.size, sliding.stride, tiling.stride)))) $ X)

    /* Matches X and K based on input channels
     * Returns:
     * AT(AT(TupleType(
     *    AT(AT(AT(AT(Float, kernel_sliding.size), kernel_sliding.size), n_kwindows_per_tile),
     *    n_kwindows_per_tile),
     *    AT(AT(AT(Float, kernel_sliding.size), kernel_sliding.size), n_kernels)),
     * input_shape.nChannels * n_tiles_per_input * n_tiles_per_input * input_shape.nInputs), input_shape.nBatches) */
    def MatchXandK(): FunDecl =
      λ(AT(AT(AT(AT(AT(AT(AT(Float, sliding.size), sliding.size), n_kwindows_per_tile),
        n_kwindows_per_tile), inputShape.nChannels),
        n_tiles_per_input * n_tiles_per_input * inputShape.nInputs), inputShape.nBatches),
        AT(AT(AT(AT(Float, sliding.size), sliding.size), inputShape.nChannels), nKernels),
        (X, K) =>
        Map(Join() o Map(λ((tile_inChs_kwins_kwins_rows_els) => {
          Zip(tile_inChs_kwins_kwins_rows_els, Transpose() $ K)
        }))) $ X)

    /* Reshapes kernels -- makes the output channels th e outermost dimension -- and groups them.
     * Returns:
     * AT(TupleType(
     *   /* weights */ AT(AT(AT(Float, kernel_sliding.size), kernel_sliding.size), kernels_per_group),
     *   /* biases */ AT(Float, kernels_per_group)),
     *   n_kernel_groups) */
    def ReshapeAndGroupKernels(): FunDecl =
      λ(AT(AT(AT(Float, sliding.size), sliding.size), nKernels),
        //AT(AT(AT(AT(Float, n_kernels), input_shape.nChannels), kernel_sliding.size), kernel_sliding.size),
        AT(Float, nKernels), (kernels, biases) =>
          Zip(Split(kernelsPerGroup) $ kernels, Split(kernelsPerGroup) $ biases))

    /* Reshapes the kernel back to the original shape, where output channels are the lowest dimension
     * of the tensor.
     * TODO: get rid of this - there is no advantage of having this
     * Returns:
     * AT(AT(AT(AT(Float, kernels_per_group), input_shape.nChannels), kernel_sliding.size), kernel_sliding.size) */
    def RestoreKernelShape(): FunDecl =
      λ(AT(AT(AT(AT(Float, inputShape.nChannels), sliding.size), sliding.size), kernelsPerGroup),
        (kernels_tile) =>
          Map(Map(Transpose()) o Transpose()) o Transpose() $ kernels_tile)

    /* Computes a weighted sum of a batch of elements for one output channel.
     * Returns:
     * AT(Float, 1) */
    def WeighElementSequence(): FunDecl =
      λ(AT(TupleType(Float, Float), seqElsPerThread),
        (tile_of_els) => {
          /* Compute a sum of the whole batch */
          MapSeq(toLocal(id)) o ReduceSeq(add, toPrivate(id) $ 0.0f) o
            /* Compute sums of each element separately */
            MapSeq(λ(TupleType(Float, Float),
              (single_element) =>
                // TODO: make sure mult doesn't save to global memory first
                toPrivate(id) $ mult(Get(single_element, 0), Get(single_element, 1))
            )) o debug.PrintTypeInConsole("tile_of_els") $ tile_of_els
        })

    /* Reduces weighted pass window rows for each channel.
     * NB: Rows are already partially reduced by the factor of els_per_thread in WeighElementSequence()
     * Returns:
     * AT(Float, kernels_per_group) */
    def ReduceRow(): FunDecl =
      λ(AT(AT(Float, sliding.size / seqElsPerThread), kernelsPerGroup),
        (weighted_row) => {
          Join() o MapLcl(locA)(λ((weighted_row_per_out_ch) => {
            MapSeq(toLocal(id)) o ReduceSeq(add, 0.0f) $ weighted_row_per_out_ch
          })) $ weighted_row
        })

    /* Reduces weighted pass windows for each channel.
     * Returns:
     * AT(Float, kernels_per_group) */
    def ReduceWindowAndAddBias(): FunDecl =
      λ(AT(AT(Float, sliding.size), kernelsPerGroup),
        AT(Float, kernelsPerGroup),
        (partially_reduced_windows, biases) => {
          Join() o Join() o
            MapLcl(locB)(λ((reduced_rows_to_wrap, bias) => {
              MapLcl(locA)(λ((reduced_rows) => {
                // Reduce weighted pass window separately for each output channel
                MapSeq(toGlobal(activationF)) o ReduceSeq(add, toPrivate(id) $ bias) $ reduced_rows
              })) o /* Wrap into an array of 1 element. This is to avoid race condition in MapLcl(a) by using 1 thread. */
                Split(sliding.size) $ reduced_rows_to_wrap
            })) $ Zip(partially_reduced_windows, biases)
        })

    Array(Layer)
  }


  def apply(iP: InitParameters): Conv = {
    /**
      * Class factory: verifies that an object can be created,
      * initializes variables, computes workgroup sizes.
      */

    val exceptionMsgPrefix: String = "In the Conv layer with the following configuration:\n" +
      conv.configToString(iP.inputShape.size, -1, iP.optParams.elsPerThread, iP.dim.nKernels,
        iP.optParams.kernelsPerGroup, iP.optParams.vectorLen, 
        iP.optParams.coalesce, iP.optParams.unrollReduce,
        iP.dim.kernelSize, iP.dim.kernelStride, iP.optParams.inputTileSize)

    /* Tiles */
    val kernelSliding: SlidingWindowConfig = SlidingWindowConfig(
      size = iP.dim.kernelSize,
      stride = iP.dim.kernelStride,
      n = {
        val n: Float = (iP.optParams.inputTileSize - (iP.dim.kernelSize - iP.dim.kernelStride)).toFloat /
          iP.dim.kernelStride
        if (n % 1 != 0) throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          f"input tiles (${iP.optParams.inputTileSize}%d) are not divisible by the chosen " +
          f"kernelSize (${iP.dim.kernelSize}%d) and kernelStride (${iP.dim.kernelStride}%d)")
        n.toInt
      },
      nChannels = iP.dim.nKernels
    )
    val inputTiling: SlidingWindowConfig = {
      val stride = iP.optParams.inputTileSize - (kernelSliding.size - kernelSliding.stride)
      SlidingWindowConfig(
        size = iP.optParams.inputTileSize,
        stride = stride,
        // TODO: change to n = ceil((iP.inputShape.size - iP.inputTileSize + stride) / stride)
        // It's the same, but makes more sense
        n = 1 + Math.ceil((iP.inputShape.size - iP.optParams.inputTileSize).toFloat / stride).toInt)
    }

    /* Check parameters */
    if (iP.dim.nKernels % iP.optParams.kernelsPerGroup != 0)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"the number of kernels (${iP.dim.nKernels}%d) must be divisible by " +
        f"kernelsPerGroup (${iP.optParams.kernelsPerGroup}%d)")

    if (kernelSliding.size % iP.optParams.elsPerThread != 0)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"kernel size in all dimensions (=${kernelSliding.size}%d) must be divisible by elsPerThread " +
        f"(${iP.optParams.elsPerThread}%d)")

    /* Padding */
    // Calculate how much padding is required
    // TODO: change to sizePadded = inputTiling.stride * inputTiling.n + (inputTiling.size - inputTiling.stride)
    // It's the same, but makes more sense
    iP.inputShape.sizePadded = inputTiling.size + inputTiling.stride * (inputTiling.n - 1)
    println(f"inputTiling.n=${inputTiling.n}%d")
    println(f"iP.inputShape.sizePadded=${iP.inputShape.sizePadded}%d")

    val outputShape: Shape = { Shape(
      nBatches = iP.inputShape.nBatches,
      nInputs = iP.inputShape.nInputs,
      size = ((iP.inputShape.size - (kernelSliding.size - kernelSliding.stride)).toFloat / kernelSliding.stride).toInt,
      sizePadded = {
        val sizePadded: Float = (iP.inputShape.sizePadded - (kernelSliding.size - kernelSliding.stride)).toFloat /
          kernelSliding.stride
        if (sizePadded % 1 != 0) throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          "padded inputs are not divisible by the chosen kernelShape and kernelStride")
        sizePadded.toInt
      },
      nChannels = iP.dim.nKernels)
    }

    /* Parallelization parameters */
    val localSize: Array[Int] = Array.fill[Int](3)(0)

    localSize(locA) = (iP.optParams.kernelsPerGroup *
      Math.ceil(kernelSliding.size.toFloat / iP.optParams.elsPerThread)).toInt
    localSize(locB) = kernelSliding.size
    // TODO: make sure it is smaller than 64 (clinfo)
    localSize(locC) = scala.math.pow(kernelSliding.n, 2).toInt

    {
      val groupSize: Int = localSize(0) * localSize(1) * localSize(2)
      if (groupSize > nn.maxWorkGroupSize)
        throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          f"group size (==$groupSize%d) must be less or equal to maxWorkGroupSize (${nn.maxWorkGroupSize}%d).\n" +
          f"Decrease nKernelsPerGroup or inputTileSize or increase elsPerThread (${iP.optParams.elsPerThread}%d)")
    }

    val globalSize: Array[Int] = Array.fill[Int](3)(0)
    globalSize(locA) = localSize(locA) * iP.inputShape.nInputs * inputTiling.n * inputTiling.n * iP.inputShape.nChannels
    globalSize(locB) = localSize(locB) * Math.ceil(iP.dim.nKernels.toFloat / iP.optParams.kernelsPerGroup).toInt
    globalSize(locC) = localSize(locC) * iP.inputShape.nBatches

    /* Now that all parameters are calculated and verified, build the layer */

    new Conv1(
      iP.liftFPropFactory(iP.activationFun, iP.inputShape, inputTiling,
        iP.dim.nKernels,kernelSliding, iP.optParams.kernelsPerGroup, iP.optParams.elsPerThread, iP.optParams.vectorLen,
        iP.optParams.coalesce, iP.optParams.unrollReduce),
      iP.inputShape, outputShape,
      inputTiling, kernelSliding,
      iP.optParams.elsPerThread, iP.optParams.kernelsPerGroup,  iP.optParams.vectorLen, 
      iP.optParams.coalesce, iP.optParams.unrollReduce,
      localSize, globalSize)
  }

  /* Padding */
  def pad(inputs: PaddedArray[Array5D[Float]], inputShape: Shape): Unit = {
    inputs.padded =
      Array.fill[Array4D[Float]](inputShape.nBatches)(
        Array.fill[Array3D[Float]](inputShape.nInputs)(
          Array.fill[Array2D[Float]](inputShape.nChannels)(
            Array.fill[Array[Float]](inputShape.sizePadded)(
              Array.fill[Float](inputShape.sizePadded)(0)))))

    // Add empty elements to lines
    for {b <- 0 until inputShape.nBatches; i <- 0 until inputShape.nInputs; c <- 0 until inputShape.nChannels
         h <- 0 until inputShape.size}
      inputs.padded(b)(i)(c)(h) = inputs.nonPadded(b)(i)(c)(h).padTo(inputShape.sizePadded, 0.0f)

    // Add empty lines
    for {b <- 0 until inputShape.nBatches; i <- 0 until inputShape.nInputs; c <- 0 until inputShape.nChannels}
      inputs.padded(b)(i)(c) = inputs.padded(b)(i)(c).padTo(
        inputShape.sizePadded,
        Array.fill[Float](inputShape.sizePadded)(0))

    //    // Add empty lines
    //    for {b <- 0 until inputShape.nBatches; i <- 0 until inputShape.nInputs}
    //      inputs.padded(b)(i) = inputs.nonPadded(b)(i).padTo(
    //        inputShape.sizePadded,
    //        Array.fill[Array[Float]](inputShape.sizePadded)(
    //          Array.fill[Float](inputShape.nChannels)(0)))
    //    // Add empty elements to lines
    //    for {b <- 0 until inputShape.nBatches; i <- 0 until inputShape.nInputs; h <- 0 until inputShape.size}
    //      inputs.padded(b)(i)(h) = inputs.nonPadded(b)(i)(h).padTo(
    //        inputShape.sizePadded,
    //        Array.fill[Float](inputShape.nChannels)(0))
  }
}

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
case class Conv1(override val liftFProp: Array[FunDecl],
                 override val inputShape: Shape, override val outputShape: Shape,
                 override val inputTiling: SlidingWindowConfig, override val kernelSliding: SlidingWindowConfig,
                 override val elsPerThread: Int, override val kernelsPerGroup: Int,
                 override val vectorLen: Int, override val coalesce: Boolean, override val unrollReduce: Boolean,
                 override val localSize: Array[Int], override val globalSize: Array[Int])
  extends Conv(liftFProp, inputShape, outputShape, inputTiling, kernelSliding,
    elsPerThread, kernelsPerGroup, vectorLen, coalesce, unrollReduce, localSize, globalSize) {

  override def toString: String =
    nn.conv.configToString(inputShape.size, outputShape.sizePadded, elsPerThread, outputShape.nChannels,
      kernelsPerGroup, vectorLen, coalesce, unrollReduce, kernelSliding.size, kernelSliding.stride, inputTiling.size)
  var runtime: Double = 0

  def groupAndUnpad(outputsFlat: Array[Float], datasets: NetDatasets): Unit = {
    datasets.asInstanceOf[ConvDatasets].outputs.nonPadded =
      patterns.nn.group(outputsFlat, (outputShape.nBatches, outputShape.nInputs,
        outputShape.nChannels, outputShape.sizePadded, outputShape.sizePadded)).map(
        batch => batch.map(
          input => input.map(
            channel => channel.map(
              row => row.slice(0, outputShape.size)
            ).slice(0, outputShape.size))))
  }
}