package nn.conv.versions

/**
  * Created by nm on 09/01/17.
  * Input channels are processed sequentially in each thread.
  * ARM Mali GPU-optimised
  */

import ir.{ArrayType, TupleType}
import ir.ast._
import lift.arithmetic.SizeVar
import nn._
import nn.conv.{Conv, ConvCompanion, ConvDatasets, SlidingWindowConfig}
import opencl.ir._
import opencl.ir.pattern._
import patterns.nn.{Array2D, Array3D, Array4D, Array5D}

/**
  * The companion object that contains the Lift expressions, configuration preprocessing and
  * verification, and helper functions.
  */
object Conv2 extends ConvCompanion {
    val kernel_xdim_SV = SizeVar("kernel_xdim_SV")
    val kernel_ydim_SV = SizeVar("kernel_ydim_SV")
    val input_xdim_SV = SizeVar("input_xdim_SV")
    val input_ydim_SV = SizeVar("input_ydim_SV")
    val in_channels_SV = SizeVar("in_channels_SV")
    val out_channels_SV = SizeVar("out_channels_SV")
    val n_inputs_SV = SizeVar("n_inputs_SV")
    val n_batches_SV = SizeVar("n_batches_SV")

  /* Sequential layer */
  def Seq(kernel_h: Int, kernel_w: Int, activation_f: UserFun): FunDecl = λ(
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
  )
  val locA: Int = 0//0
  val locB: Int = 1//1
  val locC: Int = 2//2

  val idF4Custom = UserFun("idF4", "x", "{ return x; }", Float4, Float4)


  /* Parallel layer */
  def Par(activationF: UserFun, inputShape: Shape, tiling: SlidingWindowConfig, nKernels: Int,
          sliding: SlidingWindowConfig,
          kernelsPerGroup: Int, seqElsPerThread: Int, vectorLen: Int,
          coalesce: Boolean, unrollReduce: Boolean): Array[FunDecl] = {

    /* Original */
//    def Layer: FunDecl = λ(
//      AT(AT(AT(AT(Float, kernel_sliding.size), kernel_sliding.size), input_shape.nChannels), n_kernels),
//      AT(Float, n_kernels),
//      AT(AT(AT(AT(AT(Float, input_shape.sizePadded), input_shape.sizePadded), input_shape.nChannels),
//        input_shape.nInputs), input_shape.nBatches),
//      (K, B, X) => {
//        MapWrg(locC)(λ(AT(AT(AT(AT(AT(AT(AT(Float, input_shape.nChannels), kernel_sliding.size), kernel_sliding.size),
//          /* TODO: enforce size checks */(input_tiling.size - (kernel_sliding.size - kernel_sliding.stride)) / kernel_sliding.size),
//          (input_tiling.size - (kernel_sliding.size - kernel_sliding.stride)) / kernel_sliding.size),
//          ((input_shape.sizePadded - (input_tiling.size - input_tiling.stride)) / input_tiling.size) *
//            ((input_shape.sizePadded - (input_tiling.size - input_tiling.stride)) / input_tiling.size) *
//            input_shape.nInputs), input_shape.nBatches),
//          (inputs_batch) => {
//            /*  (nImages, input_tiling.n, input_tiling.n, n_k_passes, n_k_windows, nKernels) ->
//            *   (nImages, input_tiling.n, n_k_passes, input_tiling.n, n_k_windows, nKernels) ->
//            *   (nImages, input_tiling.n, n_k_passes, input_tiling.n * n_k_windows, nKernels) ->
//            *   (nImages, input_tiling.n * n_k_passes, input_tiling.n * n_k_windows, nKernels) ->
//            *   (nImages, n_passes, n_windows, nKernels) ->
//            *   (nImages, nKernels, n_passes, n_windows) -> */
//            Map(/*input*/TransposeW() o Join() o Map(/*tile_row*/Map(TransposeW() o Join()) o TransposeW())) o
//              Split(input_tiling.n) o Split(input_tiling.n) o
//              /*  (nImages * input_tiling.n * input_tiling.n, nKernels, n_k_passes, n_k_windows) ->
//               *  (nImages * input_tiling.n * input_tiling.n, n_k_passes, n_k_windows, nKernels) */
//              λ(AT(AT(AT(AT(Float, kernel_sliding.n), kernel_sliding.n),
//                n_kernels), input_shape.nInputs * input_tiling.n * input_tiling.n),
//                (tiled_outputs) => Map(Map(TransposeW()) o TransposeW()) $ tiled_outputs) o
//              MapWrg(locA)(λ((input_tile) => {
//                /* (nKernels / kernels_per_group, kernels_per_group, n_k_passes, n_k_windows) ->
//                *  (nKernels, n_k_passes, n_k_windows) */
//                Join() o MapWrg(locB)(λ(TupleType(
//                  AT(AT(AT(AT(Float, input_shape.nChannels), kernel_sliding.size), kernel_sliding.size), kernels_per_group),
//                  AT(Float, kernels_per_group)),
//                  (kernels_tile) => {
//                    /* (kernels_per_group, n_k_passes*n_k_windows) ->
//                     * (kernels_per_group, n_k_passes, n_k_windows) */
//                    Map(Split(kernel_sliding.n)) o
//                      /* (n_passes*n_windows, kernels_per_group) -> (kernels_per_group, n_k_passes*n_k_windows) */
//                      TransposeW() o
//                      MapLcl(locC)(λ((pass_window) => {
//                        λ(AT(AT(Float, kernel_sliding.size), kernels_per_group), (partially_reduced_window) =>
//                          // TODO: add if conditional to remove final reduce in case els_per_thread == kernel_size
//                          ReduceWindowAndAddBias()(partially_reduced_window, /* biases */Get(kernels_tile, 1))) o
//                          /* (kernel_sliding.size, kernels_per_group) -> (kernels_per_group, kernel_sliding.size) */
//                          TransposeW() o
//                          MapLcl(locB)(λ((window_row, kernels_row) => {
//                            ReduceRow() o
//                              // (kernels_per_group, kernel_sliding.size / els_per_thread)
//                              Map(Join(/*tiles of elements*/)/* o
//                        MapSeq(/* Dissolve one-element output of Reduce */Join())*/) o
//                              Split(kernel_sliding.size / els_per_thread) o
//                              MapLcl(locA)(ReduceAndWeighInputChannels()) o
//                              /* (kernels_per_group, kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels) ->
//                               * (kernels_per_group * kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels)*/
//                              Join() o
//                              /* (kernels_per_group, kernel_sliding.size, input_shape.nChannels) ->
//                               * (kernels_per_group, kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels) */
//                              Map(/* for each kernel in the tile */
//                                λ((kernel_row) => Split(els_per_thread) $
//                                  Zip(
//                                    /*ReorderStride(els_per_thread) $ */window_row,
//                                    /*ReorderStride(input_shape.nChannels) $ */kernel_row))) o
//                              /* (kernel_sliding.size, input_shape.nChannels, kernels_per_group) ->
//                               * (kernels_per_group, kernel_sliding.size, input_shape.nChannels) */
//                              Transpose() o Map(Transpose()) $ kernels_row
//                          })) $ Zip(pass_window, RestoreKernelShape() $ /* weights */ Get(kernels_tile, 0))
//                      })) o toLocal(MapLcl(locC)(λ((pass_window) =>
//                      MapLcl(locA)(λ((window_row) => {
//                        MapLcl(locB)(MapSeq(id)) $ window_row
//                      })) $ pass_window))) o
//                      /* (n_passes, n_windows, n_rows) -> (n_passes*n_windows, n_rows) */
//                      Join() $ input_tile
//                  })) $ ReshapeTileAndLoadKernels()(K, B)
//              })) $ inputs_batch
//          })) o SlideX() $ X
//      }
//    )
    
    /* No local memory */
    def Layer: FunDecl = λ(
      AT(AT(AT(AT(Float, sliding.size), sliding.size), inputShape.nChannels), nKernels),
      AT(Float, nKernels),
      AT(AT(AT(AT(AT(Float, inputShape.sizePadded), inputShape.sizePadded), inputShape.nChannels),
        inputShape.nInputs), inputShape.nBatches),
      (K, B, X) => {
        MapWrg(locC)(λ(AT(AT(AT(AT(AT(AT(AT(Float, inputShape.nChannels), sliding.size), sliding.size),
          /* TODO: enforce size checks */(tiling.size - (sliding.size - sliding.stride)) / sliding.size),
          (tiling.size - (sliding.size - sliding.stride)) / sliding.size),
          ((inputShape.sizePadded - (tiling.size - tiling.stride)) / tiling.size) *
            ((inputShape.sizePadded - (tiling.size - tiling.stride)) / tiling.size) *
            inputShape.nInputs), inputShape.nBatches),
          (inputs_batch) => {
            /*  (nImages, input_tiling.n, input_tiling.n, n_k_passes, n_k_windows, nKernels) ->
            *   (nImages, input_tiling.n, n_k_passes, input_tiling.n, n_k_windows, nKernels) ->
            *   (nImages, input_tiling.n, n_k_passes, input_tiling.n * n_k_windows, nKernels) ->
            *   (nImages, input_tiling.n * n_k_passes, input_tiling.n * n_k_windows, nKernels) ->
            *   (nImages, n_passes, n_windows, nKernels) ->
            *   (nImages, nKernels, n_passes, n_windows) -> */
            Map(/*input*/TransposeW() o Join() o Map(/*tile_row*/Map(TransposeW() o Join()) o TransposeW())) o
              Split(tiling.n) o Split(tiling.n) o
              /*  (nImages * input_tiling.n * input_tiling.n, nKernels, n_k_passes, n_k_windows) ->
               *  (nImages * input_tiling.n * input_tiling.n, n_k_passes, n_k_windows, nKernels) */
              λ(AT(AT(AT(AT(Float, sliding.n), sliding.n),
                nKernels), inputShape.nInputs * tiling.n * tiling.n),
                (tiled_outputs) => Map(Map(TransposeW()) o TransposeW()) $ tiled_outputs) o
              MapWrg(locA)(λ((input_tile) => {
                /* (nKernels / kernels_per_group, kernels_per_group, n_k_passes, n_k_windows) ->
                *  (nKernels, n_k_passes, n_k_windows) */
                Join() o MapWrg(locB)(λ(TupleType(
                  AT(AT(AT(AT(Float, inputShape.nChannels), sliding.size), sliding.size), kernelsPerGroup),
                  AT(Float, kernelsPerGroup)),
                  (kernels_tile) => {
                    /* (kernels_per_group, n_k_passes*n_k_windows) ->
                     * (kernels_per_group, n_k_passes, n_k_windows) */
                    Map(Split(sliding.n)) o
                      /* (n_passes*n_windows, kernels_per_group) -> (kernels_per_group, n_k_passes*n_k_windows) */
                      TransposeW() o
                      MapLcl(locC)(λ((pass_window) => {
                        λ(AT(AT(Float, sliding.size), kernelsPerGroup), (partially_reduced_window) =>
                          // TODO: add if conditional to remove final reduce in case els_per_thread == kernel_size
                          ReduceWindowAndAddBias()(partially_reduced_window, /* biases */Get(kernels_tile, 1))) o
                          /* (kernel_sliding.size, kernels_per_group) -> (kernels_per_group, kernel_sliding.size) */
                          TransposeW() o
                          MapLcl(locB)(λ((window_row, kernels_row) => {
                            ReduceRow() o
                              // (kernels_per_group, kernel_sliding.size / els_per_thread)
                              Map(Join(/*tiles of elements*/)/* o
                        MapSeq(/* Dissolve one-element output of Reduce */Join())*/) o
                              Split(sliding.size / seqElsPerThread) o
                              MapLcl(locA)(ReduceAndWeighInputChannels()) o
                              /* (kernels_per_group, kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels) ->
                               * (kernels_per_group * kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels)*/
                              Join() o
                              /* (kernels_per_group, kernel_sliding.size, input_shape.nChannels) ->
                               * (kernels_per_group, kernel_sliding.size / els_per_thread, els_per_thread, tuple of input_shape.nChannels) */
                              Map(/* for each kernel in the tile */
                                λ((kernel_row) => Split(seqElsPerThread) $
                                  Zip(
                                    /*ReorderStride(els_per_thread) $ */window_row,
                                    /*ReorderStride(input_shape.nChannels) $ */kernel_row))) o
                              /* (kernel_sliding.size, input_shape.nChannels, kernels_per_group) ->
                               * (kernels_per_group, kernel_sliding.size, input_shape.nChannels) */
                              Transpose() o Map(Transpose()) $ kernels_row
                          })) $ Zip(pass_window, RestoreKernelShape() $ /* weights */ Get(kernels_tile, 0))
                      })) o /*toLocal(MapLcl(locC)(λ((pass_window) =>
                      MapLcl(locA)(λ((window_row) => {
                        MapLcl(locB)(MapSeq(id)) $ window_row
                      })) $ pass_window))) o*/
                      /* (n_passes, n_windows, n_rows) -> (n_passes*n_windows, n_rows) */
                      Join() $ input_tile
                  })) $ ReshapeTileAndLoadKernels()(K, B)
              })) $ inputs_batch
          })) o SlideX() $ X
      }
    )

    /* Produces a tiled slided version of X.
     * Returns:
     * AT(AT(AT(AT(AT(AT(AT(Float, input_channels), kernel_shape.s), kernel_shape.s), n_kernel_pass_windows_in_tile),
     * n_kernel_pass_strips_in_tile), n_tile_pass_windows * n_tile_pass_strips * input_shape.nInputs), input_shape.nBatches) */
    def SlideX(): FunDecl =
      λ(AT(AT(AT(AT(AT(Float, inputShape.sizePadded), inputShape.sizePadded), inputShape.nChannels),
        inputShape.nInputs), inputShape.nBatches), (X) =>
        Map(Join() o Map(Join() o
          TiledSlidedND(2)(sliding.size, sliding.stride, tiling.stride))) o
          //Map(Map(Split(input_shape.sizePadded) o ReorderStride(els_per_thread) o Join())) o
          Map(Map(Map(Transpose()) o Transpose())) $ X)

    /* Reshapes kernels -- makes the output channels the outermost dimension -- and splits them into tiles.
     * Returns:
     * AT(TupleType(
     *   /* weights */ AT(AT(AT(AT(Float, input_channels), kernel_sliding.size), kernel_sliding.size), kernels_per_group),
     *   /* biases */ AT(Float, kernels_per_group)), n_kernel_tiles) */
    def ReshapeTileAndLoadKernels(): FunDecl =
      λ(AT(AT(AT(AT(Float, sliding.size), sliding.size), inputShape.nChannels), nKernels),
        //AT(AT(AT(AT(Float, n_kernels), input_shape.nChannels), kernel_sliding.size), kernel_sliding.size),
        AT(Float, nKernels), (kernels, biases) =>
          Zip(
            /* Load kernels into local memory */
            /*MapWrg(c)(λ(AT(AT(AT(AT(Float, input_shape.nChannels), kernel_sliding.size),
              kernel_sliding.size), kernels_per_group),
              (kernels_tile_towrap) => {
              Transpose() o Join() o
              MapLcl(a)(λ(AT(AT(AT(AT(Float, input_shape.nChannels), kernel_sliding.size), kernels_per_group),
                kernel_sliding.size),
                (kernels_tile) => {
                  MapLcl(c)(λ(AT(AT(AT(Float, input_shape.nChannels), kernel_sliding.size), kernels_per_group),
                    (kernels_row) => {
                      Split(kernel_sliding.size) o Join() o
                      MapLcl(b)(λ(AT(AT(Float, input_shape.nChannels), els_per_thread),
                        (kernels_seq_of_els) => {
                          MapSeq(MapSeq(toLocal(id))) $ kernels_seq_of_els
                        })) o Split(els_per_thread) o Join() $ kernels_row
                    })) $ kernels_tile
                })) o debug.PrintType("After wrapping") o Split(kernel_sliding.size) o debug.PrintType("Before wrapping") o
                Transpose() $ kernels_tile_towrap
              })) o*/
            Split(kernelsPerGroup) o
              /* (n_out_chs, n_in_chs, n_rows, n_columns) -> (n_out_chs, n_rows, n_columns, n_in_chs) */
              Map(Map(Transpose()) o Transpose()) $ kernels,
            Split(kernelsPerGroup) $ biases))

    /* Reshapes the kernel back to the original shape, where output channels are the lowest dimension
     * of the tensor.
     * TODO: get rid of this - there is no advantage of having this
     * Returns:
     * AT(AT(AT(AT(Float, kernels_per_group), input_shape.nChannels), kernel_sliding.size), kernel_sliding.size) */
    def RestoreKernelShape(): FunDecl =
      λ(AT(AT(AT(AT(Float, inputShape.nChannels), sliding.size), sliding.size), kernelsPerGroup),
        (kernels_tile) =>
          Map(Map(Transpose()) o Transpose()) o Transpose() $ kernels_tile)

    /* Computes a weighted sum of all input channels of a batch of elements for one output channel.
     * Returns:
     * AT(Float, 1) */
    /* original */
    def ReduceAndWeighInputChannels(): FunDecl =
      λ(AT(  TupleType(AT(Float, inputShape.nChannels), AT(Float, inputShape.nChannels)),   seqElsPerThread),
        (tile_of_els) => {
          /* Compute a sum of the whole batch */
          MapSeq(toLocal(id)) o ReduceSeq(add, toPrivate(id) $ 0.0f) o Join() o
            /* Compute sums of each element separately */
            MapSeq(λ(TupleType(
              /*x_el_in_chs*/ AT(Float, inputShape.nChannels),
              /*k_el_in_chs*/ AT(Float, inputShape.nChannels)),
              (single_element) =>
                /*Join() o*/
                /*MapSeq(toGlobal(id)) o */ReduceSeq(add, toPrivate(id) $ 0.0f) o
                MapSeq(λ(TupleType(Float /*x_el_in_ch*/ , Float /*k_el_in_ch*/),
                  (el_in_ch) =>
                    mult(toPrivate(id) $ /*x_el_in_ch*/ Get(el_in_ch, 0),
                      toPrivate(id) $ /*k_el_in_ch*/ Get(el_in_ch, 1)))) $
                /* Zip input channels of one element */
                Zip(Get(single_element, 0), Get(single_element, 1))
            )) $ tile_of_els
        })
    
    /* Vectorization */
//    def ReduceAndWeighInputChannels(): FunDecl =
//      λ(AT(  TupleType(AT(Float, input_shape.nChannels), AT(Float, input_shape.nChannels)),   els_per_thread),
//        (tile_of_els) => {
//          /* Compute a sum of the whole batch */
//          MapSeq(toLocal(id)) o ReduceSeq(add, toPrivate(id) $ 0.0f) o Join() o
//            /* Compute sums of each element separately */
//            MapSeq(λ(TupleType(
//              /*x_el_in_chs*/ AT(Float, input_shape.nChannels),
//              /*k_el_in_chs*/ AT(Float, input_shape.nChannels)),
//              (single_element) =>
//                /*Join() o*/
//                /*MapSeq(toGlobal(id)) o */ReduceSeq(add, toPrivate(id) $ 0.0f) o
//                MapSeq(λ(TupleType(Float /*x_el_in_ch*/ , Float /*k_el_in_ch*/),
//                  (el_in_ch) =>
//                    dot(
//                      toPrivate(idF4Custom) $ /*x_el_in_ch*/ Get(el_in_ch, 0),
//                      toPrivate(idF4Custom) $ /*k_el_in_ch*/ Get(el_in_ch, 1)))) $
//                /* Zip input channels of one element */
//                Zip(asVector(4) $ Get(single_element, 0), asVector(4) $ Get(single_element, 1))
//            )) $ tile_of_els
//        })

    /* Reduces weighted pass window rows for each channel.
     * NB: Rows are already partially reduced by the factor of els_per_thread in ReduceAndWeighInputChannels()
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


  def apply(iP: InitParameters): Conv2 = {
    /**
      * Class factory: verifies that an object can be created,
      * initializes variables, computes workgroup sizes.
      */

    val exceptionMsgPrefix: String = "In the Conv layer with the following configuration:\n" +
      conv.configToString(iP.inputShape.size, -1, iP.optParams.elsPerThread,
        iP.dim.nKernels, iP.optParams.kernelsPerGroup, iP.optParams.vectorLen, 
        iP.optParams.coalesce, iP.optParams.unrollReduce,
        iP.dim.kernelSize, iP.dim.kernelStride, iP.optParams.inputTileSize)

    /* Tiles */
    val kernelSliding: SlidingWindowConfig = SlidingWindowConfig(
      size = iP.dim.kernelSize,
      stride = iP.dim.kernelStride,
      n = {
        val n: Float = (iP.optParams.inputTileSize - (iP.dim.kernelSize - iP.dim.kernelStride)).toFloat / iP.dim.kernelStride
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
    if (iP.inputShape.sizePadded != iP.inputShape.size)
      iP.inputShape.sizePadded = iP.inputShape.sizePadded

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
    localSize(locC) = scala.math.pow(
      (inputTiling.size - (kernelSliding.size - kernelSliding.stride)) / kernelSliding.stride, 2).toInt

//    val maxWorkGroupSize = nn.maxWorkGroupSize
    val maxWorkGroupSize = 256

    {
      val groupSize: Int = localSize(0) * localSize(1) * localSize(2)
      if (groupSize > maxWorkGroupSize)
        throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          f"group size (==$groupSize%d) must be less or equal to maxWorkGroupSize ($maxWorkGroupSize%d).\n" +
          f"Decrease nKernelsPerGroup or inputTileSize or increase elsPerThread (${iP.optParams.elsPerThread}%d)")
    }

    val globalSize: Array[Int] = Array.fill[Int](3)(0)
    globalSize(locA) = localSize(locA) * iP.inputShape.nInputs * inputTiling.n * inputTiling.n// * iP.inputShape.nChannels
    globalSize(locB) = localSize(locB) * Math.ceil(iP.dim.nKernels.toFloat / iP.optParams.kernelsPerGroup).toInt
    globalSize(locC) = localSize(locC) * iP.inputShape.nBatches

    /* Now that all parameters are calculated and verified, build the layer */

    new Conv2(
      iP.liftFPropFactory(iP.activationFun, iP.inputShape, inputTiling,
        iP.dim.nKernels,kernelSliding, iP.optParams.kernelsPerGroup, iP.optParams.elsPerThread, iP.optParams.vectorLen,
        iP.optParams.coalesce, iP.optParams.unrollReduce),
      iP.inputShape, outputShape,
      inputTiling, kernelSliding,
      iP.optParams.elsPerThread, iP.optParams.kernelsPerGroup, iP.optParams.vectorLen,
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
case class Conv2(override val liftFProp: Array[FunDecl],
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