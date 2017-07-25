package nn.cnn

/**
  * Created by nm on 09/01/17.
  */

import ir.ast._
import ir.{ArrayType, TupleType}
import lift.arithmetic.SizeVar
import nn._
import opencl.ir._
import opencl.ir.pattern._

object CNN {
  val kernel_xdim_SV = SizeVar("kernel_xdim_SV")
  val kernel_ydim_SV = SizeVar("kernel_ydim_SV")
  val input_xdim_SV = SizeVar("input_xdim_SV")
  val input_ydim_SV = SizeVar("input_ydim_SV")
  val layer_idim_SV = SizeVar("layer_idim_SV")
  val layer_odim_SV = SizeVar("layer_odim_SV")
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


  /* Parallel layer */
  def Par(activation_f: UserFun, input_shape: Shape, kernel_shape: Shape,
          n_inputs: Int, n_batches: Int, n_in_channels: Int, n_out_channels: Int, tile: Tile): FunDecl = {
    def AT = ArrayType // alias

    def Layer: FunDecl = λ(
      AT(AT(AT(AT(Float, n_out_channels), n_in_channels), kernel_shape.w), kernel_shape.h),
      AT(Float, n_out_channels),
      AT(AT(AT(AT(AT(Float, n_in_channels), input_shape.wPadded), input_shape.hPadded), n_inputs), n_batches),
      (K, B, X) => {
        MapWrg(1)(λ((inputs_batch) => {
          /*  (nImages, nInputTilesPerDim, nInputTilesPerDim, n_k_passes, n_k_windows, nKernels) ->
              (nImages, nInputTilesPerDim, n_k_passes, nInputTilesPerDim, n_k_windows, nKernels) ->
              (nImages, nInputTilesPerDim, n_k_passes, nInputTilesPerDim * n_k_windows, nKernels) ->
              (nImages, nInputTilesPerDim * n_k_passes, nInputTilesPerDim * n_k_windows, nKernels) ->
          *   (nImages, n_passes, n_windows, nKernels) */
          Map(/*input*/Join() o Map(/*tile_row*/Map(Join()) o TransposeW())) o
          Split(tile.nInputTilesPerDim) o Split(tile.nInputTilesPerDim) o
          /*  (nImages * nInputTilesPerDim * nInputTilesPerDim, nKernels, n_k_passes, n_k_windows) ->
           *  (nImages * nInputTilesPerDim * nInputTilesPerDim, n_k_passes, n_k_windows, nKernels) */
          λ(AT(AT(AT(AT(Float, tile.n_windows_per_tile_per_dim), tile.n_windows_per_tile_per_dim),
            n_out_channels), n_inputs * tile.nInputTilesPerDim * tile.nInputTilesPerDim),
            (tiled_outputs) => Map(Map(TransposeW()) o TransposeW()) $ tiled_outputs) o
          MapWrg(0)(λ((input_tile) => {
            /* (nKernels / tile.kernels_per_group, tile.kernels_per_group, n_k_passes, n_k_windows) ->
            *  (nKernels, n_k_passes, n_k_windows) */
            Join() o MapWrg(2)(λ(TupleType(
              AT(AT(AT(AT(Float, n_in_channels), kernel_shape.w), kernel_shape.h), tile.kernels_per_group),
              AT(Float, tile.kernels_per_group)),
              (kernels_tile) => {
                /* (tile.kernels_per_group, n_passes*n_windows) -> (tile.kernels_per_group, n_k_passes, n_k_windows) */
                Map(Split(tile.n_windows_per_tile_per_dim)) o
                /* (n_passes*n_windows, tile.kernels_per_group) -> (tile.kernels_per_group, n_k_passes*n_k_windows) */
                TransposeW() o
                MapLcl(1)(λ((pass_window) => {
                  λ(AT(AT(Float, kernel_shape.h), tile.kernels_per_group), (partially_reduced_window) =>
                    ReduceWindowAndAddBias()(partially_reduced_window, /* biases */Get(kernels_tile, 1))) o
                  /* (kernel_shape.h, tile.kernels_per_group) -> (tile.kernels_per_group, kernel_shape.h) */
                  TransposeW() o
                  MapLcl(2)(λ((window_row, kernels_row) => {
                    ReduceRow() o
                      // (tile.kernels_per_group, kernel_shape.w / tile.els_per_thread)
                      Map(Join(/*tiles of elements*/)/* o
                        MapSeq(/* Dissolve one-element output of Reduce */Join())*/) o
                      Split(kernel_shape.w / tile.els_per_thread) o
                      MapLcl(0)(ReduceAndWeighInputChannels()) o
/* (tile.kernels_per_group, kernel_shape.w / tile.els_per_thread, tile.els_per_thread, tuple of n_in_channels) ->
 * (tile.kernels_per_group * kernel_shape.w / tile.els_per_thread, tile.els_per_thread, tuple of n_in_channels)*/
                      Join() o
/* (tile.kernels_per_group, kernel_shape.w, n_in_channels) ->
 * (tile.kernels_per_group, kernel_shape.w / tile.els_per_thread, tile.els_per_thread, tuple of n_in_channels) */
                      Map(/* for each kernel in the tile */
                        λ((kernel_row) => Split(tile.els_per_thread) $ Zip(window_row, kernel_row))) o
                      /* (kernel_shape.w, n_in_channels, tile.kernels_per_group) ->
                       * (tile.kernels_per_group, kernel_shape.w, n_in_channels) */
                      Transpose() o Map(Transpose()) $ kernels_row
                  })) $ Zip(pass_window, RestoreKernelShape() $ /* weights */ Get(kernels_tile, 0))
                })) o toLocal(MapLcl(1)(λ((pass_window) =>
                MapLcl(2)(λ((window_row) => {
                  debug.PrintView("MyMsg", MapSeq(MapSeq(id))) $ window_row
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
     * AT(AT(AT(AT(AT(AT(AT(Float, input_channels), input_xdim), input_ydim), n_kernel_pass_windows_in_tile),
     * n_kernel_pass_strips_in_tile), n_tile_pass_windows * n_tile_pass_strips * n_inputs), n_batches) */
    def SlideX(): FunDecl =
      λ(AT(AT(AT(AT(AT(Float, n_in_channels), input_shape.wPadded), input_shape.hPadded), n_inputs), n_batches), (X) =>
        Map(Join() o Map(Join() o TiledSlidedND(2)(kernel_shape.s, 1, tile.inputTileSlideStep))) $ X)

    /* Reshapes kernels -- makes the output channels the outermost dimension -- and splits them into tiles.
     * Returns:
     * AT(TupleType(
     *   /* weights */ AT(AT(AT(AT(Float, input_channels), kernel_shape.w), kernel_shape.h), tile.kernels_per_group),
     *   /* biases */ AT(Float, tile.kernels_per_group)), n_kernel_tiles) */
    def ReshapeAndTileKernels(): FunDecl =
      λ(AT(AT(AT(AT(Float, n_out_channels), n_in_channels), kernel_shape.w), kernel_shape.h),
        AT(Float, n_out_channels), (kernels, biases) =>
        Zip(
          Split(tile.kernels_per_group) o
          /* (n_rows, n_columns, n_in_chs, n_out_chs) -> (n_out_chs, n_rows, n_columns, n_in_chs) */
          Transpose() o Map(Transpose() o Map(Transpose())) $ kernels,
          Split(tile.kernels_per_group) $ biases))

    /* Reshapes the kernel back to the original shape, where output channels are the lowest dimension
     * of the tensor.
     * TODO: get rid of this - there is no advantage of having this
     * Returns:
     * AT(AT(AT(AT(Float, tile.kernels_per_group), n_in_channels), kernel_shape.w), kernel_shape.h) */
    def RestoreKernelShape(): FunDecl =
      λ(AT(AT(AT(AT(Float, n_in_channels), kernel_shape.w), kernel_shape.h), tile.kernels_per_group),
        (kernels_tile) =>
        Map(Map(Transpose()) o Transpose()) o Transpose() $ kernels_tile)

    /* Computes a weighted sum of all input channels of a batch of elements for one output channel.
     * Returns:
     * AT(Float, 1) */
    def ReduceAndWeighInputChannels(): FunDecl =
      λ(AT(  TupleType(AT(Float, n_in_channels), AT(Float, n_in_channels)),   tile.els_per_thread),
        (tile_of_els) => {
        /* Compute a sum of the whole batch */
        MapSeq(toGlobal(id)) o ReduceSeq(add, toPrivate(id) $ 0.0f) o Join() o
        /* Compute sums of each element separately */
        MapSeq(λ(TupleType(/*x_el_in_chs*/ AT(Float, n_in_channels), /*k_el_in_chs*/ AT(Float, n_in_channels)),
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
     * AT(Float, tile.kernels_per_group) */
    def ReduceRow(): FunDecl =
      λ(AT(AT(Float, kernel_shape.w / tile.els_per_thread), tile.kernels_per_group),
        (weighted_row) => {
          Join() o MapLcl(0)(λ((weighted_row_per_out_ch) => {
            MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) $ weighted_row_per_out_ch
          })) $ weighted_row
      })

    /* Reduces weighted pass windows for each channel.
     * Returns:
     * AT(Float, tile.kernels_per_group) */
    def ReduceWindowAndAddBias(): FunDecl =
      λ(AT(AT(Float, kernel_shape.h), tile.kernels_per_group),
        AT(Float, tile.kernels_per_group),
        (partially_reduced_windows, biases) => {
          Join() o Join() o
          MapLcl(2)(λ((reduced_rows_to_wrap, bias) => {
            MapLcl(0)(λ((reduced_rows) => {
              // Reduce weighted pass window separately for each output channel
              MapSeq(toGlobal(activation_f)) o ReduceSeq(add, id(bias)) $ reduced_rows
            })) o /* Wrap into an array of 1 element. This is to avoid race condition in MapLcl(0) by using 1 thread. */
            Split(kernel_shape.h) $ reduced_rows_to_wrap
          })) $ Zip(partially_reduced_windows, biases)
      })

    Layer
  }
}

class CNN(/* CNN architectural parameters */
          val liftCNN: (UserFun, Shape, Shape, Int, Int, Int, Int, Tile) => FunDecl,
          val activationFun/*per layer*/: Array[UserFun],
          /* CNN parallelisation parameters */
          val elsPerThreadL1: Int,
          val kernelsPerGroupL1: Int,
          val inputTileSizeL1: Int,
          /* CNN application parameters */
          val nLayers: Int, val nBatches: Int, val nInputs: Int,
          val nKernels: Array[Int], val nInChannels: Array[Int],
          val inputShape: Array[Shape], val kernelShape: Array[Shape],
          /*val inputsL0: PaddedArray[Array5D[Float]], val kWeights: Array5D[Float],
          val kBiases: Array2D[Float], val targets: Array5D[Float],*/
          val pathToInputs: String, val pathToResults: String,
          val loadDatasets: (Int, String) => (PaddedArray[Array5D[Float]], Array5D[Float],
                                              Array2D[Float], Array5D[Float]),
          val previousCNN: CNN) {
  /**
  * Initializes variables, computes workgroup sizes.
  */
  /* Sizes */
  //val nLayers: Int = kBiases.length
  //val nBatches: Int = inputsL0.nonPadded.length
  //val nInputs: Int = inputsL0.nonPadded.head.length
  //val nInChannels: Array[Int] = Array.fill[Int](nLayers)(0)
//  val nKernels: Array[Int] = Array.fill[Int](nLayers)(0)
//  for (layerNo <- 0 until nLayers) {
//    nInChannels(layerNo) = kWeights(layerNo).head.head.length
//    nKernels(layerNo) = kWeights(layerNo).head.head.head.length
//  }

  /* Shapes */
//  val inputShape: Array[Shape] = Array.fill[Shape](nLayers)(Shape())
  var outputShape: Array[Shape] = Array.fill[Shape](nLayers)(Shape())
//  inputShape(0) = Shape(w = inputsL0.nonPadded.head.head.length, h = inputsL0.nonPadded.head.head.head.length,
//    ch = inputsL0.nonPadded.head.head.head.head.length)
//  var kernelShape: Array[Shape] = {for (layerNo <- 0 until nLayers) yield
//    Shape(w = kWeights(layerNo).head.length, h = kWeights(layerNo).length)}.toArray

  var kernelStep: Array[Int] = Array.fill[Int](nLayers)(1)


  /* Tiles */
  var inputTileSize: Array[Int] = {for (layerNo <- 0 until nLayers) yield
    //if (layerNo == 0) inputTileSizeL0 else kernelShape(layerNo).s}.toArray
    if (layerNo == 0 && nLayers > 1) kernelShape(layerNo).s else inputTileSizeL1 }.toArray
  var inputTileStep: Array[Int] = {for (layerNo <- 0 until nLayers) yield
    inputTileSize(layerNo) - (kernelShape(layerNo).s - kernelStep(layerNo))}.toArray
  var nTilesPerDim: Array[Int] = Array.fill[Int](nLayers)(0)
  var nWindowsPerTilePerDim: Array[Int] = Array.fill[Int](nLayers)(0)


  for (layerNo <- 0 until nLayers) {
    /* Input shape */
    if (layerNo != 0)
      inputShape(layerNo) = outputShape(layerNo - 1).copy()

    /* Padding */
    // Calculate how much padding is required
    nTilesPerDim(layerNo) = 1 + Math.ceil((inputShape(layerNo).s - inputTileSize(layerNo)).toFloat /
      inputTileStep(layerNo)).toInt
    inputShape(layerNo).wPadded = inputTileSize(layerNo) + inputTileStep(layerNo) * (nTilesPerDim(layerNo) - 1)
    inputShape(layerNo).hPadded = inputShape(layerNo).wPadded
    nWindowsPerTilePerDim(layerNo) = {
      val n: Float = (inputTileSize(layerNo) - (kernelShape(layerNo).s - kernelStep(layerNo))).toFloat / kernelStep(layerNo)
      if (n % 1 != 0) throw new java.lang.IllegalArgumentException("Input tiles are not divisible by the chosen " +
        "kernelShape and kernelStep")
      n.toInt
    }

    /* Output shape */
    outputShape(layerNo) = Shape(
      w={val w: Float = (inputShape(layerNo).w - (kernelShape(layerNo).w - kernelStep(layerNo))).toFloat /
        kernelStep(layerNo)
        if (w % 1 != 0) throw new java.lang.IllegalArgumentException("Inputs are not divisible by the chosen " +
          "kernelShape and kernelStep")
        w.toInt
      },
      h={val h: Float = (inputShape(layerNo).h - (kernelShape(layerNo).h - kernelStep(layerNo))).toFloat /
        kernelStep(layerNo)
        if (h % 1 != 0) throw new java.lang.IllegalArgumentException("Inputs are not divisible by the chosen " +
          "kernelShape and kernelStep")
        h.toInt
      },
      ch=nKernels(layerNo),
      wPadded=((inputShape(layerNo).wPadded - (kernelShape(layerNo).w - kernelStep(layerNo))).toFloat /
        kernelStep(layerNo)).toInt,
      hPadded=((inputShape(layerNo).hPadded - (kernelShape(layerNo).h - kernelStep(layerNo))).toFloat /
        kernelStep(layerNo)).toInt)
  }


  /* Execution runtimes */
  val runTimes: Array[Double] = Array.fill[Double](nLayers)(0)


  /* Parallelization parameters */
  val elsPerThread: Array[Int] = Array.fill[Int](nLayers)(1)
  elsPerThread(nLayers - 1) = elsPerThreadL1
  val kernelsPerGroup: Array[Int] = Array.fill[Int](nLayers)(4)
  kernelsPerGroup(nLayers - 1) = kernelsPerGroupL1
  val localSize: Array[Array[Int]] = Array.fill[Array[Int]](3)(Array.fill[Int](nLayers)(0))
  val globalSize: Array[Array[Int]] = Array.fill[Array[Int]](3)(Array.fill[Int](nLayers)(0))

  for (layerNo <- 0 until nLayers) {
    /* Check parameters */
    if (nKernels(layerNo) % kernelsPerGroup(layerNo) != 0)
      throw new java.lang.IllegalArgumentException(
        f"The number of kernels in layer $layerNo%d (=${nKernels(layerNo)}%d) must be divisible by kernelsPerGroup " +
          f"(=${kernelsPerGroup(layerNo)}%d)")
    if (kernelShape(layerNo).s % elsPerThread(layerNo) != 0)
      throw new java.lang.IllegalArgumentException(
        f"Kernel size in all dimensions (=${kernelShape(layerNo).s}%d) must be divisible by elsPerThread " +
          f"(=${elsPerThread(layerNo)}%d)")


    /* Compute local sizes */
    // Local size 0 =
    localSize(0)(layerNo) = (kernelsPerGroup(layerNo) *
      Math.ceil(kernelShape(layerNo).s.toFloat / elsPerThread(layerNo))).toInt

    // Local size 1 =
    localSize(1)(layerNo) = scala.math.pow((inputTileSize(layerNo) - (kernelShape(layerNo).s - 1)) /
      kernelStep(layerNo), 2).toInt

    // Local size 2 = height of the convolutional window
    localSize(2)(layerNo) = kernelShape(layerNo).h

    {
      val groupSize: Int = localSize(0)(layerNo) * localSize(1)(layerNo) * localSize(2)(layerNo)
        if (groupSize > nn.maxWorkGroupSize)
        throw new java.lang.IllegalArgumentException(
          f"Layer $layerNo%d group size (==$groupSize%d) must be less or equal to maxWorkGroupSize " +
            f"(${nn.maxWorkGroupSize}%d).\nDecrease nKernels or inputTileSize or increase elsPerThread " +
            f"(${elsPerThread(layerNo)}%d).")
    }

    /* Compute global sizes */
    globalSize(0)(layerNo) = localSize(0)(layerNo) * nInputs * nTilesPerDim(layerNo) * nTilesPerDim(layerNo)
    globalSize(1)(layerNo) = localSize(1)(layerNo) * nBatches
    globalSize(2)(layerNo) = localSize(2)(layerNo) *
      Math.ceil(nKernels(layerNo).toFloat / kernelsPerGroup(layerNo)).toInt
  }

  /* Data: if all checks above succeeded, load the data */
  var outputs: Array[PaddedArray[Array5D[Float]]] = _
  var inputs: Array[PaddedArray[Array5D[Float]]] = _
  var (inputsL0: PaddedArray[Array5D[Float]], kWeights: Array5D[Float],
       kBiases: Array2D[Float], targets: Array5D[Float]) = {
    if (previousCNN != null && previousCNN.nInputs == nInputs && previousCNN.pathToResults == pathToResults)
      // Avoid loading the same data
      (previousCNN.inputsL0, previousCNN.kWeights, previousCNN.kBiases, previousCNN.targets)
    else
      loadDatasets(nInputs, pathToInputs)
  }

  def updateInputs(layerNo: Int): Unit = if (inputs == null)
    inputs = Array(inputsL0) else inputs = inputs :+ outputs(layerNo - 1)


  /* Padding */
  def padInputs(layerNo: Int): Unit = {
    inputs(layerNo).padded =
      Array.fill[Array4D[Float]](nBatches)(
        Array.fill[Array3D[Float]](nInputs)(
          Array.fill[Array2D[Float]](inputShape(layerNo).hPadded)(
            Array.fill[Array[Float]](inputShape(layerNo).wPadded)(
              Array.fill[Float](inputShape(layerNo).ch)(0)))))
    // Add empty lines
    for {b <- 0 until nBatches; i <- 0 until nInputs}
      inputs(layerNo).padded(b)(i) = inputs(layerNo).nonPadded(b)(i).padTo(
        inputShape(layerNo).hPadded,
        Array.fill[Array[Float]](inputShape(layerNo).wPadded)(
          Array.fill[Float](inputShape(layerNo).ch)(0)))
    // Add empty elements to lines
    for {b <- 0 until nBatches; i <- 0 until nInputs; h <- 0 until inputShape(layerNo).hNonPadded}
      inputs(layerNo).padded(b)(i)(h) = inputs(layerNo).nonPadded(b)(i)(h).padTo(
        inputShape(layerNo).wPadded,
        Array.fill[Float](inputShape(layerNo).ch)(0))
  }
}
