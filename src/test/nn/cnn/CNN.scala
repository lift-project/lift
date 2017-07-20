package nn.cnn

/**
  * Created by nm on 09/01/17.
  */

import ir.ast.{FunDecl, Get, Join, Slide2D, Split, TiledSlidedND, Transpose, TransposeW, Tuple, UserFun, Zip, λ}
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
  /*def ParNonTiled(activation_f: UserFun, kernel_shape: Shape, output_shape: Shape, n_output_channels: Int,
          tile: Tile): FunDecl = λ(
    ArrayType(ArrayType(ArrayType(ArrayType(Float,
      n_output_channels), input_channels), kernel_shape.w), kernel_shape.h),
    ArrayType(Float, n_output_channels),
    ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float,
      input_channels), input_xdim), input_ydim), n_inputs), n_batches),
    (K, B, X) => {
      MapWrg(1)(λ((input_batch) => {
        MapWrg(0)(λ((input_single) => {
          MapWrg(2)(λ((input_single_wrapped) => {
            MapLcl(1)(λ((pass_window) => {
              Join() o
                MapSeq(λ((weighted_window_per_out_ch, b_per_out_ch) => {
                  // Reduce weighted pass window separately for each output
                  MapSeq(toGlobal(activation_f)) o ReduceSeq(add, id(b_per_out_ch)) $ weighted_window_per_out_ch
                })) o λ((weighted_window_across_out_chs) => Zip(weighted_window_across_out_chs, B)) o Transpose() o
                MapLcl(2)(λ((window_row, kernel_row) => {
                  Join() o
                    MapSeq(λ(ArrayType(Float, n_output_channels),
                      (weighted_row_per_out_ch) => {
                        // Reduce weighted pass window rows separately for each output
                        MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) $ weighted_row_per_out_ch
                      })) o  Transpose() o
                    /*out_chs->els*/Split(n_output_channels) o  Join(/*tiles*/) o
                    MapLcl(0)(λ(/*tiles*/ArrayType(/*out_chs*/ArrayType(
                      TupleType(/*x_el_in_ch*/Float, /*k_el_in_ch*/Float),
                      input_channels), tile.els_per_thread),
                      (tile_of_out_chs) => {
                        /* Map below returns (n_out_chs, n_els) */
                        Join() o
                          MapSeq(λ((out_ch) => {
                            MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) o
                              MapSeq(λ(TupleType(Float/*x_el_in_ch*/, Float/*k_el_in_ch*/),
                                (el_in_ch) =>
                                  mult(/*x_el_in_chs*/ Get(el_in_ch, 0), /*k_el_in_ch*/ Get(el_in_ch, 1)))) $ out_ch
                          })) o PrintType() $ tile_of_out_chs
                      })) o Split(tile.els_per_thread) o Join(/*rows->els*/) o
                    // Transpose: (n_in_chs, n_out_chs) -> (n_out_chs, n_in_chs)
                    // (x_el_in_chs0, (k_el_out_ch00, k_el_out_ch01)) ->
                    // ((x_el_in_chs0, k_el_out_ch00), (x_el_in_chs0, k_el_out_ch01))
                    Map(λ(TupleType(ArrayType(Float, input_channels), /*x_el_in_chs*/
                      ArrayType(ArrayType(Float, n_output_channels), input_channels)/*k_el_in_chs*/),
                      (el_in_chs) =>
                        Map(λ(ArrayType(Float, input_channels), (k_el_out_ch) =>
                          Zip(/*x_el_in_chs*/Get(el_in_chs, 0), k_el_out_ch))) o Transpose() $
                          /*k_el_in_chs*/Get(el_in_chs, 1)
                    )) $ Zip(window_row, kernel_row)
                })) $ Zip(pass_window, K)
            })) o /* (n_passes, n_windows, n_rows) -> (n_passes*n_windows, n_rows) */
              Join() $ input_single_wrapped
          })) o Split(output_shape.h) $ input_single // Wrap input in a one-element array
        })) $ input_batch
      })) o MapSeq(MapSeq(Slide2D(kernel_shape.h, 1, kernel_shape.w, 1))) $ X
    }
  )*/

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
          /*  (nInputTiles, n_k_passes, n_k_windows, nKernels) ->
          *   (n_tile_passes, n_tile_windows, nKernels) */
          MapSeq(/*input*/Join() o MapSeq(/*tile_row*/MapSeq(Join()) o TransposeW())) o Split(tile.nInputTilesPerDim) o
          Split(tile.nInputTilesPerDim) o
          /*  (nInputTiles(nImages * nInputTilesPerDim * nInputTilesPerDim), nKernels, n_k_passes, n_k_windows) ->
           *  (nInputTiles(nImages * nInputTilesPerDim * nInputTilesPerDim), n_k_passes, n_k_windows, nKernels) */
          λ(AT(AT(AT(AT(Float, tile.n_windows_per_tile_per_dim), tile.n_windows_per_tile_per_dim),
            n_out_channels), n_inputs * tile.nInputTilesPerDim * tile.nInputTilesPerDim),
            (tiled_outputs) => MapSeq(MapSeq(TransposeW()) o TransposeW()) $ tiled_outputs) o
          MapWrg(0)(λ((input_tile) => {
            /* (nKernels / tile.kernels_per_group, tile.kernels_per_group, n_k_passes, n_k_windows) ->
            *  (nKernels, n_k_passes, n_k_windows) */
            Join() o MapWrg(2)(λ(TupleType(
              AT(AT(AT(AT(Float, n_in_channels), kernel_shape.w), kernel_shape.h), tile.kernels_per_group),
              AT(Float, tile.kernels_per_group)),
              (kernels_tile) => {
                /* (tile.kernels_per_group, n_passes*n_windows) -> (tile.kernels_per_group, n_k_passes, n_k_windows) */
                MapSeq(Split(tile.n_windows_per_tile_per_dim)) o
                /* (n_passes*n_windows, tile.kernels_per_group) -> (tile.kernels_per_group, n_k_passes*n_k_windows) */
                TransposeW() o
                MapLcl(1)(λ((pass_window) => {
                  λ(AT(AT(Float, kernel_shape.h), tile.kernels_per_group), (partially_reduced_window) =>
                    ReduceWindowAndAddBias()(partially_reduced_window, /* biases */Get(kernels_tile, 1))) o
                  /* (kernel_shape.h, tile.kernels_per_group) -> (tile.kernels_per_group, kernel_shape.h) */
                  TransposeW() o
                  MapLcl(2)(λ((window_row, kernels_row) => {
                    Join() o MapSeq(ReduceRow()) o
                    // (tile.kernels_per_group, kernel_shape.w)
                      MapSeq(Join(/*tiles of elements*/)/* o
                        MapSeq(/* Dissolve one-element output of Reduce */Join())*/) o
                      Split(kernel_shape.w / tile.els_per_thread) o
                      MapLcl(0)(WeightedSumOfInputChannels()) o
    /* (tile.kernels_per_group, kernel_shape.w / tile.els_per_thread, tile.els_per_thread, tuple of n_in_channels) ->
     * (tile.kernels_per_group * kernel_shape.w / tile.els_per_thread, tile.els_per_thread, tuple of n_in_channels)*/
                      Join() o
      /* (tile.kernels_per_group, kernel_shape.w, n_in_channels) ->
       * (tile.kernels_per_group, kernel_shape.w / tile.els_per_thread, tile.els_per_thread, tuple of n_in_channels) */
                      MapSeq(/* for each kernel */
                        λ((kernel_row) => Split(tile.els_per_thread) $ Zip(window_row, kernel_row))) o
                      /* (kernel_shape.w, n_in_channels, tile.kernels_per_group) ->
                       * (tile.kernels_per_group, kernel_shape.w, n_in_channels) */
                      Transpose() o MapSeq(Transpose()) $ kernels_row
                  })) $ Zip(pass_window, RestoreKernelShape() $ /* weights */ Get(kernels_tile, 0))
                })) o toLocal(MapLcl(1)(λ((pass_window) =>
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
     * AT(AT(AT(AT(AT(AT(AT(Float, input_channels), input_xdim), input_ydim), n_kernel_pass_windows_in_tile),
     * n_kernel_pass_strips_in_tile), n_tile_pass_windows * n_tile_pass_strips * n_inputs), n_batches) */
    def SlideX(): FunDecl =
      λ(AT(AT(AT(AT(AT(Float, n_in_channels), input_shape.wPadded), input_shape.hPadded), n_inputs), n_batches), (X) =>
        MapSeq(Join() o MapSeq(Join() o TiledSlidedND(2)(kernel_shape.s, 1, tile.inputTileSlideStep))) $ X)

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
          Transpose() o MapSeq(Transpose() o MapSeq(Transpose())) $ kernels,
          Split(tile.kernels_per_group) $ biases))

    /* Reshapes the kernel back to the original shape, where output channels are the lowest dimension
     * of the tensor.
     * TODO: get rid of this - there is no advantage of having this
     * Returns:
     * AT(AT(AT(AT(Float, tile.kernels_per_group), input_channels), kernel_shape.w), kernel_shape.h) */
    def RestoreKernelShape(): FunDecl =
      λ(AT(AT(AT(AT(Float, n_in_channels), kernel_shape.w), kernel_shape.h), tile.kernels_per_group),
        (kernels_tile) =>
        MapSeq(MapSeq(Transpose()) o Transpose()) o Transpose() $ kernels_tile)

    /* Computes a weighted sum of all input channels of one element for one output channel.
     * Returns:
     * TODO */
    def WeightedSumOfInputChannels(): FunDecl =
      λ(AT(TupleType(
            AT(Float, n_in_channels),
            AT(Float, n_in_channels)),
          tile.els_per_thread),
        (tile_of_els) => {
        MapSeq(toGlobal(id)) o ReduceSeq(add, toPrivate(id) $ 0.0f) o Join() o
        MapSeq(λ(TupleType(AT(Float, n_in_channels), /*x_el_in_chs*/
          AT(AT(Float, tile.kernels_per_group), n_in_channels) /*k_el_in_chs*/),
          (single_element) =>
            /*Join() o*/
            /*MapSeq(toGlobal(id)) o */ReduceSeq(add, toPrivate(id) $ 0.0f) o
              MapSeq(λ(TupleType(Float /*x_el_in_ch*/ , Float /*k_el_in_ch*/),
                (el_in_ch) =>
                  mult(toPrivate(id) $ /*x_el_in_chs*/ Get(el_in_ch, 0), toPrivate(id) $ /*k_el_in_ch*/ Get(el_in_ch, 1)))) $
              Zip(Get(single_element, 0), Get(single_element, 1))
        )) $ tile_of_els
      })

    /* Reduces weighted pass window rows for one channel.
     * Returns:
     * AT(Float, 1) */
    def ReduceRow(): FunDecl =
      λ(AT(Float, kernel_shape.w / tile.els_per_thread),
        (weighted_row_per_out_ch) => {
          MapSeq(toGlobal(id)) o ReduceSeq(add, 0.0f) $ weighted_row_per_out_ch
      })

    /* Reduces weighted pass windows for each channel.
     * Returns:
     * AT(Float, tile.kernels_per_group) */
    def ReduceWindowAndAddBias(): FunDecl =
      λ(AT(AT(Float, kernel_shape.h), tile.kernels_per_group),
        AT(Float, tile.kernels_per_group),
        (partially_reduced_windows, biases) => {
          Join() o
          MapSeq(λ((reduced_rows, bias) => {
            // Reduce weighted pass window separately for each output channel
            MapSeq(toGlobal(activation_f)) o ReduceSeq(add, id(bias)) $ reduced_rows
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
          val pathToResults: String) {
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


  /* Data storage */
  var outputs: Array[PaddedArray[Array5D[Float]]] = _
  var inputs: Array[PaddedArray[Array5D[Float]]] = _
  var inputsL0: PaddedArray[Array5D[Float]] = _
  var kWeights: Array5D[Float] = _
  var kBiases: Array2D[Float] = _
  var targets: Array5D[Float] = _
  def setData(data: (PaddedArray[Array5D[Float]], Array5D[Float], Array2D[Float], Array5D[Float])): Unit = {
    // TODO: reformat this ugliness
    inputsL0 = data._1
    kWeights = data._2
    kBiases = data._3
    targets = data._4
  }
  def updateInputs(layerNo: Int): Unit = if (inputs == null)
    inputs = Array(inputsL0) else inputs = inputs :+ outputs(layerNo - 1)


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
    /* For each possible tile of kernels per work group, there is the best possible image tile size.
     * tileConfigs contains the list of kernel tile sizes -> image tile size mappings. */
    /*val tileConfigs: List[(Int, Int, Int)] = {
      val kSize: Int = kernelShape(layerNo).s
      val kStep: Int = 1
      val e: Int = elsPerThread(layerNo)
      val m: Int = nn.maxWorkGroupSize
      for (nKernels <- (1 to nKernels(layerNo)).toList) yield {
        val tileSize: Int = Math.max(kSize,
          Math.floor(Math.sqrt(m / (Math.ceil(kSize * nKernels / e) * kSize)) + kSize - 1).toInt)
        // TODO: check if nonPadded or padded should be used here
        val tileStep: Int = tileSize - (kSize - kStep)
        val outerLength: Int = Math.ceil((inputShape.s - (tileSize - tileStep)) / tileStep).toInt
        val numberOfGroups: Int = nKernels * outerLength
        tileSize, tileStep, numberOfGroups
      }
    }*/
    /* Find the kernel tile size, which with corresponding image tile size results in the least total number of groups */
    //val nKernelsPerWrg = tileConfigs.zipWithIndex.minBy(_._1._3)._2
    //slideTileStep(layerNo) =

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
        // TODO: replace with IllegalArgumentException
        throw new java.lang.IllegalArgumentException(
          f"Layer $layerNo%d group size (==$groupSize%d) must be less or equal to maxWorkGroupSize " +
            f"(${nn.maxWorkGroupSize}%d).\nDecrease nKernels or inputTileSize or increase elsPerThread " +
            f"(${elsPerThread(layerNo)}%d).")
    }

    /* Compute global sizes */
    globalSize(0)(layerNo) = localSize(0)(layerNo) * nInputs
    globalSize(1)(layerNo) = localSize(1)(layerNo) * nBatches
    globalSize(2)(layerNo) = localSize(2)(layerNo) *
      Math.ceil(nKernels(layerNo).toFloat / kernelsPerGroup(layerNo)).toInt
  }
}
