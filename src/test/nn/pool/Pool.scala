package nn.pool

/**
  * Created by nm on 09/01/17.
  */

import ir.ast.debug.PrintType
import ir.ast.{λ, _}
import ir.{ArrayType, TupleType}
import lift.arithmetic.SizeVar
import nn._
import opencl.ir._
import opencl.ir.pattern._

object Pool {
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

  /* Parallel layer */
  def Par(activation_f: UserFun, input_shape: Shape, kernel_shape: Shape, kernel_stride: Int,
          n_inputs: Int, n_batches: Int, n_in_channels: Int, tile: Tile): FunDecl = {
    def AT = ArrayType // alias

    def Layer: FunDecl = λ(
      AT(AT(AT(AT(AT(Float, n_in_channels), input_shape.size), input_shape.size), n_inputs), n_batches),
      (X) => {
        MapWrg(0)(λ((inputs_batch) => {
          /*  (nImages, nInputTilesPerDim, nInputTilesPerDim, n_k_passes, n_k_windows) ->
              (nImages, nInputTilesPerDim, n_k_passes, nInputTilesPerDim, n_k_windows) ->
              (nImages, nInputTilesPerDim, n_k_passes, nInputTilesPerDim * n_k_windows) ->
              (nImages, nInputTilesPerDim * n_k_passes, nInputTilesPerDim * n_k_windows) ->
          *   (nImages, n_passes, n_windows) */
          Map(/*input*/Join() o Map(/*tile_row*/Map(Join()) o TransposeW())) o
          Split(tile.n_input_tiles_per_dim) o Split(tile.n_input_tiles_per_dim) o
          /*  (nImages * nInputTilesPerDim * nInputTilesPerDim, n_k_passes, n_k_windows) ->
           *  (nImages * nInputTilesPerDim * nInputTilesPerDim, n_k_passes, n_k_windows) */
          MapWrg(1)(λ(AT(AT(AT(AT(AT(Float, n_in_channels), kernel_shape.size), kernel_shape.size),
            tile.n_kwindows_per_tile_per_dim), tile.n_kwindows_per_tile_per_dim),
            (input_tile_to_wrap) => {
            Join() o MapWrg(2)(λ(AT(AT(AT(AT(AT(Float, n_in_channels), kernel_shape.size), kernel_shape.size),
              tile.n_kwindows_per_tile_per_dim), tile.n_kwindows_per_tile_per_dim),
              (input_tile) => {
                /* (n_k_passes * n_k_windows) -> (n_k_passes, n_k_windows) */
                Split(tile.n_kwindows_per_tile_per_dim) o
                MapLcl(0)(λ((pass_window) => { Join() o
                  MapLcl(1)(λ((input_channel) => { Join() o {
                    if (tile.els_per_thread < kernel_shape.size * kernel_shape.size) {
                      /* If window has only been partially reduced until now */
                      MapLcl(2)(λ((partially_reduced_window) => {
                        MapSeq(toGlobal(id)) o ReduceSeq(activation_f, 0.0f) $ partially_reduced_window
                      })) o /* Wrap data into an array of 1 element */
                      Split(kernel_shape.size * kernel_shape.size / tile.els_per_thread)
                    } else {
                      /* Window is fully reduced, so we only need to copy the result into global memory */
                      MapLcl(2)(λ((fully_reduced_window) => MapSeq(toGlobal(id)) $ fully_reduced_window)) o
                        /* Wrap data into an array of 1 element */
                        Split(kernel_shape.size * kernel_shape.size / tile.els_per_thread)
                    }
                  } o Join() o
                  MapLcl(2)(λ(AT(Float, tile.els_per_thread), (tile_of_els) => {
                    MapSeq(toLocal(id)) o ReduceSeq(activation_f, id $ 0.0f) $ tile_of_els})) o
                  PrintType("maplcl(2)") o
/* (kernel_shape.s * kernel_shape.s) ->
 * (kernel_shape.s * kernel_shape.s / tile.els_per_thread, tile.els_per_thread) */
                    Split(tile.els_per_thread) $ input_channel
                  })) o PrintType("maplcl(1)") o Transpose() o Join() $ pass_window
                })) o PrintType("maplcl(0)") o toLocal(MapLcl(0)(λ((pass_window) =>
                MapLcl(1)(λ((window_row) => {
                  MapSeq(MapSeq(id)) $ window_row
                })) $ pass_window))) o
                  /* (n_passes, n_windows, n_rows) -> (n_passes*n_windows, n_rows) */
                  Join() $ input_tile
              })) o /* Wrap into an array of 1 element */
              Split(tile.n_kwindows_per_tile_per_dim) $ input_tile_to_wrap
          })) $ inputs_batch
        })) o SlideX() $ X
      }
    )

    /* Produces a tiled slided version of X.
     * Returns:
     * AT(AT(AT(AT(AT(AT(AT(Float, input_channels), kernel_shape.s), kernel_shape.s), n_kernel_pass_windows_in_tile),
     * n_kernel_pass_strips_in_tile), n_tile_pass_windows * n_tile_pass_strips * n_inputs), n_batches) */
    def SlideX(): FunDecl =
      λ(AT(AT(AT(AT(AT(Float, n_in_channels), input_shape.sizePadded), input_shape.sizePadded), n_inputs), n_batches), (X) =>
        Map(Join() o Map(Join() o TiledSlidedND(2)(kernel_shape.size, kernel_stride, tile.input_tile_stride))) $ X)

    /* Computes a max of elsPerThread elements for each input channel separately
     * Returns:
     * AT(Float, 1) */
    def PartiallyReduceWindow(): FunDecl =
      λ(AT(Float, tile.els_per_thread), (tile_of_els) => {
        MapSeq(toLocal(id)) o ReduceSeq(activation_f, id $ 0.0f) $ tile_of_els})

    /* Computes a max of all elements in the kernel window.
     * NB: Rows are already partially reduced by the factor of els_per_thread in PartiallyReduceWindow()
     * Returns:
     * AT(Float, 1) */
    def ReduceWindow(): FunDecl =
      λ(AT(Float, kernel_shape.size * kernel_shape.size / tile.els_per_thread),
        (partially_reduced_row_to_wrap) => {
        Join() o MapLcl(2)(λ((partially_reduced_row) => {
          MapSeq(toGlobal(id)) o ReduceSeq(activation_f, 0.0f) $ partially_reduced_row
        })) o /* Wrap data into an array of 1 element */
        Split(kernel_shape.size * kernel_shape.size / tile.els_per_thread) $ partially_reduced_row_to_wrap
      })

    Layer
  }
}

class Pool(/* Pool architectural parameters */
           val liftPool: (UserFun, Shape, Shape, Int, Int, Int, Int, Tile) => FunDecl,
           val activationFun/*per layer*/: Array[UserFun],
           /* Pool parallelisation parameters */
           val elsPerThread: Array[Int],
           val inputTileSize: Array[Int],
           /* Pool application parameters */
           val nLayers: Int, val nBatches: Int, val nInputs: Int,
           val nInChannels: Array[Int],
           val inputShape: Array[Shape], val kernelShape: Array[Shape], val kernelStride: Array[Int],
           val pathToInputs: String, val pathToResults: String,
           val loadDatasets: (Int, String) => (PaddedArray[Array5D[Float]], Array5D[Float]),
           val previousPool: Pool) {
  /**
  * Initializes variables, computes workgroup sizes.
  */
  var outputShape: Array[Shape] = Array.fill[Shape](nLayers)(Shape())

  /* Tiles */
  var inputTileStep: Array[Int] = {for (layerNo <- 0 until nLayers) yield
    inputTileSize(layerNo) - (kernelShape(layerNo).size - kernelStride(layerNo))}.toArray
  var nTilesPerDim: Array[Int] = Array.fill[Int](nLayers)(0)
  var nWindowsPerTilePerDim: Array[Int] = Array.fill[Int](nLayers)(0)


  for (layerNo <- 0 until nLayers) {
    /* Input shape */
    if (layerNo != 0)
      inputShape(layerNo) = outputShape(layerNo - 1).copy()

    /* Padding */
    // Calculate how much padding is required
    nTilesPerDim(layerNo) = 1 + Math.ceil((inputShape(layerNo).size - inputTileSize(layerNo)).toFloat /
      inputTileStep(layerNo)).toInt
    inputShape(layerNo).sizePadded = inputTileSize(layerNo) + inputTileStep(layerNo) * (nTilesPerDim(layerNo) - 1)
    inputShape(layerNo).sizePadded = inputShape(layerNo).sizePadded
    nWindowsPerTilePerDim(layerNo) = {
      val n: Float = (inputTileSize(layerNo) -
        (kernelShape(layerNo).size - kernelStride(layerNo))).toFloat / kernelStride(layerNo)
      if (n % 1 != 0) throw new java.lang.IllegalArgumentException(
        "Input tiles are not divisible by the chosen kernelShape and kernelStride")
      n.toInt
    }

    /* Output shape */
    outputShape(layerNo) = Shape(
      size={val w: Float = (inputShape(layerNo).size - (kernelShape(layerNo).size - kernelStride(layerNo))).toFloat /
        kernelStride(layerNo)
        if (w % 1 != 0) throw new java.lang.IllegalArgumentException(
          "Inputs are not divisible by the chosen kernelShape and kernelStride")
        w.toInt
      },
      size={val h: Float = (inputShape(layerNo).size - (kernelShape(layerNo).size - kernelStride(layerNo))).toFloat /
        kernelStride(layerNo)
        if (h % 1 != 0) throw new java.lang.IllegalArgumentException(
          "Inputs are not divisible by the chosen kernelShape and kernelStride")
        h.toInt
      },
      nChannels=inputShape(layerNo).nChannels,
      sizePadded=((inputShape(layerNo).sizePadded - (kernelShape(layerNo).size - kernelStride(layerNo))).toFloat /
        kernelStride(layerNo)).toInt,
      sizePadded=((inputShape(layerNo).sizePadded - (kernelShape(layerNo).size - kernelStride(layerNo))).toFloat /
        kernelStride(layerNo)).toInt)
  }


  /* Execution runtimes */
  val runTimes: Array[Double] = Array.fill[Double](nLayers)(0)


  /* Parallelization parameters */
  val localSize: Array[Array[Int]] = Array.fill[Array[Int]](3)(Array.fill[Int](nLayers)(0))
  val globalSize: Array[Array[Int]] = Array.fill[Array[Int]](3)(Array.fill[Int](nLayers)(0))

  for (layerNo <- 0 until nLayers) {
    /* Check parameters */
    if (inputShape(layerNo).size < kernelShape(layerNo).size)
      throw new java.lang.IllegalArgumentException(
        f"The kernel size in layer $layerNo%d (=${kernelShape(layerNo).size}%d) must be smaller than the image size " +
        f"(=${inputShape(layerNo).size}%d)")

    if (kernelShape(layerNo).size % elsPerThread(layerNo) != 0)
      throw new java.lang.IllegalArgumentException(
        f"Kernel size in all dimensions (=${kernelShape(layerNo).size}%d) must be divisible by elsPerThread " +
        f"(=${elsPerThread(layerNo)}%d)")


    /* Compute local sizes */

    // Local size 0
    localSize(0)(layerNo) = scala.math.pow(nWindowsPerTilePerDim(layerNo), 2).toInt

    // Local size 1
    localSize(1)(layerNo) = inputShape(layerNo).size

    // Local size 2
    localSize(2)(layerNo) = Math.ceil(scala.math.pow(kernelShape(layerNo).size, 2).toFloat / elsPerThread(layerNo)).toInt

    {
      val groupSize: Int = localSize(0)(layerNo) * localSize(1)(layerNo) * localSize(2)(layerNo)
        if (groupSize > nn.maxWorkGroupSize)
        throw new java.lang.IllegalArgumentException(
          f"Layer $layerNo%d group size (==$groupSize%d) must be less or equal to maxWorkGroupSize " +
            f"(${nn.maxWorkGroupSize}%d).\nDecrease inputTileSize or increase elsPerThread " +
            f"(${elsPerThread(layerNo)}%d).")
    }

    /* Compute global sizes */
    globalSize(0)(layerNo) = localSize(0)(layerNo) * nBatches
    globalSize(1)(layerNo) = localSize(1)(layerNo) * nInputs * nTilesPerDim(layerNo) * nTilesPerDim(layerNo)
    globalSize(2)(layerNo) = localSize(2)(layerNo) * 1
  }

  /* Data: if all checks above succeeded, load the data */
  var outputs: Array[PaddedArray[Array5D[Float]]] = _
  var inputs: Array[PaddedArray[Array5D[Float]]] = _
  var (inputsL0: PaddedArray[Array5D[Float]], targets: Array5D[Float]) = {
    if (previousPool != null && previousPool.nInputs == nInputs && previousPool.pathToResults == pathToResults)
      // Avoid loading the same data
      (previousPool.inputsL0, previousPool.targets)
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
          Array.fill[Array2D[Float]](inputShape(layerNo).sizePadded)(
            Array.fill[Array[Float]](inputShape(layerNo).sizePadded)(
              Array.fill[Float](inputShape(layerNo).nChannels)(0)))))
    // Add empty lines
    for {b <- 0 until nBatches; i <- 0 until nInputs}
      inputs(layerNo).padded(b)(i) = inputs(layerNo).nonPadded(b)(i).padTo(
        inputShape(layerNo).sizePadded,
        Array.fill[Array[Float]](inputShape(layerNo).sizePadded)(
          Array.fill[Float](inputShape(layerNo).nChannels)(0)))
    // Add empty elements to lines
    for {b <- 0 until nBatches; i <- 0 until nInputs; h <- 0 until inputShape(layerNo).hNonPadded}
      inputs(layerNo).padded(b)(i)(h) = inputs(layerNo).nonPadded(b)(i)(h).padTo(
        inputShape(layerNo).sizePadded,
        Array.fill[Float](inputShape(layerNo).nChannels)(0))
  }
}
