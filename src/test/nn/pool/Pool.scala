package nn.pool

/**
  * Created by nm on 09/01/17.
  */

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
      AT(AT(AT(AT(AT(Float, n_in_channels), kernel_shape.s), kernel_shape.s), n_inputs), n_batches),
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
          MapWrg(1)(λ(AT(AT(AT(AT(AT(Float, n_in_channels), kernel_shape.s), kernel_shape.s),
            tile.n_kwindows_per_tile_per_dim), tile.n_kwindows_per_tile_per_dim),
            (input_tile_to_wrap) => {
            Join() o MapWrg(2)(λ(AT(AT(AT(AT(AT(Float, n_in_channels), kernel_shape.s), kernel_shape.s),
              tile.n_kwindows_per_tile_per_dim), tile.n_kwindows_per_tile_per_dim),
              (input_tile) => {
                /* (n_k_passes * n_k_windows) -> (n_k_passes, n_k_windows) */
                Split(tile.n_kwindows_per_tile_per_dim) o
                MapLcl(0)(λ((pass_window) => {
                  Join() o MapLcl(1)(λ((input_channel) => {
                    ReduceWindow() o Join() o MapLcl(2)(PartiallyReduceWindow()) o
/* (kernel_shape.s * kernel_shape.s) ->
 * (kernel_shape.s * kernel_shape.s / tile.els_per_thread, tile.els_per_thread) */
                    Split(tile.els_per_thread) $ input_channel
                  })) o Transpose() o Join() $ pass_window
                })) o toLocal(MapLcl(0)(λ((pass_window) =>
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
      λ(AT(AT(AT(AT(AT(Float, n_in_channels), input_shape.wPadded), input_shape.hPadded), n_inputs), n_batches), (X) =>
        Map(Join() o Map(Join() o TiledSlidedND(2)(kernel_shape.s, kernel_stride, tile.input_tile_stride))) $ X)

    /* Computes a max of elsPerThread elements for each input channel separately
     * Returns:
     * AT(Float, 1) */
    def PartiallyReduceWindow(): FunDecl =
      λ(AT(Float, tile.els_per_thread), (tile_of_els) => {
        MapSeq(toGlobal(id)) o ReduceSeq(activation_f, toPrivate(id) $ 0.0f) $ tile_of_els})

    /* Computes a max of all elements in the kernel window.
     * NB: Rows are already partially reduced by the factor of els_per_thread in PartiallyReduceWindow()
     * Returns:
     * AT(Float, 1) */
    def ReduceWindow(): FunDecl =
      λ(AT(Float, kernel_shape.s * kernel_shape.s / tile.els_per_thread),
        (partially_reduced_row_to_wrap) => {
        Join() o MapLcl(2)(λ((partially_reduced_row) => {
          MapSeq(toGlobal(id)) o ReduceSeq(activation_f, 0.0f) $ partially_reduced_row
        })) o /* Wrap data into an array of 1 element */
        Split(kernel_shape.s * kernel_shape.s / tile.els_per_thread) $ partially_reduced_row_to_wrap
      })

    Layer
  }
}

class Pool(/* Pool architectural parameters */
           val liftPool: (UserFun, Shape, Shape, Int, Int, Int, Int, Tile) => FunDecl,
           val activationFun/*per layer*/: Array[UserFun],
           /* Pool parallelisation parameters */
           val elsPerThreadL1: Int,
           val inputTileSizeL1: Int,
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
  var inputTileSize: Array[Int] = {for (layerNo <- 0 until nLayers) yield
    if (layerNo == 0 && nLayers > 1) kernelShape(layerNo).s else inputTileSizeL1 }.toArray
  var inputTileStep: Array[Int] = {for (layerNo <- 0 until nLayers) yield
    inputTileSize(layerNo) - (kernelShape(layerNo).s - kernelStride(layerNo))}.toArray
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
      val n: Float = (inputTileSize(layerNo) -
        (kernelShape(layerNo).s - kernelStride(layerNo))).toFloat / kernelStride(layerNo)
      if (n % 1 != 0) throw new java.lang.IllegalArgumentException(
        "Input tiles are not divisible by the chosen kernelShape and kernelStep")
      n.toInt
    }

    /* Output shape */
    outputShape(layerNo) = Shape(
      w={val w: Float = (inputShape(layerNo).w - (kernelShape(layerNo).w - kernelStride(layerNo))).toFloat /
        kernelStride(layerNo)
        if (w % 1 != 0) throw new java.lang.IllegalArgumentException(
          "Inputs are not divisible by the chosen kernelShape and kernelStep")
        w.toInt
      },
      h={val h: Float = (inputShape(layerNo).h - (kernelShape(layerNo).h - kernelStride(layerNo))).toFloat /
        kernelStride(layerNo)
        if (h % 1 != 0) throw new java.lang.IllegalArgumentException(
          "Inputs are not divisible by the chosen kernelShape and kernelStep")
        h.toInt
      },
      ch=inputShape(layerNo).ch,
      wPadded=((inputShape(layerNo).wPadded - (kernelShape(layerNo).w - kernelStride(layerNo))).toFloat /
        kernelStride(layerNo)).toInt,
      hPadded=((inputShape(layerNo).hPadded - (kernelShape(layerNo).h - kernelStride(layerNo))).toFloat /
        kernelStride(layerNo)).toInt)
  }


  /* Execution runtimes */
  val runTimes: Array[Double] = Array.fill[Double](nLayers)(0)


  /* Parallelization parameters */
  val elsPerThread: Array[Int] = Array.fill[Int](nLayers)(1)
  elsPerThread(nLayers - 1) = elsPerThreadL1
  val localSize: Array[Array[Int]] = Array.fill[Array[Int]](3)(Array.fill[Int](nLayers)(0))
  val globalSize: Array[Array[Int]] = Array.fill[Array[Int]](3)(Array.fill[Int](nLayers)(0))

  for (layerNo <- 0 until nLayers) {
    /* Check parameters */
    if (inputShape(layerNo).s < kernelShape(layerNo).s)
      throw new java.lang.IllegalArgumentException(
        f"The kernel size in layer $layerNo%d (=${kernelShape(layerNo).s}%d) must be smaller than the image size " +
        f"(=${inputShape(layerNo).s}%d)")

    if (kernelShape(layerNo).s % elsPerThread(layerNo) != 0)
      throw new java.lang.IllegalArgumentException(
        f"Kernel size in all dimensions (=${kernelShape(layerNo).s}%d) must be divisible by elsPerThread " +
        f"(=${elsPerThread(layerNo)}%d)")


    /* Compute local sizes */

    // Local size 0
    localSize(0)(layerNo) = scala.math.pow(nWindowsPerTilePerDim(layerNo), 2).toInt

    // Local size 1
    localSize(1)(layerNo) = inputShape(layerNo).s

    // Local size 2
    localSize(2)(layerNo) = Math.ceil(scala.math.pow(kernelShape(layerNo).s, 2).toFloat / elsPerThread(layerNo)).toInt

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
