package nn.conv.versions

/**
  * Created by nm on 09/01/17.
  * Input channels are processed sequentially in each thread.
  * ARM Mali GPU-optimised
  */

import ir.ast._
import ir.ast.debug.AssertType
import ir.{ArrayTypeWSWC, TupleType}
import nn.conv._
import nn.{AT, _}
import opencl.ir._
import opencl.ir.pattern._

/**
  * The companion object that contains the Lift expressions, configuration preprocessing and
  * verification, and helper functions.
  */
object Conv3 extends ConvCompanion {
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

  val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
    "{ return acc + (l * r); }",
    Seq(Float, Float, Float), Float)
  
  /************************** Parallel layer **************************/
  def Par(activation_f: UserFun, inputShape: Shape, tiling: SlidingWindowConfig, nKernels: Int,
          sliding: SlidingWindowConfig,
          nKernelsPerWrg: Int, seqElsPerThread: Int): FunDecl = {
    /*********** Constants ***********/
    val nTilesInRow =
      (inputShape.sizePadded - (tiling.size - tiling.stride)) / tiling.stride
    val nTilesInCol = nTilesInRow
    
    val nWindowsInTileRow =
      (tiling.size - (sliding.size - sliding.stride)) / sliding.stride
    val nWindowsInTileCol = nWindowsInTileRow
    
    val nTilesInWindow = inputShape.nChannels * sliding.size * sliding.size / seqElsPerThread
    
    val nWindowsInTile = nWindowsInTileRow * nWindowsInTileCol
    
    val nTilesTotal = nTilesInRow * nTilesInCol * inputShape.nInputs * inputShape.nBatches

    /*********** Types ***********/
//    val single_element_type: AT = AT(Float, inputShape.nChannels)

//    val sliding_window_type: AT = AT(single_element_type, sliding.size * sliding.size)
    
    val windowTileType: AT = AT(Float, seqElsPerThread)
    val windowType: AT = AT(windowTileType, nTilesInWindow)

    val nonTiledWindowType: AT = AT(Float, inputShape.nChannels * sliding.size * sliding.size)
    
    val partReducedWindowType: AT = AT(Float, nTilesInWindow)
    

    val xTileType: AT = AT(windowType, nWindowsInTile)
    val partReducedXTileType: AT = AT(partReducedWindowType, nWindowsInTile)


    val partReducedKernelGroupType: AT = AT(partReducedXTileType, nKernelsPerWrg)

    val partReducedKernelsType: AT = AT(partReducedKernelGroupType, nKernels / nKernelsPerWrg)
    
    
    val transformedXType: AT = AT(xTileType, nTilesTotal)
    
    val partReducedXType: AT = AT(partReducedKernelsType, nTilesTotal)
    
    val xType: AT = AT(AT(AT(AT(AT(Float, inputShape.nChannels), inputShape.sizePadded), inputShape.sizePadded),
      inputShape.nInputs), inputShape.nBatches)

    
    val paramsType: TupleType = TupleType(/* weights */windowType, /* bias */Float)
    val weightsNoInTuple = 0
    val biasNoInTuple = 1
    val paramsGroupType: AT = AT(paramsType, nKernelsPerWrg)
    val allParamsType: AT = AT(paramsGroupType, nKernels / nKernelsPerWrg)

    /*********** F-prop lambda: convolution with partial reduction ***********/
    def Layer_partial: FunDecl = {
      λ(
        AT(AT(AT(AT(Float, sliding.size), sliding.size), inputShape.nChannels), nKernels), AT(Float, nKernels), xType,
        (K, B, X) => {
          /*** Layer BEGIN ***/

          AssertType(partReducedXType, "Part reduced X type") o
            MapWrg(1)(λ(xTileType, (tile) => {
              /*** Tile BEGIN ***/

              AssertType(partReducedKernelsType, "Part reduced kernels type") o
                MapWrg(0)(λ(paramsGroupType, (paramsGroup) => {
                  /***** Params group BEGIN *****/

                  AssertType(partReducedKernelGroupType, "Part reduced kernel group type") o
                    MapLcl(1)(λ(paramsType, (params) => {
                      /***** Params BEGIN *****/

                      AssertType(partReducedXTileType, "Part reduced X tile type") o
                        MapLcl(0)(λ(TupleType(windowType, partReducedWindowType), (windowAndAcc) =>
                          /******* Sliding window BEGIN *******/

                          AssertType(partReducedWindowType, "Part reduced window type") o
                            MapSeq(λ(TupleType(windowTileType, windowTileType), (tileAndAccAndItsWeights) => {
                              toGlobal(MapSeq(id)) o
                                ReduceSeq(λ((acc, y) =>
                                  /******* Reducing window tile BEGIN *******/

                                  multAndSumUp.apply(acc, /* X */ Get(Get(y, 0), 0), /* weights */ Get(y, 1))),
                                  toPrivate(id) $ /* accumulator */ Get(Get(tileAndAccAndItsWeights, 0), 1)) $
                                tileAndAccAndItsWeights

                              /******* Reducing window tile END *******/
                            })) $ Zip(windowAndAcc, Get(params, weightsNoInTuple))

                          /******* Sliding window END *******/
                        )) $ Zip(AssertType(xTileType) $ tile,
                        Value("0.0f",
                          ArrayTypeWSWC(ArrayTypeWSWC(Float, nTilesInWindow), nWindowsInTileRow * nWindowsInTileCol)))

                      /***** Params END *****/
                    })) o AssertType(paramsGroupType, "Parameter group type") $ paramsGroup

                  /***** Params group END *****/
                })) o AssertType(allParamsType, "All parameters type after split") o
                Split(nKernelsPerWrg) $ Zip(Map(TileAndCoalesce() o Join() o Map(Join())) $ K, B)

              /*** Tile END ***/
            })) o SlideX() $ X

          /*** Layer END ***/
        })
    }
    
    /* Produces a tiled slided tiled version of X */
    def SlideX(): FunDecl = {
      λ(xType, (X) =>
        AssertType(transformedXType, "SlideX output") o 
          
          /*Map(Join() o Map(Join() o Map(Map(
            TileAndCoalesce() o  
            Join() o Map(Join()))))) o Join() o*/
          Map(Map(TiledSlidedND(2)(sliding.size, sliding.stride, tiling.stride))) o
          Join() o

          AssertType(xType, "SlideX input") $ X)
    }

    def Coalesce(): FunDecl =
      λ(nonTiledWindowType, (windowNontiled) =>
        Gather(ReorderWithStride(nTilesInWindow))
          $ windowNontiled)
    
    def TileAndCoalesce(): FunDecl =
      λ(nonTiledWindowType, (windowNontiled) =>
        Split(seqElsPerThread) o Coalesce() $ windowNontiled)

    def Layer = Layer_partial

    Layer
  }


  def apply(iP: InitParameters): Conv3 = {
    /**
      * Class factory: verifies that an object can be created,
      * initializes variables, computes workgroup sizes.
      */

    val exceptionMsgPrefix: String = "In the Conv layer with the following configuration:\n" +
      conv.configToString(iP.inputShape.sizePadded, -1, iP.optParams.elsPerThread,
        iP.dim.nKernels, iP.optParams.kernelsPerGroup, iP.dim.kernelSize, iP.dim.kernelStride, iP.optParams.inputTileSize)

    /* Tiles */
    val kernelSliding: SlidingWindowConfig = SlidingWindowConfig(
      size = iP.dim.kernelSize,
      stride = iP.dim.kernelStride,
      n = {
        val n: Float = 
          (iP.optParams.inputTileSize - (iP.dim.kernelSize - iP.dim.kernelStride)).toFloat / iP.dim.kernelStride
        if (n % 1 != 0) throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          f"input tiles (${iP.optParams.inputTileSize}%d) are not divisible by the chosen " +
          f"kernelSize (${iP.dim.kernelSize}%d) and kernelStride (${iP.dim.kernelStride}%d)")
        n.toInt
      },
      nChannels = iP.dim.nKernels
    )
    val inputTiling: SlidingWindowConfig = {
      val stride: Int = iP.optParams.inputTileSize - (kernelSliding.size - kernelSliding.stride)
      val trueN: Float = (iP.inputShape.size - iP.optParams.inputTileSize).toFloat / stride
      if (!iP.padData && trueN % 1 != 0)
        throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          f"image size (${iP.inputShape.size}%d) is not divisible by the chosen " +
          f"input tile size (${iP.optParams.inputTileSize}%d) and corresponding tile stride ($stride%d)")
        
      SlidingWindowConfig(
        size = iP.optParams.inputTileSize,
        stride = stride,
        // It's the same, but makes more sense
        n = Math.ceil(trueN).toInt)
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
    // It's the same, but makes more sense
    iP.inputShape.sizePadded = 
      if (iP.padData) inputTiling.size + inputTiling.stride * inputTiling.n else iP.inputShape.size
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

    localSize(2) = (iP.optParams.kernelsPerGroup *
      Math.ceil(kernelSliding.size.toFloat / iP.optParams.elsPerThread)).toInt
    localSize(1) = kernelSliding.size
    // TODO: make sure it is smaller than 64 (clinfo)
    localSize(0) = scala.math.pow(
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
    globalSize(2) = localSize(2) * iP.inputShape.nInputs * inputTiling.n * inputTiling.n// * iP.inputShape.nChannels
    globalSize(1) = localSize(1) * Math.ceil(iP.dim.nKernels.toFloat / iP.optParams.kernelsPerGroup).toInt
    globalSize(0) = localSize(0) * iP.inputShape.nBatches

    /* Now that all parameters are calculated and verified, build the layer */

    new Conv3(
      iP.liftFPropFactory(iP.activationFun, iP.inputShape, inputTiling,
        iP.dim.nKernels, kernelSliding, iP.optParams.kernelsPerGroup, iP.optParams.elsPerThread),
      iP.inputShape, outputShape,
      inputTiling, kernelSliding,
      iP.optParams.elsPerThread, iP.optParams.kernelsPerGroup,
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
case class Conv3(override val liftFProp: FunDecl,
                 override val inputShape: Shape, override val outputShape: Shape,
                 override val inputTiling: SlidingWindowConfig, override val kernelSliding: SlidingWindowConfig,
                 override val elsPerThread: Int, override val kernelsPerGroup: Int,
                 override val localSize: Array[Int], override val globalSize: Array[Int])
  extends Conv(liftFProp, inputShape, outputShape, inputTiling, kernelSliding,
    elsPerThread, kernelsPerGroup, localSize, globalSize) {
  val configToString: String =
    nn.conv.configToString(inputShape.sizePadded, outputShape.sizePadded, elsPerThread, outputShape.nChannels,
      kernelsPerGroup, kernelSliding.size, kernelSliding.stride, inputTiling.size)
  var runtime: Double = 0

  def groupAndUnpad(outputsFlat: Array[Float], datasets: NetDatasets): Unit = {
    datasets.asInstanceOf[ConvDatasets].outputs.nonPadded =
      nn.group(outputsFlat, (outputShape.nBatches, outputShape.nInputs,
        outputShape.nChannels, outputShape.sizePadded, outputShape.sizePadded)).map(
        batch => batch.map(
          input => input.map(
            channel => channel.map(
              row => row.slice(0, outputShape.size)
            ).slice(0, outputShape.size))))
  }
}