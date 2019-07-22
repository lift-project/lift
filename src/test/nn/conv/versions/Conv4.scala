package nn.conv.versions

/**
  * Created by nm on 09/01/17.
  * Input channels are processed sequentially in each thread.
  * ARM Mali GPU-optimised
  */

import ir.ast.debug.AssertType
import ir.ast.{Zip, _}
import ir.{TupleType, Type}
import nn.conv._
import nn.{AT, _}
import opencl.ir.pattern._
import opencl.ir.{add, _}

/**
  * The companion object that contains the Lift expressions, configuration preprocessing and
  * verification, and helper functions.
  */
object Conv4 extends ConvCompanion {
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
  
  /************************** Parallel layer **************************/
  def Par(activationF: UserFun, inputShape: Shape, tiling: SlidingWindowConfig, nKernels: Int,
          sliding: SlidingWindowConfig,
          nKernelsPerWrg: Int, seqElsPerThread: Int, vectorLen: Int,
          coalesce: Boolean, unrollReduce: Boolean): Array[FunDecl] = {
    /*********** UserFuns ***********/
    def dotAndSumUp = UserFun("dotAndSumUp", Array("acc", "l", "r"),
      "{ return acc + dot(l, r); }",
      vectorLen match {
        case 2 => Seq(Float, Float2, Float2)
        case 3 => Seq(Float, Float3, Float3)
        case 4 => Seq(Float, Float4, Float4)
        case _ => throw new NotImplementedError("dotAndSumUp() does not support vectors of size " + vectorLen)
      }, Float)

    def vectoriseNonContiguous(vectorLen: Int) = {
      vectorLen match {
        case 2 => UserFun ("vectoriseNonContiguous", Array ("f0", "f1"),
          "{ return (float2)(f0, f1); }", Seq (Float, Float), Float2)
        case 3 => UserFun ("vectoriseNonContiguous", Array ("f0", "f1", "f2"),
          "{ return (float3)(f0, f1, f2); }", Seq (Float, Float, Float), Float3)
        case 4 => UserFun ("vectoriseNonContiguous", Array ("f0", "f1", "f2", "f3"),
          "{ return (float4)(f0, f1, f2, f3); }", Seq (Float, Float, Float, Float), Float4)
        case _ =>
          throw new NotImplementedError("vectoriseNonContiguous() does not support vectors of size " + vectorLen)
      }
    }
    
    def ArrayToVector(): FunDecl = {
      λ(AT(Float, vectorLen), (arr) => {
        /*ArrayAccess(0) $ */{
        if (coalesce)
          vectorLen match {
            case 2 => vectoriseNonContiguous(vectorLen)(
              ArrayAccess(0) $ arr,
              ArrayAccess(1) $ arr)
            case 3 => vectoriseNonContiguous(vectorLen)(
              ArrayAccess(0) $ arr,
              ArrayAccess(1) $ arr,
              ArrayAccess(2) $ arr)
            case 4 => vectoriseNonContiguous(vectorLen)(
              ArrayAccess(0) $ arr,
              ArrayAccess(1) $ arr,
              ArrayAccess(2) $ arr,
              ArrayAccess(3) $ arr)
            case _ => throw new NotImplementedError("ArrayToVector() does not support size " + vectorLen)
          }
        else
          ArrayAccess(0) o asVector(vectorLen) $ arr
      }})}
    
    def Continue(): Lambda = λ((x) => x)
    
    def ReduceSeqMaybeUnroll(f: Lambda2, init: Expr) = 
      if (unrollReduce) ReduceSeqUnroll(f, init) 
      else ReduceSeq(f, init)
      
    /*********** Constants ***********/
    val nTilesInRow = tiling.n //(inputShape.sizePadded - (tiling.size - tiling.stride)) / tiling.stride
    val nTilesInCol = nTilesInRow
    val nTilesInInput = nTilesInCol * nTilesInRow
    
    val nWindowsInTileRow = sliding.n //(tiling.size - (sliding.size - sliding.stride)) / sliding.stride
    val nWindowsInTileCol = nWindowsInTileRow

    val nWindowsInRow = nTilesInRow * nWindowsInTileRow // Output W
    val nWindowsInCol = nTilesInCol * nWindowsInTileCol // Output Y 
    
    val nElementsInWindow = sliding.size * sliding.size * inputShape.nChannels
    
    val nSeqTilesInWindow = nElementsInWindow / seqElsPerThread

    val nWindowsInTile = nWindowsInTileCol * nWindowsInTileRow
    
    val nTilesTotal = inputShape.nBatches * inputShape.nInputs * nTilesInInput 
    
    val nKernelGroups = nKernels / nKernelsPerWrg

    /*********** Types ***********/
    val originalElementType: Float.type = Float
    val originalFlatWindowType: AT = AT(originalElementType, nElementsInWindow)
    val originalXType: AT = AT(AT(AT(AT(AT(originalElementType, inputShape.nChannels), inputShape.sizePadded), inputShape.sizePadded),
      inputShape.nInputs), inputShape.nBatches)

//    val elementType: Type = if (vectorLen == 1) Float else VectorType(Float, vectorLen)
    val elementType: Type = if (vectorLen == 1) Float else AT(Float, vectorLen)
    val windowSeqTileType: AT = AT(elementType, seqElsPerThread / vectorLen)
    val windowType: AT = AT(windowSeqTileType, nSeqTilesInWindow)
    val xTileType: AT = AT(windowType, nWindowsInTile)
    val xType: AT = AT(xTileType, nTilesTotal)

    val flatWindowType: AT = AT(elementType, nElementsInWindow)

    val partReducedElementType: Float.type = Float
    val partReducedWindowType: AT = AT(partReducedElementType, nSeqTilesInWindow)
    val partReducedOutChannelType: AT = AT(partReducedWindowType, nWindowsInTile)
    val partReducedOutChannelGroupType: AT = AT(partReducedOutChannelType, nKernelsPerWrg)
    val partReducedXTileType: AT = AT(partReducedOutChannelGroupType, nKernelGroups)
    val partReducedXType: AT = AT(partReducedXTileType, nTilesTotal)
    val flatPartReducedXType: AT = AT(partReducedElementType, nTilesTotal * nKernels * nWindowsInTile * nSeqTilesInWindow)
    
    val reducedWindowType: Float.type = Float
    val reducedOutChannelType: AT = AT(reducedWindowType, nWindowsInTile)
    val reducedXTileType: AT = AT(reducedOutChannelType, nKernels)
    val reducedXType: AT = AT(reducedXTileType, nTilesTotal)

    val resultType: AT = AT(AT(AT(AT(AT(Float,
      nWindowsInRow), nWindowsInCol),
      nKernels),
      inputShape.nInputs), inputShape.nBatches)

    val kernelWWindowType: AT = /* weights */windowType
    val kernelWGroupType: AT = AT(kernelWWindowType, nKernelsPerWrg)
    val kernelWType: AT = AT(kernelWGroupType, nKernelGroups)

    val kernelBPerWindowType: Float.type = /* bias */Float
    val kernelBGroupType: AT = AT(kernelBPerWindowType, nKernelsPerWrg)
    val kernelBType: AT = AT(kernelBGroupType, nKernelGroups)

    /*********** F-prop lambda: convolution with partial reduction ***********/
    def LayerPartial: FunDecl = {
      λ(AT(AT(AT(AT(Float, inputShape.nChannels), sliding.size), sliding.size), nKernels), 
        originalXType,
        (K, X) => {
          /*** Layer BEGIN ***/

          MapWrg(2)(λ(originalXType, (XUnwrapped) => {
            
            AssertType(partReducedXType, "Part reduced X type") o
              MapWrg(1)(λ(xTileType, (XTile) => {
                /*** Tile BEGIN ***/

                AssertType(partReducedXTileType, "Part reduced X XTile type") o
                  MapWrg(0)(λ(kernelWGroupType, (kernelWGroup) => {
                    /***** Output channel group BEGIN *****/

                    AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") o
                      MapLcl(2)(λ(kernelWWindowType, (kernelWWindow) => {
                        /***** Output channel BEGIN *****/

                        AssertType(partReducedOutChannelType, "Part reduced X output channel type") o
                          MapLcl(1)(λ(windowType, (window) =>
                            /******* Sliding window BEGIN *******/

                            AssertType(partReducedWindowType, "Part reduced window type") o
                              /* Remove the one-sized dimension introduced by Reduce */
                              Join() o
                              MapLcl(0)(λ(TupleType(windowSeqTileType, windowSeqTileType),
                                (SeqTileAndWeightsAndAcc) => {
                                  toGlobal(MapSeq(id)) o
                                    ReduceSeqMaybeUnroll(
                                      λ((acc, y) => {

                                        /********* Reducing window tile BEGIN *********/
                                        if (vectorLen == 1)
                                          multAndSumUp(acc, /* X */ Get(y, 0), /* kernelWWindow */ Get(y, 1))
                                        else
                                          dotAndSumUp(acc,
                                            ArrayToVector() $ /* X */ Get(y, 0),
                                            ArrayToVector() $ /* kernelWWindow */ Get(y, 1))
                                      }),
                                      toPrivate(id) $ Value("0.0f", Float)) $
                                    Zip(
                                      AssertType(windowSeqTileType) $ Get(SeqTileAndWeightsAndAcc, 0),
                                      AssertType(windowSeqTileType) $ Get(SeqTileAndWeightsAndAcc, 1))

                                  /********* Reducing window tile END *********/
                              })) $ Zip(window, /*Get(kernelWWindow, weightsNoInTuple)*/kernelWWindow)

                            /******* Sliding window END *******/
                          )) o AssertType(xTileType) $ XTile

                        /***** Output channel END *****/
                      })) o AssertType(kernelWGroupType, "Kernel weights group type") $ kernelWGroup

                    /***** Output channel group END *****/
                  })) o AssertType(kernelWType, "All kernel weights type after split") o
                  Split(nKernelsPerWrg) o Map(TileAndCoalesce() o Join() o Map(Join())) $ K 

                /*** Tile END ***/
              })) o SlideX() $ XUnwrapped

            // Wrap X into an array of 1
            // TODO: Split(originalXType.size)
          })) o Split(inputShape.nBatches) $ X

          /*** Layer END ***/
        })
    }
    
    /* Produces a tiled slided tiled version of X */
    def SlideX(): FunDecl = {
      λ(originalXType, (X) =>
        AssertType(xType, "SlideX output") o
          // Tile and coalesce
          Map(Map(TileAndCoalesce())) o
          // Join tiles and channels
          Map(Map(Join() o Join())) o
          // Join tile rows
          Map(Join()) o
          // Join inputs and tile rows
          Join() o Map(Join()) o
          // Join batches and inputs
          Join() o
          Map(Map(TiledSlidedND(2)(sliding.size, sliding.stride, tiling.stride))) o

          AssertType(originalXType, "SlideX input") $ X)
    }

    def Coalesce(): FunDecl =
      λ(flatWindowType, (window) =>
        Gather(ReorderWithStride(nSeqTilesInWindow))
          $ window)

//    def Vectorise(): FunDecl =
//      λ(originalFlatWindowType, (window) => {
//        if (!coalesce) 
//          asVector(vectorLen) $ window
//        else
//          Map(λ((futureVector) => {
//              vectorLen match {
//                case 2 => vectoriseNonContiguous(vectorLen)(
//                  CheckedArrayAccess(0, 0.0f) $ futureVector,
//                  CheckedArrayAccess(1, 0.0f) $ futureVector)
//                case 3 => vectoriseNonContiguous(vectorLen)(
//                  CheckedArrayAccess(0, 0.0f) $ futureVector,
//                  CheckedArrayAccess(1, 0.0f) $ futureVector,
//                  CheckedArrayAccess(2, 0.0f) $ futureVector)
//                case 4 => vectoriseNonContiguous(vectorLen)(
//                  CheckedArrayAccess(0, 0.0f) $ futureVector,
//                  CheckedArrayAccess(1, 0.0f) $ futureVector,
//                  CheckedArrayAccess(2, 0.0f) $ futureVector,
//                  CheckedArrayAccess(3, 0.0f) $ futureVector)
//                case _ => throw new NotImplementedError("Vectorise() does not support vectors of size " + vectorLen)
//              }})) o Split(vectorLen) $ window
//      })
    
    def TileAndCoalesce(): FunDecl =
      λ(originalFlatWindowType, (window) =>
        // Prepare for vectorisation
        {if (vectorLen != 1) Map(Split(vectorLen)) else Continue()} o 
          // Tile
          Split(seqElsPerThread) o
          // Coalesce
          {if (coalesce) Coalesce() else Continue()}
          
          $ window)

    
    /*********** F-prop lambda: final part of the reduction ***********/
    def LayerFinal: FunDecl = {
      λ(AT(Float, nKernels), flatPartReducedXType,
        (B, X) => {
          /*** Layer BEGIN ***/
 
            formatResults() o
            MapWrg(1)(λ(partReducedXTileType, (XTile) => {
              /*** Tile BEGIN ***/

              AssertType(reducedXTileType, "Reduced kernels type") o Join() o 
                MapWrg(0)(λ(TupleType(partReducedOutChannelGroupType, kernelBGroupType), (outputChannelGroup) => {
                  /***** Output channel group BEGIN *****/

                    MapLcl(1)(λ(TupleType(partReducedOutChannelType, kernelBPerWindowType), (outputChannel) => {
                      /***** Output channel BEGIN *****/

                      AssertType(reducedOutChannelType, "Reduced X output channel type") o
                        /* Remove the one-sized dimension introduced by Reduce */
                        Join() o
                        MapLcl(0)(λ(partReducedWindowType, (window) =>
                          /******* Sliding window BEGIN *******/

                          /* Map before AssertType is needed because of the one-sized dimension introduced 
                             by Reduce */
                          Map(AssertType(reducedWindowType, "Reduced window type")) o
                            MapSeq(toGlobal(id)) o
                            ReduceSeqMaybeUnroll(add, toPrivate(id) o
                              AssertType(kernelBPerWindowType, "Kernel bias per window type") $
                              Get(outputChannel, 1)
                            ) o AssertType(partReducedWindowType, "Part reduced X window type") $ window

                          /******* Sliding window END *******/
                        )) o AssertType(partReducedOutChannelType) $ Get(outputChannel, 0)

                      /***** Output channel END *****/
                    })) $ Zip(
                    AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") $
                      Get(outputChannelGroup, 0),
                    AssertType(kernelBGroupType, "Bias group type") $ Get(outputChannelGroup, 1))

                  /***** Output channel group END *****/
                })) $ Zip(
                AssertType(partReducedXTileType, "Part reduced X tile") $ XTile,
                AssertType(kernelBType, "All kernel biases type after split") o Split(nKernelsPerWrg) $ B)

              /*** Tile END ***/
            })) o structurizeX() $ X

          /*** Layer END ***/
        })
    }

    /** structurizeX() converts the flat output of the previous kernel into 5D array **/
    def structurizeX(): FunDecl =
      λ(flatPartReducedXType, (X) =>
        AssertType(partReducedXType, "Partially reduced X type") o
          Split(nKernelGroups) o Split(nKernelsPerWrg) o Split(nWindowsInTile) o 
          Split(nSeqTilesInWindow) $ X)
    
    def formatResults(): FunDecl =
      λ(reducedXType, (reducedX) =>
        AssertType(resultType, "Result type") o
          // Flatten tiles
          Map(/* Batch */
            Map(/* Image */
              Map(/* Output channel */
                Join() o
                  Map(/* Tile row */
                    Map(/* Tile row */ Join()) o
                      TransposeW())))) o
          // Move nChannels up
          Map(/* Batch */
            Map(/* Image */
              /* (tile rows, nChannels, tiles) -> (nChannels, tile rows, tiles) */
              TransposeW() o
                Map(/* Tile row */
                  /* (tiles, nChannels) -> (nChannels, tiles) */
                  TransposeW()))) o          
          /* Batches */ Split(inputShape.nInputs) o
          /* Inputs */ Split(nTilesInCol) o
          /* Tile rows */ Split(nTilesInRow) o
          /* Sliding window rows in tiles */
          Map(Map(Split(nWindowsInTileRow))) o
          AssertType(reducedXType, "Reduced X type") $ reducedX)

    Array(LayerPartial, LayerFinal)
  }

  def apply(iP: InitParameters): Conv4 = {
    /**
      * Class factory: verifies that an object can be created,
      * initializes variables, computes workgroup sizes.
      */

    val exceptionMsgPrefix: String = "[" + iP.testConfigFilename + ": layer " + iP.layerNo + "]\n" +
      "In the Conv layer with the following configuration:\n" + iP.toString + "\n"
//      conv.configToString(iP.inputShape.size, -1, iP.optParams.elsPerThread,
//        iP.dim.nKernels, iP.optParams.kernelsPerGroup,  iP.optParams.vectorLen, 
//        iP.optParams.coalesce, iP.optParams.unrollReduce,
//        iP.dim.kernelSize, iP.dim.kernelStride, iP.optParams.inputTileSize)

    /* Tiles */
    val slider: SlidingWindowConfig = SlidingWindowConfig(
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
    val tiler: SlidingWindowConfig = {
      val stride: Int = iP.optParams.inputTileSize - (slider.size - slider.stride)
        
      SlidingWindowConfig(
        size = iP.optParams.inputTileSize,
        stride = stride,
        n = {
          // Sometimes (e.g. for ResNet), layer configuration is such that the input cannot be evenly 
          // divided by kernel size and stride. In this case, Caffe uses floor, i.e. pixels covered only 
          // by non-fitting windows are discarded. 
          val activeInputSize: Int =
          (Math.floor((iP.inputShape.size - (iP.dim.kernelSize - iP.dim.kernelStride)).toFloat / iP.dim.kernelStride) *
            iP.dim.kernelStride + (iP.dim.kernelSize - iP.dim.kernelStride)).toInt
          val n: Float =
            (activeInputSize - (iP.optParams.inputTileSize - stride)).toFloat / stride
          if (n % 1 != 0) throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
            f"active image size ($activeInputSize%d) (image portion covered by specified kernel size and stride) " +
            f"is not divisible by the chosen " +
            f"input tile size (${iP.optParams.inputTileSize}%d) and corresponding tile stride ($stride%d)")
          n.toInt
        })
    }

    /* Check parameters */    
    if (iP.dim.nKernels % iP.optParams.kernelsPerGroup != 0)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"the number of kernels (${iP.dim.nKernels}%d) must be divisible by " +
        f"kernelsPerGroup (${iP.optParams.kernelsPerGroup}%d)")

    val window_size = slider.size * slider.size * iP.inputShape.nChannels
    if (window_size % iP.optParams.elsPerThread != 0)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"window size (kernel size * kernel size * nChannels = ${window_size}%d) " +
        f"must be divisible by elsPerThread (${iP.optParams.elsPerThread}%d)")
    
    if (iP.optParams.elsPerThread % iP.optParams.vectorLen != 0)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"elsPerThread (${iP.optParams.elsPerThread}%d) must be divisible by " +
        f"vectorLen (${iP.optParams.vectorLen}%d)")
    
    /* Memory requirements */
    val nTilesTotal = iP.inputShape.nBatches.toLong * iP.inputShape.nInputs * tiler.n * tiler.n
    val nWindowsInTile = slider.n.toLong * slider.n
    val nSeqTilesInWindow = slider.size.toLong * slider.size * iP.inputShape.nChannels / iP.optParams.elsPerThread
    val partReducedInputsSize = nTilesTotal.toLong * slider.nChannels * nWindowsInTile * nSeqTilesInWindow * 4
    
    val memoryLimit = 1500000000
    if (partReducedInputsSize > memoryLimit)
      throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
        f"partReducedInputsSize (=$partReducedInputsSize%d) must be smaller than the memory limit ($memoryLimit%d)")

    /* Padding */
    // Calculate how much padding is required
    // It's the same, but makes more sense
    iP.inputShape.sizePadded = 
      if (iP.padData) tiler.size + tiler.stride * tiler.n else iP.inputShape.size
    iP.inputShape.sizePadded = iP.inputShape.sizePadded

    val outputShape: Shape = { 
      val size = ((iP.inputShape.size - (slider.size - slider.stride)).toFloat / slider.stride).toInt
      
      Shape(
      nBatches = iP.inputShape.nBatches,
      nInputs = iP.inputShape.nInputs,
      size = size,
      sizePadded = if (!iP.padData) size else {
        val sizePadded: Float = (iP.inputShape.sizePadded - (slider.size - slider.stride)).toFloat /
          slider.stride
        if (sizePadded % 1 != 0) throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          "padded inputs are not divisible by the chosen kernelShape and kernelStride")
        sizePadded.toInt
      },
      nChannels = iP.dim.nKernels)
    }

    /* Parallelization parameters */
    val localSize: Array[Int] = Array.fill[Int](5)(0)

//    localSize(2) = (iP.optParams.kernelsPerGroup *
//      Math.ceil(slider.size.toFloat / iP.optParams.elsPerThread)).toInt
//    localSize(1) = slider.size
//    localSize(0) = scala.math.pow(
//      (tiler.size - (slider.size - slider.stride)) / slider.stride, 2).toInt
    
    // First kernel
    localSize(2) = iP.optParams.kernelsPerGroup
    localSize(1) = /* nWindowsInTile */ slider.n * slider.n
    localSize(0) = /* nSeqTilesInWindow */ iP.inputShape.nChannels * slider.size * slider.size / 
      iP.optParams.elsPerThread
    
    // Second kernel
    localSize(1 + 3) = iP.optParams.kernelsPerGroup
    localSize(0 + 3) = /* nWindowsInTile */ slider.n * slider.n

//    val maxWorkGroupSize = nn.maxWorkGroupSize
    val maxWorkGroupSize = 256

    {
      val groupSize: Int = localSize(0) * localSize(1) * localSize(2)
      if (groupSize > maxWorkGroupSize)
        throw new java.lang.IllegalArgumentException(exceptionMsgPrefix +
          f"group size (==$groupSize%d) must be less or equal to maxWorkGroupSize ($maxWorkGroupSize%d).\n" +
          f"Decrease nKernelsPerGroup or inputTileSize or increase elsPerThread (${iP.optParams.elsPerThread}%d)")
    }

    val globalSize: Array[Int] = Array.fill[Int](5)(0)
//    globalSize(2) = localSize(2) * iP.inputShape.nInputs * tiler.n * tiler.n// * iP.inputShape.nChannels
//    globalSize(1) = localSize(1) * Math.ceil(iP.dim.nKernels.toFloat / iP.optParams.kernelsPerGroup).toInt
//    globalSize(0) = localSize(0) * iP.inputShape.nBatches
    // First kernel
    globalSize(2) = 1 * localSize(2)
    globalSize(1) = /* nTilesTotal */ tiler.n * tiler.n * iP.inputShape.nInputs * iP.inputShape.nBatches * 
      localSize(1)
    globalSize(0) = /* nKernelGroups */ (iP.dim.nKernels / iP.optParams.kernelsPerGroup) * localSize(0)
    
    // Second kernel
    globalSize(1 + 3) = /* nTilesTotal */ tiler.n * tiler.n * iP.inputShape.nInputs * 
      iP.inputShape.nBatches * localSize(1 + 3)
    globalSize(0 + 3) = /* nKernelGroups */ (iP.dim.nKernels / iP.optParams.kernelsPerGroup) * localSize(0 + 3)

    /* Now that all parameters are calculated and verified, build the layer */

    new Conv4(
      iP.liftFPropFactory(iP.activationFun, iP.inputShape, tiler,
        iP.dim.nKernels, slider, iP.optParams.kernelsPerGroup, iP.optParams.elsPerThread, iP.optParams.vectorLen,
        iP.optParams.coalesce, iP.optParams.unrollReduce),
      iP.inputShape, outputShape,
      tiler, slider,
      iP.optParams.elsPerThread, iP.optParams.kernelsPerGroup,
      iP.optParams.vectorLen, iP.optParams.coalesce, iP.optParams.unrollReduce,
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
case class Conv4(override val liftFProp: Array[FunDecl],
                 override val inputShape: Shape, override val outputShape: Shape,
                 override val inputTiling: SlidingWindowConfig, override val kernelSliding: SlidingWindowConfig,
                 override val elsPerThread: Int, override val kernelsPerGroup: Int,
                 override val vectorLen: Int, override val coalesce: Boolean, override val unrollReduce: Boolean,
                 override val localSize: Array[Int], override val globalSize: Array[Int])
  extends Conv(liftFProp, inputShape, outputShape, inputTiling, kernelSliding,
    elsPerThread, kernelsPerGroup, vectorLen, coalesce, unrollReduce, localSize, globalSize) {

  override def toString: String =
    f"Conv4(inputShape = " + inputShape.toString + "," +
      "\noutputShape = " + outputShape.toString + "," +
      "\ninputTiling = " + inputTiling.toString + "," +
      "\nkernelSliding = " + kernelSliding.toString + "," +
      f"\nelsPerThread = $elsPerThread%d, kernelsPerGroup = $kernelsPerGroup%d, vectorLen = $vectorLen%d," +
      f"\ncoalesce = $coalesce%b, unrollReduce = $unrollReduce%b," +
      f"\nlocalSize = [" + localSize.mkString(", ") + "], globalSize = [" + globalSize.mkString(", ") + "])"
  
//  val configToString: String =
//    nn.conv.configToString(inputShape.sizePadded, outputShape.sizePadded, elsPerThread, outputShape.nChannels,
//      kernelsPerGroup, vectorLen, coalesce, unrollReduce, kernelSliding.size, kernelSliding.stride, inputTiling.size)
  var runtime: Double = 0
  val intermediateDataShape: (Int, Int, Int, Int, Int) = (
    inputShape.nBatches * inputShape.nInputs * inputTiling.n * inputTiling.n,
    outputShape.nChannels / kernelsPerGroup,
    kernelsPerGroup, kernelSliding.n * kernelSliding.n,
    inputShape.nChannels * kernelSliding.size * kernelSliding.size / elsPerThread)

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