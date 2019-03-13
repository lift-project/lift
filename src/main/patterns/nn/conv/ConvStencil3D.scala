package patterns.nn.conv

import ir.{ArrayType, ArrayTypeWS, TupleType, Type}
import ir.ast.debug.AssertType
import ir.ast.{ArrayAccess, Expr, FunDecl, Gather, Get, Join, Lambda, Lambda2, Map, Pattern, ReorderWithStride, Split, TiledSlidedND, TransposeW, UserFun, Value, Zip, asVector, λ}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.ir._
import opencl.ir.pattern._
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DTuneParams}
import patterns.nn.{LayerConfig, LayerExpression, LayerTuneParams}
import patterns.nn.utils.Utils.slidingOutputSize


class ConvStencil3D[ConfigType <: ArithExpr, TuneParamType <: ArithExpr]
(layerConfig: ConvStencil3DLayerConfig[ConfigType],
 tuneParams: ConvStencil3DTuneParams[TuneParamType])
  extends LayerExpression(layerConfig, tuneParams) {

  def AT = ArrayType // alias
  type AT = ArrayType // alias


  /************************** Parallel layer **************************/
  def apply(activationF: UserFun): (Lambda, Lambda) = {
    /*********** UserFuns ***********/
    val vectorisableMultAndSumUp = UserFun("vectorisableMultAndSumUp", Array("acc", "l", "r"),
      "{ return acc + (l * r); }",
      Seq(Float, Float, Float), Float)

//    @Deprecated
//    def dotAndSumUp = UserFun("dotAndSumUp", Array("acc", "l", "r"),
//      "{ return acc + dot(l, r); }",
//      tuneParams.vectorLen match {
//        case Cst(2) => Seq(Float, Float2, Float2)
//        case Cst(3) => Seq(Float, Float3, Float3)
//        case Cst(4) => Seq(Float, Float4, Float4)
//        case _ => throw new NotImplementedError("dotAndSumUp() does not support vectors of size " + tuneParams.vectorLen)
//      }, Float)
//
//    @Deprecated
//    def vectoriseNonContiguous(vectorLen: ArithExpr) = {
//      vectorLen match {
//        case Cst(2) => UserFun ("vectoriseNonContiguous", Array ("f0", "f1"),
//          "{ return (float2)(f0, f1); }", Seq (Float, Float), Float2)
//        case Cst(3) => UserFun ("vectoriseNonContiguous", Array ("f0", "f1", "f2"),
//          "{ return (float3)(f0, f1, f2); }", Seq (Float, Float, Float), Float3)
//        case Cst(4) => UserFun ("vectoriseNonContiguous", Array ("f0", "f1", "f2", "f3"),
//          "{ return (float4)(f0, f1, f2, f3); }", Seq (Float, Float, Float, Float), Float4)
//        case _ =>
//          throw new NotImplementedError("vectoriseNonContiguous() does not support vectors of size " + vectorLen)
//      }
//    }
//
//    @Deprecated
//    def ArrayToVector(): FunDecl = {
//      λ(AT(Float, tuneParams.vectorLen), (arr) => {
//        /*ArrayAccess(0) $ */{
//          if (tuneParams.coalesce)
//            tuneParams.vectorLen match {
//              case Cst(2) => vectoriseNonContiguous(tuneParams.vectorLen)(
//                ArrayAccess(0) $ arr,
//                ArrayAccess(1) $ arr)
//              case Cst(3) => vectoriseNonContiguous(tuneParams.vectorLen)(
//                ArrayAccess(0) $ arr,
//                ArrayAccess(1) $ arr,
//                ArrayAccess(2) $ arr)
//              case Cst(4) => vectoriseNonContiguous(tuneParams.vectorLen)(
//                ArrayAccess(0) $ arr,
//                ArrayAccess(1) $ arr,
//                ArrayAccess(2) $ arr,
//                ArrayAccess(3) $ arr)
//              case _ => throw new NotImplementedError("ArrayToVector() does not support size " + tuneParams.vectorLen)
//            }
//          else
//            ArrayAccess(0) o asVector(tuneParams.vectorLen) $ arr
//        }})}

    def Continue(): Lambda = λ((x) => x)

    def ReduceSeqMaybeUnroll(f: Lambda2, init: Expr) =
      if (tuneParams.unrollReduce) ReduceSeqUnroll(f, init)
      else ReduceSeq(f, init)

    /*********** Constants ***********/
//    val tileWidthHeight = (layerConfig.kernelWidthHeight - layerConfig.kernelStride) + tuneParams.tileStride
    val tileWidthHeight = tuneParams.tileWidthHeight
    val tileStride = tileWidthHeight - (layerConfig.kernelWidthHeight - layerConfig.kernelStride)
//    val tileStride = tuneParams.tileStride
    val nTilesInRow = slidingOutputSize(
      layerConfig.inputWidthHeight,
      tileWidthHeight,
      tileStride)

    val nTilesInCol = nTilesInRow
    val nTilesInInput = nTilesInCol * nTilesInRow

    val nWindowsInTileRow = slidingOutputSize(
      tileWidthHeight,
      layerConfig.kernelWidthHeight,
      layerConfig.kernelStride)
    val nWindowsInTileCol = nWindowsInTileRow

    val nWindowsInRow = nTilesInRow * nWindowsInTileRow // Output W
    val nWindowsInCol = nTilesInCol * nWindowsInTileCol // Output Y

    val nElementsInWindow = layerConfig.kernelWidthHeight * layerConfig.kernelWidthHeight * layerConfig.inputChannels

    val nSeqTilesInWindow = nElementsInWindow /^ tuneParams.seqOpsPerThread

    val nWindowsInTile = nWindowsInTileCol * nWindowsInTileRow

    val nTilesTotal = layerConfig.nInputs * nTilesInInput

    val nKernelGroups = layerConfig.kernelChannels /^ tuneParams.nKernelsPerWrg

    /*********** Types ***********/
    val originalElementType: Float.type = Float
    val originalFlatWindowType: AT = AT(originalElementType, nElementsInWindow)
    val originalXType: AT = AT(AT(AT(AT(AT(originalElementType,
      layerConfig.inputChannels),
      layerConfig.inputWidthHeight),
      layerConfig.inputWidthHeight),
      layerConfig.nInputs), 1 /*TODO: get rid of the input batch dimension*/)

//    val elementType: Type = if (tuneParams.vectorLen == 1) Float else AT(Float, tuneParams.vectorLen)
    val elementType: Type = Float //if (vectorLen == 1) Float else  AT(Float, tuneParams.vectorLen)
    val windowSeqTileType: AT = AT(elementType, tuneParams.seqOpsPerThread)// / tuneParams.vectorLen)
    val windowType: AT = AT(windowSeqTileType, nSeqTilesInWindow)
    val xTileType: AT = AT(windowType, nWindowsInTile)
    val xType: AT = AT(xTileType, nTilesTotal)


    val slidedXType: AT = AT(AT(AT(AT(AT(AT(AT(AT(AT(originalElementType,
      layerConfig.inputChannels),
      layerConfig.kernelWidthHeight),
      layerConfig.kernelWidthHeight),
      slidingOutputSize(
        tileWidthHeight,
        layerConfig.kernelWidthHeight,
        layerConfig.kernelStride)),
      slidingOutputSize(
        tileWidthHeight,
        layerConfig.kernelWidthHeight,
        layerConfig.kernelStride)),
      slidingOutputSize(
        layerConfig.inputWidthHeight,
        tileWidthHeight,
        tileStride)),
      slidingOutputSize(
        layerConfig.inputWidthHeight,
        tileWidthHeight,
        tileStride)),
      layerConfig.nInputs),
      1)

    val flatWindowType: AT = AT(elementType, nElementsInWindow)

    val partReducedElementType: Float.type = Float
    val partReducedWindowType: AT = AT(partReducedElementType, nSeqTilesInWindow)
    val partReducedOutChannelType: AT = AT(partReducedWindowType, nWindowsInTile)
    val partReducedOutChannelGroupType: AT = AT(partReducedOutChannelType, tuneParams.nKernelsPerWrg)
    val partReducedXTileType: AT = AT(partReducedOutChannelGroupType, nKernelGroups)
    val partReducedXType: AT = AT(partReducedXTileType, nTilesTotal)
    val flatPartReducedXType: AT = AT(partReducedElementType,
      nTilesTotal * layerConfig.kernelChannels * nWindowsInTile * nSeqTilesInWindow)

    val reducedWindowType: Float.type = Float
    val reducedOutChannelType: AT = AT(reducedWindowType, nWindowsInTile)
    val reducedXTileType: AT = AT(reducedOutChannelType, layerConfig.kernelChannels)
    val reducedXType: AT = AT(reducedXTileType, nTilesTotal)

    val resultType: AT = AT(AT(AT(AT(AT(Float,
      nWindowsInRow), nWindowsInCol),
      layerConfig.kernelChannels),
      layerConfig.nInputs), 1 /*TODO: get rid of the input batch dimension*/)

    val kernelWWindowType: AT = /* weights */windowType
    val kernelWGroupType: AT = AT(kernelWWindowType, tuneParams.nKernelsPerWrg)
    val kernelWType: AT = AT(kernelWGroupType, nKernelGroups)

    val kernelBPerWindowType: Float.type = /* bias */Float
    val kernelBGroupType: AT = AT(kernelBPerWindowType, tuneParams.nKernelsPerWrg)
    val kernelBType: AT = AT(kernelBGroupType, nKernelGroups)

    /****** Helper functions ******/

    /* Produces a tiled slided tiled version of X */
    def SlideX(): FunDecl = {
      λ(originalXType, (X) =>
//          AssertType(xType, "SlideX output") o
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
//          AssertType(slidedXType, "tiledSlideND type") o
          Map(Map(TiledSlidedND(2)(layerConfig.kernelWidthHeight, layerConfig.kernelStride, tileStride,
          enableUndoTiling = false))) //o
//          AssertType(originalXType, "SlideX input")
      $ X)
    }

    def Coalesce(): FunDecl =
      λ(flatWindowType, (window) =>
        Gather(ReorderWithStride(nSeqTilesInWindow))
          $ window)

    def TileAndCoalesce(): FunDecl =
      λ(originalFlatWindowType, (window) =>
        // Prepare for vectorisation
        //        {if (tuneParams.vectorLen != 1) Map(Split(tuneParams.vectorLen)) else Continue()} o
        Continue() o
        //Map(Split(tuneParams.vectorLen)) o
          // Tile
          Split(tuneParams.seqOpsPerThread) o
          // Coalesce
          {if (tuneParams.coalesce) Coalesce() else Continue()}

          $ window)


    /** structuriseX() converts the flat output of the previous kernel into 5D array **/
    def structuriseX(): FunDecl =
      λ(flatPartReducedXType, (X) =>
        //AssertType(partReducedXType, "Partially reduced X type") o
          Split(nKernelGroups) o Split(tuneParams.nKernelsPerWrg) o Split(nWindowsInTile) o
          Split(nSeqTilesInWindow) $ X)

    def formatResults(): FunDecl =
      λ(reducedXType, (reducedX) =>
        //AssertType(resultType, "Result type") o
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
          /* Batches */ Split(layerConfig.nInputs) o
          /* Inputs */ Split(nTilesInCol) o
          /* Tile rows */ Split(nTilesInRow) o
          /* Sliding window rows in tiles */
          Map(Map(Split(nWindowsInTileRow))) /*o
          AssertType(reducedXType, "Reduced X type")*/ $ reducedX)

    /*********** F-prop lambda: convolution with partial reduction ***********/
    val layerPartial: Lambda = {
      λ(AT(AT(AT(AT(Float, layerConfig.inputChannels),
        layerConfig.kernelWidthHeight),
        layerConfig.kernelWidthHeight),
        layerConfig.kernelChannels),
        originalXType,
        (K, X) => {
          /*** Layer BEGIN ***/

          MapWrg(2)(λ(originalXType, (XUnwrapped) => {

//            AssertType(partReducedXType, "Part reduced X type") o
              MapWrg(1)(λ(xTileType, (XTile) => {
                /*** Tile BEGIN ***/

//                AssertType(partReducedXTileType, "Part reduced X XTile type") o
                  MapWrg(0)(λ(kernelWGroupType, (kernelWGroup) => {
                    /***** Output channel group BEGIN *****/

//                    AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") o
                      MapLcl(2)(λ(kernelWWindowType, (kernelWWindow) => {
                        /***** Output channel BEGIN *****/

//                        AssertType(partReducedOutChannelType, "Part reduced X output channel type") o
                          MapLcl(1)(λ(windowType, (window) =>
                            /******* Sliding window BEGIN *******/

//                            AssertType(partReducedWindowType, "Part reduced window type") o
                              /* Remove the one-sized dimension introduced by Reduce */
                              Join() o
                              MapLcl(0)(λ(TupleType(windowSeqTileType, windowSeqTileType),
                                (SeqTileAndWeightsAndAcc) => {
                                  toGlobal(MapSeq(id)) o
                                    ReduceSeqMaybeUnroll(
                                      λ((acc, y) => {

                                        /********* Reducing window tile BEGIN *********/
//                                        if (tuneParams.vectorLen == 1)
//                                          multAndSumUp(acc, /* X */ Get(y, 0), /* kernelWWindow */ Get(y, 1))
//                                        else
//                                          dotAndSumUp(acc,
//                                            ArrayToVector() $ /* X */ Get(y, 0),
//                                            ArrayToVector() $ /* kernelWWindow */ Get(y, 1))
                                        vectorisableMultAndSumUp(acc, /* X */ Get(y, 0), /* kernelWWindow */ Get(y, 1))
                                      }),
                                      toPrivate(id) $ Value("0.0f", Float)) $
                                    Zip(
                                      /*AssertType(windowSeqTileType) $ */ Get(SeqTileAndWeightsAndAcc, 0),
                                      /*AssertType(windowSeqTileType) $ */Get(SeqTileAndWeightsAndAcc, 1))

                                  /********* Reducing window tile END *********/
                                })) $ Zip(window, /*Get(kernelWWindow, weightsNoInTuple)*/kernelWWindow)

                            /******* Sliding window END *******/
                          ))/* o AssertType(xTileType) */$ XTile

                        /***** Output channel END *****/
                      }))/* o AssertType(kernelWGroupType, "Kernel weights group type") */$ kernelWGroup

                    /***** Output channel group END *****/
                  })) o //o AssertType(kernelWType, "All kernel weights type after split") o
                  Split(tuneParams.nKernelsPerWrg) o Map(TileAndCoalesce() o Join() o Map(Join())) $ K

                /*** Tile END ***/
              })) o SlideX() $ XUnwrapped

            // Wrap X into an array of 1
            // TODO: Split(originalXType.size)
          })) o Split(1 /*TODO: get rid of the input batch dimension*/) $ X

          /*** Layer END ***/
        })
    }

    /*********** F-prop lambda: final part of the reduction ***********/
    val layerFinal: Lambda = {
      λ(AT(Float, layerConfig.kernelChannels), flatPartReducedXType,
        (B, X) => {
          /*** Layer BEGIN ***/

          formatResults() o
            MapWrg(1)(λ(partReducedXTileType, (XTile) => {
              /*** Tile BEGIN ***/

//              AssertType(reducedXTileType, "Reduced kernels type") o
                Join() o
                MapWrg(0)(λ(TupleType(partReducedOutChannelGroupType, kernelBGroupType), (outputChannelGroup) => {
                  /***** Output channel group BEGIN *****/

                  MapLcl(1)(λ(TupleType(partReducedOutChannelType, kernelBPerWindowType), (outputChannel) => {
                    /***** Output channel BEGIN *****/

//                    AssertType(reducedOutChannelType, "Reduced X output channel type") o
                      /* Remove the one-sized dimension introduced by Reduce */
                      Join() o
                      MapLcl(0)(λ(partReducedWindowType, (window) =>
                        /******* Sliding window BEGIN *******/

                        /* Map before AssertType is needed because of the one-sized dimension introduced
                           by Reduce */
//                        Map(AssertType(reducedWindowType, "Reduced window type")) o
                          MapSeq(toGlobal(id)) o
                          ReduceSeqMaybeUnroll(add, toPrivate(id) /*o
                            AssertType(kernelBPerWindowType, "Kernel bias per window type") */$
                            Get(outputChannel, 1)
                          ) /*o AssertType(partReducedWindowType, "Part reduced X window type") */$ window

                        /******* Sliding window END *******/
                      )) /*o AssertType(partReducedOutChannelType) */$ Get(outputChannel, 0)

                    /***** Output channel END *****/
                  })) $ Zip(
                   /* AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") $*/
                      Get(outputChannelGroup, 0),
                    /*AssertType(kernelBGroupType, "Bias group type") $*/ Get(outputChannelGroup, 1))

                  /***** Output channel group END *****/
                })) $ Zip(
                /*AssertType(partReducedXTileType, "Part reduced X tile") $ */XTile,
                /*AssertType(kernelBType, "All kernel biases type after split") o*/ Split(tuneParams.nKernelsPerWrg) $ B)

              /*** Tile END ***/
            })) o structuriseX() $ X

          /*** Layer END ***/
        })
    }

    (layerPartial, layerFinal)
  }
}

object ConvStencil3D {
  val kernelWidthHeightTmp = Var("kernelWidthHeight")
  val kernelStrideTmp = Var("kernelStride")
  class ConvStencil3DLayerConfig[T <: ArithExpr](// Input config
                                                 val nInputs: T = Var("nInputs"),
                                                 val inputWidthHeight: T = Var("inputWidthHeight"),
                                                 val inputChannels: T = Var("inputChannels"),
                                                 // Layer-specific config
                                                 val kernelWidthHeight: T = kernelWidthHeightTmp,
                                                 val kernelChannels: T = Var("kernelChannels"),
                                                 val kernelStride: T = kernelStrideTmp,
                                                 val padWidthHeight: T = Var("padWidthHeight")) extends LayerConfig[T] {
    // TODO: handle padding
    val paramVector: Vector[T] = Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels,
      kernelStride, padWidthHeight)
  }

//  object ConvStencil3DLayerConfig {
//    /**
//      * Generates layer configuration based on the ONNX AST node and input data
//      */
//    def apply(onnxNode: ir.ast.onnx.Conv, input: Expr): ConvStencil3DLayerConfig = {
//      val ArrayTypeWS(ArrayTypeWS(ArrayTypeWS(ArrayTypeWS(Float, inputChannels), inputWidthHeight), _), nInputs) =
//        input.t
//
//      new ConvStencil3DLayerConfig(nInputs, inputWidthHeight, inputChannels,
//        /* kernel should be square, hence one value suffices here */
//        onnxNode.kernelShape(ir.ast.onnx.Conv.firstSpatialDimension),
//        onnxNode.kernelShape.head,
//        /* all strides should be the same, hence one value suffices here */
//        onnxNode.strides(0),
//        /* all pads should be the same, hence one value suffices here */
//        onnxNode.pads(0))
//    }
//  }

  /**
    * During lowering, the expression for the Conv layer is created using default variables for OptParams without
    * ranges. During parameter space exploration, they are replaced with constants (Cst()).
    */
  val tileStrideTmp = Var("tileStride")
  class ConvStencil3DTuneParams[T <: ArithExpr](val tileWidthHeight: T = (kernelWidthHeightTmp - kernelStrideTmp) + tileStrideTmp,
                                                val tileStride: T = tileStrideTmp,

                                                val vectorLen: T = Var("vectorLen"),
                                                val nKernelsPerWrg: T = Var("nKernelsPerWrg"),
                                                val seqOpsPerThread: T = Var("seqOpsPerThread"),

                                                val coalesce: Boolean = false,
                                                val unrollReduce: Boolean = false) extends LayerTuneParams[T] {
    val paramVector: Vector[T] = Vector(tileWidthHeight, tileStride, vectorLen, nKernelsPerWrg, seqOpsPerThread)
  }
  /**
    * Produces a convolution expression without an activation function
    */
  def apply(layerConfig: ConvStencil3DLayerConfig[ArithExpr],
            tuneParams: ConvStencil3DTuneParams[ArithExpr]): Seq[FunDecl] = {
    apply(id, layerConfig, tuneParams)
  }


  /**
    * Produces a convolution expression with an activation function
    */
  def apply(activationF: UserFun,
            layerConfig: ConvStencil3DLayerConfig[ArithExpr],
            tuneParams: ConvStencil3DTuneParams[ArithExpr]): Seq[FunDecl] = {

    val (layerPartial, layerFinal) = new ConvStencil3D(layerConfig, tuneParams)(activationF)

    Seq(layerPartial, layerFinal)
  }
}