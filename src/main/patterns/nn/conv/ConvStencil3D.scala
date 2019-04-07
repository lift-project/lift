package patterns.nn.conv

import ir.ast.debug.AssertType
import ir.ast.{ArrayAccess, Expr, FunDecl, Gather, Get, Join, Lambda, Lambda2, Map, ReorderWithStride, Slide, Split, TiledSlidedND, TransposeW, UserFun, Value, Zip, asVector, λ}
import ir.{ArrayType, TupleType, Type}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.ir._
import opencl.ir.pattern._
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DTuneParams}
import patterns.nn.utils.Utils.slidingOutputSize
import patterns.nn._
import _root_.utils._


class ConvStencil3D(layerConfig: ConvStencil3DLayerConfig,
                    tuneParams: ConvStencil3DTuneParams)
  extends LayerExpression(layerConfig, tuneParams) {

  def AT = ArrayType // alias
  type AT = ArrayType // alias

  /*********** Constants ***********/
  val paddedInputWidthHeight = layerConfig.inputWidthHeight + 2 * layerConfig.padFunc + 2 * tuneParams.padOpt

  val tileStride = tuneParams.tileWidthHeight - (layerConfig.kernelWidthHeight - layerConfig.kernelStride)
  //    val tileStride = tuneParams.tileStride
  val nTilesInRow = slidingOutputSize(
    paddedInputWidthHeight,
    tuneParams.tileWidthHeight,
    tileStride) // 19 // 228/12 = 19

  val nTilesInCol = nTilesInRow
  val nTilesInInput = nTilesInCol * nTilesInRow // 361 // 361

  val nWindowsInTileRow = slidingOutputSize(
    tuneParams.tileWidthHeight,
    layerConfig.kernelWidthHeight,
    layerConfig.kernelStride) // 4 // 144/3 = 38
  val nWindowsInTileCol = nWindowsInTileRow

  val nWindowsInRow = nTilesInRow * nWindowsInTileRow // Output W
  val nWindowsInCol = nTilesInCol * nWindowsInTileCol // Output Y

  val nElementsInWindow = layerConfig.kernelWidthHeight * layerConfig.kernelWidthHeight * layerConfig.inputChannels // 576 // 576

  val nSeqTilesInWindow = nElementsInWindow /^ tuneParams.seqOpsPerThread // 576 / 288 = 2 // 576 / 288 =2

  val nWindowsInTile = nWindowsInTileCol * nWindowsInTileRow // 16 // 1444

  val nTilesTotal = layerConfig.nInputs * nTilesInInput // 361 // 361

  val nKernelGroups = layerConfig.kernelChannels /^ tuneParams.nKernelsPerWrg

  val flatPartReducedXSize = nTilesTotal * layerConfig.kernelChannels * nWindowsInTile * nSeqTilesInWindow

  /** ********* Types ***********/
  val originalElementType: Float.type = Float
  val originalFlatWindowType: AT = AT(originalElementType, nElementsInWindow)
  val originalXType = AT(AT(AT(AT(originalElementType,
    layerConfig.inputChannels),
    paddedInputWidthHeight),
    paddedInputWidthHeight),
    layerConfig.nInputs)

  //    val elementType: Type = if (tuneParams.vectorLen == 1) Float else AT(Float, tuneParams.vectorLen)
  val elementType: Type = Float //if (vectorLen == 1) Float else  AT(Float, tuneParams.vectorLen)
  val windowSeqTileType: AT = AT(elementType, tuneParams.seqOpsPerThread)
  // / tuneParams.vectorLen)
  val windowType: AT = AT(windowSeqTileType, nSeqTilesInWindow)
  val xTileType: AT = AT(windowType, nWindowsInTile)
  val xType: AT = AT(xTileType, nTilesTotal)


  val slidedXType: AT = AT(AT(AT(AT(AT(AT(AT(AT(originalElementType,
    layerConfig.inputChannels),
    layerConfig.kernelWidthHeight),
    layerConfig.kernelWidthHeight),
    slidingOutputSize(
      tuneParams.tileWidthHeight,
      layerConfig.kernelWidthHeight,
      layerConfig.kernelStride)),
    slidingOutputSize(
      tuneParams.tileWidthHeight,
      layerConfig.kernelWidthHeight,
      layerConfig.kernelStride)),
    slidingOutputSize(
      paddedInputWidthHeight,
      tuneParams.tileWidthHeight,
      tileStride)),
    slidingOutputSize(
      paddedInputWidthHeight,
      tuneParams.tileWidthHeight,
      tileStride)),
    layerConfig.nInputs)

  val flatWindowType: AT = AT(elementType, nElementsInWindow)

  val partReducedElementType: Float.type = Float
  val partReducedWindowType: AT = AT(partReducedElementType, nSeqTilesInWindow)
  val partReducedOutChannelType: AT = AT(partReducedWindowType, nWindowsInTile)
  val partReducedOutChannelGroupType: AT = AT(partReducedOutChannelType, tuneParams.nKernelsPerWrg)
  val partReducedXTileType: AT = AT(partReducedOutChannelGroupType, nKernelGroups)
  val partReducedXType = AT(partReducedXTileType, nTilesTotal)
  val flatPartReducedXType: AT = AT(partReducedElementType, flatPartReducedXSize)

  val reducedWindowType: Float.type = Float
  val reducedOutChannelType: AT = AT(reducedWindowType, nWindowsInTile)
  val reducedXTileType: AT = AT(reducedOutChannelType, layerConfig.kernelChannels)
  val reducedXType = AT(reducedXTileType, nTilesTotal)
  //    layerConfig.nInputs * nTilesInRow * nTilesInCol
  //    layerConfig.nInputs * nTilesInCol, nTilesInRow
  //    layerConfig.nInputs, nTilesInCol, nTilesInRow
  //    1, layerConfig.nInputs, nTilesInCol, nTilesInRow

  val resultType = AT(AT(AT(AT(Float,
    nWindowsInRow), nWindowsInCol),
    layerConfig.kernelChannels),
    layerConfig.nInputs)

  val kernelWWindowType: AT = /* weights */ windowType
  val kernelWGroupType: AT = AT(kernelWWindowType, tuneParams.nKernelsPerWrg)
  val kernelWType: AT = AT(kernelWGroupType, nKernelGroups)

  val kernelBPerWindowType: Float.type = /* bias */ Float
  val kernelBGroupType: AT = AT(kernelBPerWindowType, tuneParams.nKernelsPerWrg)
  val kernelBType: AT = AT(kernelBGroupType, nKernelGroups)


  /** ************************ Parallel layer **************************/
  def apply(activationF: UserFun): List[Lambda] = {
    /** ********* UserFuns ***********/
      def dotAndSumUp = UserFun("dotAndSumUp", Array("acc", "l", "r"),
        "{ return acc + dot(l, r); }", Seq(Float, Float, Float), Float)
//        tuneParams.vectorLen match {
//          case Cst(2) => Seq(Float, Float2, Float2)
//          case Cst(3) => Seq(Float, Float3, Float3)
//          case Cst(4) => Seq(Float, Float4, Float4)
//          case _ => throw new NotImplementedError("dotAndSumUp() does not support vectors of size " + tuneParams.vectorLen)
//        }, Float)
    //
//        @Deprecated
//        def vectoriseNonContiguous(vectorLen: ArithExpr) = {
//          vectorLen match {
//            case Cst(2) => UserFun ("vectoriseNonContiguous", Array ("f0", "f1"),
//              "{ return (float2)(f0, f1); }", Seq (Float, Float), Float2)
//            case Cst(3) => UserFun ("vectoriseNonContiguous", Array ("f0", "f1", "f2"),
//              "{ return (float3)(f0, f1, f2); }", Seq (Float, Float, Float), Float3)
//            case Cst(4) => UserFun ("vectoriseNonContiguous", Array ("f0", "f1", "f2", "f3"),
//              "{ return (float4)(f0, f1, f2, f3); }", Seq (Float, Float, Float, Float), Float4)
//            case _ =>
//              throw new NotImplementedError("vectoriseNonContiguous() does not support vectors of size " + vectorLen)
//          }
//        }
//
//        @Deprecated
//        def ArrayToVector(): FunDecl = {
//          λ(AT(Float, tuneParams.vectorLen), (arr) => {
//            /*ArrayAccess(0) $ */{
//              if (tuneParams.coalesce)
//                tuneParams.vectorLen match {
//                  case Cst(2) => vectoriseNonContiguous(tuneParams.vectorLen)(
//                    ArrayAccess(0) $ arr,
//                    ArrayAccess(1) $ arr)
//                  case Cst(3) => vectoriseNonContiguous(tuneParams.vectorLen)(
//                    ArrayAccess(0) $ arr,
//                    ArrayAccess(1) $ arr,
//                    ArrayAccess(2) $ arr)
//                  case Cst(4) => vectoriseNonContiguous(tuneParams.vectorLen)(
//                    ArrayAccess(0) $ arr,
//                    ArrayAccess(1) $ arr,
//                    ArrayAccess(2) $ arr,
//                    ArrayAccess(3) $ arr)
//                  case _ => throw new NotImplementedError("ArrayToVector() does not support size " + tuneParams.vectorLen)
//                }
//              else
//                ArrayAccess(0) o asVector(tuneParams.vectorLen) $ arr
//            }})}

    def Continue(): Lambda = λ((x) => x)

    def ReduceSeqMaybeUnroll(f: Lambda2, init: Expr) =
      if (tuneParams.unrollReduce) ReduceSeqUnroll(f, init)
      else ReduceSeq(f, init)

       /** **** Helper functions ******/

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
          AssertType(slidedXType, "tiledSlideND type") o
          Map(TiledSlidedND(2)(layerConfig.kernelWidthHeight, layerConfig.kernelStride, tileStride,
            enableUndoTiling = false)) o
          AssertType(originalXType, "SlideX input")
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
          {
            if (tuneParams.coalesce) Coalesce() else Continue()
          }

          $ window)


    /** structuriseX() converts the flat output of the previous kernel into 5D array **/
    def structuriseX(): FunDecl =
      λ(flatPartReducedXType, (X) =>
          Split(nKernelGroups) o Split(tuneParams.nKernelsPerWrg) o Split(nWindowsInTile) o
          Split(nSeqTilesInWindow) $ X)

    def formatResults(): FunDecl =
      λ(reducedXType, (reducedX) =>
        AssertType(resultType, "Result type") o
          // Flatten tiles
            Map(/* Image */
              Map(/* Output channel */
                Join() o
                  Map(/* Tile row */
                    Map(/* Tile row */ Join()) o
                      TransposeW()))) o
          // Move nChannels up
            Map(/* Image */
              /* (tile rows, nChannels, tiles) -> (nChannels, tile rows, tiles) */
              TransposeW() o
                Map(/* Tile row */
                  /* (tiles, nChannels) -> (nChannels, tiles) */
                  TransposeW())) o
          /* Inputs */ Split(nTilesInCol) o
          AssertType(AT(AT(AT(AT(AT(Float,
            tileStride / layerConfig.kernelStride),
            tileStride / layerConfig.kernelStride),
            layerConfig.kernelChannels),
            nTilesInRow),
            layerConfig.nInputs * nTilesInCol),
            "Split reduced X type (temp check)") o
          /* Tile rows */ Split(nTilesInRow) o
          /* Sliding window rows in tiles */
          //          PrintType() o
          Map(Map(Split(nWindowsInTileRow))) o
          AssertType(reducedXType, "Reduced X type") $ reducedX)

    /** ********* F-prop lambda: convolution with partial reduction ***********/
    val layerPartial: Lambda = {
      λ(AT(AT(AT(AT(Float, layerConfig.inputChannels),
        layerConfig.kernelWidthHeight),
        layerConfig.kernelWidthHeight),
        layerConfig.kernelChannels),
        originalXType,
        (K, X) => {
          /** * Layer BEGIN ***/
          AssertType(partReducedXType, "Part reduced X type") o
            Join() o
            MapWrg(2)(λ(originalXType, (XUnwrapped) => {
              MapWrg(1)(λ(xTileType, (XTile) => {
                /** * Tile BEGIN ***/

                AssertType(partReducedXTileType, "Part reduced X XTile type") o
                  MapWrg(0)(λ(kernelWGroupType, (kernelWGroup) => {
                    /** *** Output channel group BEGIN *****/

                    AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") o
                      MapLcl(2)(λ(kernelWWindowType, (kernelWWindow) => {
                        /** *** Output channel BEGIN *****/

                        AssertType(partReducedOutChannelType, "Part reduced X output channel type") o
                          MapLcl(1)(λ(windowType, (window) =>

                            /** ***** Sliding window BEGIN *******/

                            AssertType(partReducedWindowType, "Part reduced window type") o
                              /* Remove the one-sized dimension introduced by Reduce */
                              Join() o
                              MapLcl(0)(λ(TupleType(windowSeqTileType, windowSeqTileType),
                                (SeqTileAndWeightsAndAcc) => {
                                  toGlobal(MapSeq(id)) o
                                    /*ReduceSeqMaybeUnroll*/ReduceSeq(
                                      λ((acc, y) => {

                                        /** ******* Reducing window tile BEGIN *********/
  //                                        if (tuneParams.vectorLen == 1)
  //                                          multAndSumUp(acc, /* X */ Get(y, 0), /* kernelWWindow */ Get(y, 1))
  //                                        else
  //                                          dotAndSumUp(acc,
  //                                            ArrayToVector() $ /* X */ Get(y, 0),
  //                                            ArrayToVector() $ /* kernelWWindow */ Get(y, 1))
                                        dotAndSumUp(acc, /* X */ Get(y, 0), /* kernelWWindow */ Get(y, 1))
                                      }),
                                      toPrivate(id) $ Value("0.0f", Float)) $
                                    Zip(
                                      AssertType(windowSeqTileType) $ Get(SeqTileAndWeightsAndAcc, 0),
                                      AssertType(windowSeqTileType) $ Get(SeqTileAndWeightsAndAcc, 1))

                                  /** ******* Reducing window tile END *********/
                                })) $ Zip(window, /*Get(kernelWWindow, weightsNoInTuple)*/ kernelWWindow)

                            /** ***** Sliding window END *******/
                          )) o AssertType(xTileType) $ XTile

                        /** *** Output channel END *****/
                      })) o AssertType(kernelWGroupType, "Kernel weights group type") $ kernelWGroup

                    /** *** Output channel group END *****/
                  })) o AssertType(kernelWType, "All kernel weights type after split") o
                  Split(tuneParams.nKernelsPerWrg) o Map(TileAndCoalesce() o Join() o Map(Join())) $ K

                /** * Tile END ***/
              })) o SlideX() $ XUnwrapped

            // Wrap X into an array of 1
          })) o Split(layerConfig.nInputs) $ X

          /** * Layer END ***/
        })
    }

    /** ********* F-prop lambda: final part of the reduction ***********/
    val layerFinal: Lambda = {
      λ(AT(Float, layerConfig.kernelChannels), /*flatPartReducedXType*/ partReducedXType,
        (B, X) => {
          /** * Layer BEGIN ***/

          formatResults() o
            MapWrg(1)(λ(partReducedXTileType, (XTile) => {
              /** * Tile BEGIN ***/

              AssertType(reducedXTileType, "Reduced kernels type") o
                Join() o
                MapWrg(0)(λ(TupleType(partReducedOutChannelGroupType, kernelBGroupType), (outputChannelGroup) => {
                  /** *** Output channel group BEGIN *****/

                  MapLcl(1)(λ(TupleType(partReducedOutChannelType, kernelBPerWindowType), (outputChannel) => {
                    /** *** Output channel BEGIN *****/

                    AssertType(reducedOutChannelType, "Reduced X output channel type") o
                      /* Remove the one-sized dimension introduced by Reduce */
                      Join() o
                      MapLcl(0)(λ(partReducedWindowType, (window) =>

                        /** ***** Sliding window BEGIN *******/

                        /* Map before AssertType is needed because of the one-sized dimension introduced
                           by Reduce */
                        Map(AssertType(reducedWindowType, "Reduced window type")) o
                          MapSeq(toGlobal(id)) o
                          ReduceSeqMaybeUnroll(add, toPrivate(id) o
                            AssertType(kernelBPerWindowType, "Kernel bias per window type") $
                            Get(outputChannel, 1)
                          ) o AssertType(partReducedWindowType, "Part reduced X window type") $ window

                        /** ***** Sliding window END *******/
                      )) o AssertType(partReducedOutChannelType) $ Get(outputChannel, 0)

                    /** *** Output channel END *****/
                  })) $ Zip(
                    AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") $
                      Get(outputChannelGroup, 0),
                    AssertType(kernelBGroupType, "Bias group type") $ Get(outputChannelGroup, 1))

                  /** *** Output channel group END *****/
                })) $ Zip(
                AssertType(partReducedXTileType, "Part reduced X tile") $ XTile,
                AssertType(kernelBType, "All kernel biases type after split") o Split(tuneParams.nKernelsPerWrg) $ B)

              /** * Tile END ***/
            })) o AssertType(partReducedXType, "Partially reduced X type") /*o structuriseX()*/ $
            X

          /** * Layer END ***/
        })
    }

    List(layerPartial, layerFinal)
  }

  def eval(K: Array4D[Float],
           B: Array[Float],
           X: Array4D[Float],
           v: scala.collection.immutable.Map[Var, Cst]): Array4D[Float] = {

    // Check parameter values
    assert(layerConfig.paramVector.forall(param => v.contains(param)))
    assert(tuneParams.paramVector.forall(param => v.contains(param)))


    // Check shapes
    assert(shape4d(K).equals(List(
      v(layerConfig.kernelChannels).evalInt,
      v(layerConfig.kernelWidthHeight).evalInt,
      v(layerConfig.kernelWidthHeight).evalInt,
      v(layerConfig.inputChannels).evalInt)))

    assert(shape1d(B).equals(List(
      v(layerConfig.kernelChannels).evalInt)))

    assert(shape4d(X).equals(List(
      v(layerConfig.nInputs).evalInt,
      ArithExpr.substitute(paddedInputWidthHeight, v.toMap).evalInt,
      ArithExpr.substitute(paddedInputWidthHeight, v.toMap).evalInt,
      v(layerConfig.inputChannels).evalInt)))

    X.map(input => {
      val slidedRows: Array4D[Float] = Slide(
        v(layerConfig.kernelWidthHeight).evalInt,
        v(layerConfig.kernelStride).evalInt).
        eval(input)
      val slidedRowsShape = shape4d(slidedRows)

      val inputSlidedIn2D: Array5D[Float] = slidedRows.map(
        rowOfWindows => rowOfWindows.map(columnOfWindows =>
          Slide(
            v(layerConfig.kernelWidthHeight).evalInt,
            v(layerConfig.kernelStride).evalInt).
            eval(columnOfWindows)).transpose)
      val inputSlidedIn2Dshape = shape5d(inputSlidedIn2D)

      K.zip(B).map {
        case (kernelW, kernelB) =>
          inputSlidedIn2D.map(slideRow =>
            slideRow.map(slideWindow => {
              val reducedWindow: Float = slideWindow.zip(kernelW).map {
                case (slideWindowRow, kernelRow) => slideWindowRow.zip(kernelRow).map {
                  case (slideWindowElement, kernelElement) => slideWindowElement.zip(kernelElement).map {
                    case (slideWindowElementChannelValue, kernelElementChannelValue) =>
                      slideWindowElementChannelValue * kernelElementChannelValue
                  }.sum // reduce elements
                }.sum // reduce rows
              }.sum // reduce windows

              reducedWindow + kernelB
            }))
      }
    })
  }

  def evalFinalLambda(B: Array[Float],
                      firstLambdaOutput: Array5D[Float],
                      v: scala.collection.immutable.Map[Var, Cst]): Array4D[Float] = {

    // Check parameter values
    assert(layerConfig.paramVector.forall(param => v.contains(param)))
    assert(tuneParams.paramVector.forall(param => v.contains(param)))

    val partReducedXTypeLengths: Seq[Int] = Type.getLengths(
      Type.substitute(partReducedXType,
        v.asInstanceOf[scala.collection.immutable.Map[ArithExpr, ArithExpr]])).dropRight(1).map(_.evalInt)

    val reducedXTypeLengths: Seq[Int] = Type.getLengths(
      Type.substitute(reducedXType,
        v.asInstanceOf[scala.collection.immutable.Map[ArithExpr, ArithExpr]])).dropRight(1).map(_.evalInt)

    val resultTypeLengths: Seq[Int] = Type.getLengths(
      Type.substitute(resultType,
        v.asInstanceOf[scala.collection.immutable.Map[ArithExpr, ArithExpr]])).dropRight(1).map(_.evalInt)

    val nWindowsInTileRowInt: Int = ArithExpr.substitute(nWindowsInTileRow, v.toMap).evalInt
    val nTilesInRowInt: Int = ArithExpr.substitute(nTilesInRow, v.toMap).evalInt


    // Check shapes
    assert(shape1d(B).equals(List(
      v(layerConfig.kernelChannels).evalInt)))

    assert(shape5d(firstLambdaOutput).equals(partReducedXTypeLengths))

    val nonformattedResult = firstLambdaOutput.map(XTile =>
      XTile.zip({
        val t = B.sliding(v(tuneParams.nKernelsPerWrg).evalInt, v(tuneParams.nKernelsPerWrg).evalInt).toArray
        t
      }).map(
        outputChannelGroup =>
          outputChannelGroup._1.zip(outputChannelGroup._2).map(outputChannel => {
            outputChannel._1.map(window =>
              window.foldLeft(outputChannel._2)(_ + _)
            )})).flatten
    )

    // Check shapes
    assert(shape3d(nonformattedResult).equals(reducedXTypeLengths))

    // Format results
    val intermediateResult: Array6D[Float] =
      nonformattedResult.map(_.map(_.sliding(
        nWindowsInTileRowInt, nWindowsInTileRowInt).toArray)).
        sliding(nTilesInRowInt, nTilesInRowInt).toArray.
        sliding(nTilesInRowInt, nTilesInRowInt).toArray

    // Move nChannels up
    val intermediateResult2: Array6D[Float] =
      intermediateResult.map(_.map(_.transpose).transpose)

    // Flatten tiles
    val result: Array4D[Float] =
      intermediateResult2.map(_.map(_.map(_.transpose.map(_.flatten)).flatten))

    assert(shape4d(result).equals(resultTypeLengths))

    result
  }
}

object ConvStencil3D extends LayerExpressionFactory {

  val kernelWidthHeightTmp = Var("kernelWidthHeight")
  val kernelStrideTmp = Var("kernelStride")
  class ConvStencil3DLayerConfig(// Input config
                                 val nInputs: Var = Var("nInputs"),
                                 val inputWidthHeight: Var = Var("inputWidthHeight"),
                                 val inputChannels: Var = Var("inputChannels"),
                                 // Layer-specific config
                                 val kernelWidthHeight: Var = kernelWidthHeightTmp,
                                 val kernelChannels: Var = Var("kernelChannels"),
                                 val kernelStride: Var = kernelStrideTmp,
                                 val padFunc: Var = Var("padFunc")
                                ) extends LayerConfig {
    // TODO: handle padding
    val paramVector: Vector[Var] = Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels,
      kernelStride, padFunc)
  }

  /**
    * During lowering, the expression for the Conv layer is created using default variables for OptParams without
    * ranges. During parameter space exploration, they are replaced with constants (Cst()).
    */
//  val tileStrideTmp = Var("tileStride")
  class ConvStencil3DTuneParams(val tileWidthHeight: Var = Var("tileWidthHeight"),//(kernelWidthHeightTmp - kernelStrideTmp) + tileStrideTmp,
//                                val tileStride: Var = Var("tileStride"),

                                val vectorLen: Var = Var("vectorLen"),
                                val nKernelsPerWrg: Var = Var("nKernelsPerWrg"),
                                val seqOpsPerThread: Var = Var("seqOpsPerThread"),

                                val padOpt: Var = Var("padOpt"),

                                val coalesce: Boolean = false,
                                val unrollReduce: Boolean = false) extends LayerTuneParams {
        val paramVector: Vector[Var] = Vector(tileWidthHeight, vectorLen, nKernelsPerWrg, seqOpsPerThread, padOpt)
  }
  /**
    * Produces a convolution expression without an activation function
    */
  def apply(layerConfig: ConvStencil3DLayerConfig,
            tuneParams: ConvStencil3DTuneParams): Seq[Lambda] =
    apply(id, layerConfig, tuneParams)


  /**
    * Produces a convolution expression with an activation function
    */
  def apply(activationF: UserFun,
            layerConfig: ConvStencil3DLayerConfig,
            tuneParams: ConvStencil3DTuneParams): Seq[Lambda] =
    new ConvStencil3D(layerConfig, tuneParams)(activationF)
}