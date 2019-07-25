package patterns.nn.conv

import _root_.utils._
import ir.ast.debug.AssertType
import ir.ast.{Expr, FunDecl, Get, Join, Lambda, Lambda2, Let, Map, RewritingGuidePost, Slide, Split, TiledSlidedND, Transpose, TransposeW, UserFun, Value, Zip, λ}
import ir.{ArrayType, ArrayTypeWSWC, TupleType, Type}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.ir._
import opencl.ir.pattern._
import patterns.nn._
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DRewriteParams, ConvStencil3DTuneParams}
import patterns.nn.utils.Utils.slidingOutputSize


class ConvStencil3D(layerConfig: ConvStencil3DLayerConfig,
                    tuneParams: ConvStencil3DTuneParams,
                    rewriteParams: ConvStencil3DRewriteParams,
                    fuseLambdas: Boolean,
                    shareKernels: Boolean)
  extends LayerExpression(layerConfig, tuneParams, rewriteParams) {

  def AT = ArrayType // alias
  type AT = ArrayType // alias

  /*********** Constants ***********/
  val paddedInputWidthHeight = layerConfig.inputWidthHeight + 2 * layerConfig.padFunc + tuneParams.padOptTotal

  val tileStride = tuneParams.tileWidthHeight - (layerConfig.kernelWidthHeight - layerConfig.kernelStride)
  //    val tileStride = tuneParams.tileStride
  val nTilesInRow = slidingOutputSize(
    paddedInputWidthHeight,
    tuneParams.tileWidthHeight,
    tileStride)

  val nTilesInCol = nTilesInRow
  val nTilesInInput = nTilesInCol * nTilesInRow

  val nWindowsInTileRow = slidingOutputSize(
    tuneParams.tileWidthHeight,
    layerConfig.kernelWidthHeight,
    layerConfig.kernelStride)
  val nWindowsInTileCol = nWindowsInTileRow

  val nWindowsInRow = nTilesInRow * nWindowsInTileRow // Output W
  val nWindowsInCol = nTilesInCol * nWindowsInTileCol // Output Y

  val chunkSize = tuneParams.seqOpsPerThread

  val nFloatsInWindow = layerConfig.kernelWidthHeight * layerConfig.kernelWidthHeight * layerConfig.inputChannels

  val nChunksInWindow = nFloatsInWindow /^ chunkSize

  val nWindowsInTile = nWindowsInTileCol * nWindowsInTileRow

  val nWindowGroupsInTile = nWindowsInTile /^ tuneParams.nWindowsPerThread

  val nTilesTotal = layerConfig.nInputs * nTilesInInput

  val nKernelGroups = layerConfig.kernelChannels /^ tuneParams.nKernelsPerWrg

  val flatPartReducedXSize = nTilesTotal * layerConfig.kernelChannels * nWindowsInTile * nChunksInWindow

  /** ********* Types ***********/
  val originalFlatWindowType: AT = AT(Float, nFloatsInWindow)
  val originalXType = AT(AT(AT(AT(Float,
    layerConfig.inputChannels),
    paddedInputWidthHeight),
    paddedInputWidthHeight),
    layerConfig.nInputs)

  val windowChunkType: AT = AT(Float, chunkSize)
  val windowType: AT = AT(windowChunkType, nChunksInWindow)
  val flatWindowType: AT = AT(Float, nFloatsInWindow)
  val xTileType: AT = AT(flatWindowType, nWindowsInTile)
  val xType: AT = AT(xTileType, nTilesTotal)

  val flatWindowGroupType = AT(flatWindowType, tuneParams.nWindowsPerThread)
  //  val groupedXTileType: AT = AT(windowGroupType, nWindowGroupsInTile)


  val slidedXType: AT = AT(AT(AT(AT(AT(AT(AT(AT(Float,
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


  val partReducedElementType: Float.type = Float
  val partReducedWindowType: AT = AT(partReducedElementType, nChunksInWindow)
  val partReducedOutChannelType: AT = AT(partReducedWindowType, nWindowsInTile) // loc 0
  val partReducedOutChannelGroupType: AT = AT(partReducedOutChannelType, tuneParams.nKernelsPerWrg) // loc 1
  val partReducedXTileType: AT = AT(partReducedOutChannelGroupType, nKernelGroups) // group 0
  val partReducedXType = AT(partReducedXTileType, nTilesTotal) // group 1
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

  val kernelWGroupType: AT = AT(flatWindowType, tuneParams.nKernelsPerWrg)
  val kernelWType: AT = AT(kernelWGroupType, nKernelGroups)

  val kernelBPerWindowType: Float.type = /* bias */ Float
  val kernelBGroupType: AT = AT(kernelBPerWindowType, tuneParams.nKernelsPerWrg)
  val kernelBType: AT = AT(kernelBGroupType, nKernelGroups)


  /** ************************ Parallel layer **************************/
  def apply(activationF: UserFun): List[Lambda] = {
    /** ********* UserFuns ***********/
    def dotAndSumUp = UserFun("dotAndSumUp", Array("acc", "l", "r"),
      "{ return acc + dot(l, r); }", Seq(Float, Float, Float), Float)

    def continue(): Lambda = λ((x) => x)

    def ReduceSeqMaybeUnroll(f: Lambda2, init: Expr) =
      if (tuneParams.unrollReduce) ReduceSeqUnroll(f, init)
      else ReduceSeq(f, init)

    /** **** Helper functions ******/

    /* Produces a tiled slided tiled version of X */
    def SlideX(): FunDecl = {
      λ(originalXType, (X) =>
        AssertType(xType, "SlideX output") o
          // Tile and coalesce
          //          Map(Map(TileAndCoalesce())) o
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

    def vectoriseCoalesceAndSplit(): FunDecl =
      λ(originalFlatWindowType, (window) =>
        RewritingGuidePost("windowType") o AssertType(windowType, "vectoriseCoalesceAndSplit() result type") o
          // Split into chunks
          RewritingGuidePost("chunkOfVectors") o Split(chunkSize) o
          // Coalesce
          {
            if (true/*tuneParams.coalesce*/) RewritingGuidePost("coalescing") else continue()
          } o
          // Vectorise
          RewritingGuidePost("potentialAsVector")
          $ window)


    /** structuriseX() converts the flat output of the previous kernel into 5D array **/
    //    def structuriseX(): FunDecl =
    //      λ(flatPartReducedXType, (X) =>
    //          Split(nKernelGroups) o Split(tuneParams.nKernelsPerWrg) o Split(nWindowsInTile) o
    //          Split(nSeqTilesInWindow) $ X)

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

    val useGlobalMaps: Boolean = System.getenv("USE_GLOBAL_MAPS") != null && System.getenv("USE_GLOBAL_MAPS").toLowerCase == "true"

    val layerPartialSingleWorkgroupLambda: Lambda =
      λ(kernelWGroupType, xTileType,
        (kernelWGroup, XTile) => {
          AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") o
            TransposeW() o
            {val map2 = if (!useGlobalMaps) MapLcl(1) else MapSeq
              map2(λ(flatWindowType, (window) =>
                TransposeW() o

                  {val map3 = if (!useGlobalMaps) MapLcl(0) else MapGlb(0)
                    map3(λ((partialWindowAndPartialKernels) => {

                      {
                        def processPartialWindowsAndPartialKernels(optimiseRegisterUsage: Boolean): Expr = {

                          if (!optimiseRegisterUsage) {
                            // AT(float, nKernelsPerWrg)
                            Let(partialWindow => {

                              val partialKernels = Get(partialWindowAndPartialKernels, 1)

                              Join() o
                                MapSeq(λ((partialKernel) =>
                                  MapSeq(toGlobal(id)) o
                                    ReduceSeq(
                                      λ((acc, y) => {
                                        RewritingGuidePost("vectorisableDotAndSumUp") $
                                          dotAndSumUp(acc, /* X */ Get(y, 0), /* kernelW */ Get(y, 1))
                                      }),
                                      toPrivate(id) $ Value("0.0f", Float)) $ Zip(partialWindow, partialKernel)
                                )) $ partialKernels

                            }) o MapSeq(toPrivate(RewritingGuidePost("vectorisableId") o id)) $
                              Get(partialWindowAndPartialKernels, 0)

                          } else {
                            /* Use less private memory by storing one input value at a time */
                            val partialKernels = Get(partialWindowAndPartialKernels, 1)

                            MapSeq(toLocal /*toGlobal*/(id)) o Join() o
                              ReduceSeq(λ((acc, tupleOfSingleWindowValueAndArrayOfSingleKernelValue) => {

                                Let(singleWindowValue => {

                                  val arrayOfSingleKernelValue = Get(tupleOfSingleWindowValueAndArrayOfSingleKernelValue, 1)

                                  MapSeq(λ((accAndSingleKernelValue) => {
                                    val accSingleValue = Get(accAndSingleKernelValue, 0)
                                    val singleKernelValue = Get(accAndSingleKernelValue, 1)

                                    RewritingGuidePost("vectorisableDotAndSumUp") $
                                      dotAndSumUp(accSingleValue, /* X */ singleWindowValue, /* kernelW */ singleKernelValue)
                                  }
                                  )) $ Zip(acc, arrayOfSingleKernelValue)
                                }) o toPrivate(RewritingGuidePost("vectorisableId") o id) $
                                  Get(tupleOfSingleWindowValueAndArrayOfSingleKernelValue, 0)
                              }),
                                MapSeq(toPrivate(id)) $ Value("0.0f", ArrayTypeWSWC(Float, tuneParams.nKernelsPerWrg))

                              ) $ Zip(
                              Get(partialWindowAndPartialKernels, 0),
                              Transpose() $ partialKernels)
                          }
                        }

                        processPartialWindowsAndPartialKernels(true)
                      }}))} $ Zip(
                  //AT(AT(Float, tuneParams.seqOpsPerThread), nSeqTilesInWindow)
                  vectoriseCoalesceAndSplit() $ window,
                  Transpose() o Map(vectoriseCoalesceAndSplit()) $ kernelWGroup)
              ))}/* o Map(Split(nWindowsPerKernels))*/ $ XTile
        })

    val layerFinalSingleWorkgroupLambda: Lambda =
      λ(kernelBGroupType, partReducedOutChannelGroupType,
        (kernelBGroup, partReducedXTileOutChannelGroup) => {

          {val map2 = if (!useGlobalMaps) MapLcl(1) else MapSeq
            map2(λ(TupleType(partReducedOutChannelType, kernelBPerWindowType), (outputChannel) => {
              /** *** Output channel BEGIN *****/

              AssertType(reducedOutChannelType, "Reduced X output channel type") o
                /* Remove the one-sized dimension introduced by Reduce */
                Join() o
                {val map3 = if (!useGlobalMaps) MapLcl(0) else MapGlb(0)
                  map3(λ(partReducedWindowType, (window) =>

                    /** ***** Sliding window BEGIN *******/

                    /* Map before AssertType is needed because of the one-sized dimension introduced
                       by Reduce */
                    Map(AssertType(reducedWindowType, "Reduced window type")) o
                      MapSeq(toGlobal(id)) o
                      ReduceSeqMaybeUnroll(add, toPrivate(id) o
                        AssertType(kernelBPerWindowType, "Kernel bias per window type") $ Get(outputChannel, 1)
                      ) o AssertType(partReducedWindowType, "Part reduced X window type") $ window

                    /** ***** Sliding window END *******/
                  ))} o AssertType(partReducedOutChannelType) $ Get(outputChannel, 0)

              /** *** Output channel END *****/
            }))} $ Zip(
            AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") $
              partReducedXTileOutChannelGroup,
            AssertType(kernelBGroupType, "Bias group type") $ kernelBGroup)
        })


    /* Share not just input windows among kernels, but also kernels among input windows */
    val layerPartialSingleWorkgroupWithKernelShareLambda: Lambda =
      λ(kernelWGroupType, xTileType,
        (kernelWGroup, XTile) => {
          AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") o
            TransposeW() o Join() o
            {val map2 = if (!useGlobalMaps) MapLcl(1) else MapSeq
              map2(λ(flatWindowGroupType, (windowGroup) =>
                Map(TransposeW()) o TransposeW() o

                  {val map3 = if (!useGlobalMaps) MapLcl(0) else MapGlb(0)
                    map3(λ((partialWindowsAndPartialKernels) => {

                      /* Use less private memory by storing one input value at a time */
                      val partialWindows = Get(partialWindowsAndPartialKernels, 0)
                      val partialKernels = Get(partialWindowsAndPartialKernels, 1)
                      MapSeq(MapSeq(toLocal /*toGlobal*/(id))) o Join() o
                        ReduceSeq(λ((acc, elementwiseTupleOfWindowAndKernelGroups) => {

                          // Preload partial windows and kernels into private memory before the computations
                          Let(elementAcrossWindowGroup => {
                            Let(elementAcrossKernelGroup => {

                              MapSeq(λ((accAndSingleWindowValue) => {
                                MapSeq(λ((accAndSingleKernelValue) => {

                                  val singleAccValue = Get(accAndSingleKernelValue, 0)
                                  val singleWindowValue = Get(accAndSingleWindowValue, 1)
                                  val singleKernelValue = Get(accAndSingleKernelValue, 1)

                                  RewritingGuidePost("vectorisableDotAndSumUp") $
                                    dotAndSumUp(singleAccValue, /* X */ singleWindowValue, /* kernelW */ singleKernelValue)

                                })) $ Zip(Get(accAndSingleWindowValue, 0), elementAcrossKernelGroup)
                              })) $ Zip(acc, elementAcrossWindowGroup)

                            }) o MapSeq(toPrivate(RewritingGuidePost("vectorisableId") o id)) $
                              Get(elementwiseTupleOfWindowAndKernelGroups, 1)
                          }) o MapSeq(toPrivate(RewritingGuidePost("vectorisableId") o id)) $
                            Get(elementwiseTupleOfWindowAndKernelGroups, 0)
                        }),
                          MapSeq(MapSeq(toPrivate(id))) $ Value("0.0f",
                            ArrayTypeWSWC(ArrayTypeWSWC(Float, tuneParams.nKernelsPerWrg), tuneParams.nWindowsPerThread))

                        ) $ Zip(
                        Transpose() $ partialWindows,
                        Transpose() $ partialKernels)

                      }))} $ Zip(
                  Transpose() o Map(vectoriseCoalesceAndSplit()) $ windowGroup,
                  Transpose() o Map(vectoriseCoalesceAndSplit()) $ kernelWGroup)
              ))} o Split(tuneParams.nWindowsPerThread) $ XTile
        })



    val layerPartial: Lambda = {
      λ(AT(AT(AT(AT(Float, layerConfig.inputChannels),
        layerConfig.kernelWidthHeight),
        layerConfig.kernelWidthHeight),
        layerConfig.kernelChannels),
        originalXType,
        (K, X) => {
          {if (fuseLambdas) formatResults() else continue()} o
          /** * Layer BEGIN ***/
          AssertType(partReducedXType, "Part reduced X type") o
            {val map0 = if (!useGlobalMaps) MapWrg(1) else MapGlb(2)
              map0(λ(xTileType, (XTile) => {
                /** * Tile BEGIN ***/

                AssertType(partReducedXTileType, "Part reduced X XTile type") o
                  {val map1 = if (!useGlobalMaps) MapWrg(0) else MapGlb(1)
                    map1(λ(kernelWGroupType, (kernelWGroup) => {
                      /** *** Output channel group BEGIN *****/

                      {if (shareKernels) layerPartialSingleWorkgroupWithKernelShareLambda
                      else layerPartialSingleWorkgroupLambda}.apply(kernelWGroup, XTile)

                      /** *** Output channel group END *****/
                    }))} o AssertType(kernelWType, "All kernel weights type after split") o
                  Split(tuneParams.nKernelsPerWrg) o Map(/*TileAndCoalesce() o */Join() o Map(Join())) $ K

                /** * Tile END ***/
              }))} o SlideX() $ X
          /** * Layer END ***/
        })
    }

    /** ********* F-prop lambda: final part of the reduction ***********/
    val layerFinal: Lambda = {
      λ(AT(Float, layerConfig.kernelChannels), /*flatPartReducedXType*/ partReducedXType,
        (B, X) => {
          /** * Layer BEGIN ***/

          formatResults() o
            {val map0 = if (!useGlobalMaps) MapWrg(1) else MapGlb(2)
              map0(λ(partReducedXTileType, (XTile) => {
                /** * Tile BEGIN ***/

                AssertType(reducedXTileType, "Reduced kernels type") o
                  Join() o
                  {val map1 = if (!useGlobalMaps) MapWrg(0) else MapGlb(1)
                    map1(λ(TupleType(partReducedOutChannelGroupType, kernelBGroupType), (outputChannelGroup) => {
                      /** *** Output channel group BEGIN *****/

                      layerFinalSingleWorkgroupLambda.apply(
                        AssertType(kernelBGroupType, "Bias group type") $ Get(outputChannelGroup, 1),
                        AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") $
                          Get(outputChannelGroup, 0))

                      /** *** Output channel group END *****/
                    }))} $ Zip(
                  AssertType(partReducedXTileType, "Part reduced X tile") $ XTile,
                  AssertType(kernelBType, "All kernel biases type after split") o Split(tuneParams.nKernelsPerWrg) $ B)

                /** * Tile END ***/
              }))} o AssertType(partReducedXType, "Partially reduced X type") /*o structuriseX()*/ $
            X

          /** * Layer END ***/
        })
    }

    val layerFused: Lambda = {
      λ(AT(AT(AT(AT(Float, layerConfig.inputChannels),
        layerConfig.kernelWidthHeight),
        layerConfig.kernelWidthHeight),
        layerConfig.kernelChannels),
        AT(Float, layerConfig.kernelChannels),
        originalXType,
        (K, B, X) => {
          /** * Layer BEGIN ***/

          formatResults() o
            {val map0 = if (!useGlobalMaps) MapWrg(1) else MapGlb(2)
              map0(λ(partReducedXTileType, (XTile) => {
                /** * Tile BEGIN ***/

                AssertType(reducedXTileType, "Reduced kernels type") o
                  Join() o
                  {val map1 = if (!useGlobalMaps) MapWrg(0) else MapGlb(1)
                    map1(λ(TupleType(kernelWGroupType, kernelBGroupType), (outputChannelGroup) => {
                      /** *** Output channel group BEGIN *****/

                      layerFinalSingleWorkgroupLambda.apply(
                        AssertType(kernelBGroupType, "Bias group type") $ Get(outputChannelGroup, 1),
                        AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") $
                          {if (shareKernels) layerPartialSingleWorkgroupWithKernelShareLambda
                          else layerPartialSingleWorkgroupLambda}.apply(Get(outputChannelGroup, 0), XTile))

                      /** *** Output channel group END *****/
                    }))} $ Zip(
//                  AssertType(partReducedXTileType, "Part reduced X tile") $ XTile,
                  AssertType(kernelWType, "All kernel weights type after split") o Split(tuneParams.nKernelsPerWrg) o
                    Map(/*TileAndCoalesce() o */Join() o Map(Join())) $ K,
                  AssertType(kernelBType, "All kernel biases type after split") o Split(tuneParams.nKernelsPerWrg) $ B)

                /** * Tile END ***/
              }))} o SlideX() $ X
          /** * Layer END ***/
    })}

    if (fuseLambdas)
      List(layerFused)
    else
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
        val t = B.iterator.sliding(v(tuneParams.nKernelsPerWrg).evalInt, v(tuneParams.nKernelsPerWrg).evalInt).withPartial(false).toArray.map(_.toArray)
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
      nonformattedResult.map(_.map(_.iterator.sliding(
        nWindowsInTileRowInt, nWindowsInTileRowInt).withPartial(false).toArray.map(_.toArray))).
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
  class ConvStencil3DTuneParams(val tileWidthHeight: Var = Var("tileWidthHeight"),
                                val nKernelsPerWrg: Var = Var("nKernelsPerWrg"),
                                val seqOpsPerThread: Var = Var("seqOpsPerThread"),
                                val nWindowsPerThread: Var = Var("nWindowsPerThread"),

                                val padOptTotal: Var = Var("padOptTotal"),

                                val coalesce: Boolean = false,
                                val unrollReduce: Boolean = false) extends LayerTuneParams {
    val paramVector: Vector[Var] = Vector(
      tileWidthHeight, nKernelsPerWrg, seqOpsPerThread, nWindowsPerThread, padOptTotal)
  }


  class ConvStencil3DRewriteParams(val vectorLen: Var = Var("vectorLen")) extends LayerRewriteParams {
    val paramVector: Vector[Var] = Vector(vectorLen)
  }
  /**
    * Produces a convolution expression without an activation function
    */
  def apply(layerConfig: ConvStencil3DLayerConfig,
            tuneParams: ConvStencil3DTuneParams,
            rewriteParams: ConvStencil3DRewriteParams,
            fuseLambdas: Boolean, shareKernels: Boolean): Seq[Lambda] =
    apply(id, layerConfig, tuneParams, rewriteParams, fuseLambdas, shareKernels)


  /**
    * Produces a convolution expression with an activation function
    */
  def apply(activationF: UserFun,
            layerConfig: ConvStencil3DLayerConfig,
            tuneParams: ConvStencil3DTuneParams,
            rewriteParams: ConvStencil3DRewriteParams,
            fuseLambdas: Boolean, shareKernels: Boolean): Seq[Lambda] =
    new ConvStencil3D(layerConfig, tuneParams, rewriteParams, fuseLambdas, shareKernels)(activationF)
}