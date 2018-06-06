import ir.ast._
import ir.ast.debug.AssertType
import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import lift.arithmetic.SizeVar
import nn.AT
import opencl.executor.Compile
import opencl.ir._
import opencl.ir.pattern._

/**
  * Created by nm on 11/04/18.
  */

object dontweAll {
  def AT = ArrayType
  type AT = ArrayType


//  def Thing(): FunDecl =
//    λ(
//        AT(AT(Float, 1), 2304),
//        AT(Float, 2304),
//        /* w */AT(AT(Float, 1), 2304),
//      (window, acc, w) =>
//        MapGlb(λ(
//          TupleType(
//            AT(Float, 2304),
//            AT(Float, 2304),
//            Float),
//          (tileAndWeightsAndAcc) => {
//            toGlobal(MapSeq(id)) o
//              ReduceSeq(λ((acc, y) =>
//
//                /********* Reducing window tile BEGIN *********/
//
//                multAndSumUp.apply(acc, /* X */ Get(y, 0), /* weights */ Get(y, 1))),
//                toPrivate(id) $ /* accumulator */ Get(tileAndWeightsAndAcc, 2)) $
//              Zip(Get(tileAndWeightsAndAcc, 0), Get(tileAndWeightsAndAcc, 1))
//
//            /********* Reducing window tile END *********/
//          })) $ Zip(window, w, acc))
val N = SizeVar("N")
  def Thing(): FunDecl =
    fun(
      ArrayTypeWSWC(Float, N),
      x =>
        Map(plusOne) o Let(y => Map(id) $ y) $ x
    )
//λ(
//  AT(Float, 1),
//  TupleType(
//    /* w */AT(Float, 1),
//    /* b */Float),
//  (window, params) =>
//    MapSeq(λ(a =>
//      add.apply(/* X */ Get(a, 0), /* weights */ Get(a, 1)))) $ Zip(window, Get(params, 0)))

//  val nTilesInRow = 56
//  val nTilesInCol = nTilesInRow
//
//  val nWindowsInTileRow = 1
//  val nWindowsInTileCol = nWindowsInTileRow
//
//  val nSeqTilesInWindow = 256 * 3 * 3 / 1
//
//  val nWindowsInTile = nWindowsInTileRow * nWindowsInTileCol
//
//  val nTilesTotal = nTilesInRow * nTilesInCol * 1 * 1
//  
//  val seqElsPerThread = 1
//
//  /*********** Types ***********/
//  //    val single_element_type: AT = AT(Float, inputShape.nChannels)
//
//  //    val sliding_window_type: AT = AT(single_element_type, sliding.size * sliding.size)
//
//  val windowSeqTileType: AT = AT(Float, seqElsPerThread)
//  val windowType: AT = AT(windowSeqTileType, nSeqTilesInWindow)
//
//  val nonSeqTiledWindowType: AT = AT(Float, 256 * sliding.size * sliding.size)
//
//  val partReducedWindowType: AT = AT(Float, 2304)//nSeqTilesInWindow)
//
//
//  val xTileType: AT = AT(windowType, nWindowsInTile)
//  val partReducedXTileType: AT = AT(partReducedWindowType, 1)//nWindowsInTile)
//
//
//  val partReducedKernelGroupType: AT = AT(partReducedXTileType, nKernelsPerWrg)
//
//  val partReducedKernelsType: AT = AT(partReducedKernelGroupType, nKernels / nKernelsPerWrg)
//
//
//  val transformedXType: AT = AT(xTileType, nTilesTotal)
//
//  val partReducedXType: AT = AT(partReducedKernelsType, nTilesTotal)
//
//  val xType: AT = AT(AT(AT(AT(AT(Float, inputShape.nChannels), inputShape.sizePadded), inputShape.sizePadded),
//    inputShape.nInputs), inputShape.nBatches)
//
//
//  //    val paramsType: TupleType = TupleType(/* weights */windowType, /* bias */Float)
//  val paramsType: AT = /* weights */windowType
//  val weightsNoInTuple = 0
//  val biasNoInTuple = 1
//  val paramsGroupType: AT = AT(paramsType, nKernelsPerWrg)
//  val allParamsType: AT = AT(paramsGroupType, nKernels / nKernelsPerWrg)
//  
//    def Thing(): FunDecl =
//      λ(
//          AT(AT(Float, 1), 2304),
//          AT(Float, 2304),
//          /* w */AT(AT(Float, 1), 2304),
//        (window, acc, w) =>
//  AssertType(partReducedXTileType, "Part reduced X tile type") o
//    MapSeq(λ(TupleType(windowType, partReducedWindowType), (windowAndAcc) =>
//
//      /******* Sliding window BEGIN *******/
//
//      AssertType(partReducedWindowType, "Part reduced window type") o
//        /* Remove the one-sized dimension introduced by Reduce */
//        Join() o
//        MapSeq(λ(TupleType(windowSeqTileType, windowSeqTileType, Float),
//          (tileAndWeightsAndAcc) => {
//            toGlobal(MapSeq(id)) o
//              ReduceSeq(λ((acc, y) =>
//
//                /********* Reducing window tile BEGIN *********/
//
//                multAndSumUp.apply(acc, /* X */ Get(y, 0), /* weights */ Get(y, 1))),
//                toPrivate(id) $ /* accumulator */ Get(tileAndWeightsAndAcc, 2)) $
//              Zip(Get(tileAndWeightsAndAcc, 0), Get(tileAndWeightsAndAcc, 1))
//
//            /********* Reducing window tile END *********/
//          })) $ Zip(Get(windowAndAcc, 0), /*Get(params, weightsNoInTuple)*/params, Get(windowAndAcc, 1))
//
//      /******* Sliding window END *******/
//    )) $ Zip(AssertType(xTileType) $ tile, Value("0.0f", partReducedXTileType)))

  
  //TODO: Reproduce
  /*
  MapWrg(
    MapWrg(
      MapLcl(
        MapLcl()
      Value())))
  */
//  def LayerPartial =
//    λ((K, X) => {
//        /*** Layer BEGIN ***/
//
//        MapWrg(2)(λ(originalXType, (XUnwrapped) => {
//
//          AssertType(partReducedXType, "Part reduced X type") o
//            MapWrg(1)(λ(xTileType, (XTile) => {
//              /*** Tile BEGIN ***/
//
//              AssertType(partReducedXTileType, "Part reduced X XTile type") o
//                MapWrg(0)(λ(kernelWGroupType, (kernelWGroup) => {
//                  /***** Output channel group BEGIN *****/
//
//                  AssertType(partReducedOutChannelGroupType, "Part reduced X output channel group type") o
//                    MapLcl(2)(λ(kernelWWindowType, (kernelWWindow) => {
//                      /***** Output channel BEGIN *****/
//
//                      AssertType(partReducedOutChannelType, "Part reduced X output channel type") o
//                        MapLcl(1)(λ(windowType, (window) =>
//                          /******* Sliding window BEGIN *******/
//
//                          AssertType(partReducedWindowType, "Part reduced windo+ type") o
//                            Join() o
//                            MapLcl(0)(λ(TupleType(windowSeqTileType, windowSeqTileType),
//                              (SeqTileAndWeightsAndAcc) => {
//                                ReduceWindowTile() $ SeqTileAndWeightsAndAcc
//                                /********* Reducing window tile END *********/
//                              })) $ Zip(window, kernelWWindow)
//
//                          /******* Sliding window END *******/
//                        )) o AssertType(xTileType) $ XTile
//
//                      /***** Output channel END *****/
//                    })) o AssertType(kernelWGroupType, "Kernel weights group type") $ kernelWGroup
//
//                  /***** Output channel group END *****/
//                })) o AssertType(kernelWType, "All kernel weights type after split") o
//                Split(nKernelsPerWrg) o Map(TileAndCoalesce() o Join() o Map(Join())) $ K
//
//              /*** Tile END ***/
//            })) o SlideX() $ XUnwrapped
//
//          // Wrap X into an array of 1
//        })) o Split(inputShape.nBatches) $ X
//
//        /*** Layer END ***/
//      })
    
  def main(args: Array[String]): Unit = {
    println(Compile(Thing))
  }

}