import ir.ast._
import ir.{ArrayType, Type}
import opencl.executor.Compile
import opencl.ir._
import opencl.ir.pattern._

/**
  * Created by nm on 11/04/18.
  */

object dontweAll {
  def AT = ArrayType
  type AT = ArrayType

//  Starting the experiment:
//    nBatches=1, nInputs=1, imageSize=226
//
//  Layer 0 (Conv):
//    inputSizePadded=226, outputSizePadded=224,
//  inputTileSize=10, kernelSize=3, kernelStride=1,
//  elsPerThread=27, nKernels=64, kernelsPerGroup=4,
//  vectorLen=1, coalesce=false, unrollReduce=true

  val seqElsPerThread = 27
  val vectorLen = 1
  val unrollReduce = true
  val coalesce = false
  val nKernelsPerWrg = 4
  val nTilesInRow = 28 //(226 - (tiling.size - tiling.stride)) / tiling.stride
  val nTilesInCol: Int = nTilesInRow
  val nTilesInInput: Int = nTilesInCol * nTilesInRow

  val nWindowsInTileRow: Int = 8 //(tiling.size - (2 - sliding.stride)) / sliding.stride
  val nWindowsInTileCol: Int = nWindowsInTileRow

  val nWindowsInRow: Int = nTilesInRow * nWindowsInTileRow // Output W
  val nWindowsInCol: Int = nTilesInCol * nWindowsInTileCol // Output Y 

  val nElementsInWindow: Int = 3 * 3 * 3

  val nSeqTilesInWindow: Int = nElementsInWindow / seqElsPerThread

  val nWindowsInTile: Int = nWindowsInTileCol * nWindowsInTileRow

  val nTilesTotal: Int = 1 * nTilesInInput

  val nKernelGroups: Int = 64 / 4
  val originalElementType: Float.type = Float
  val originalFlatWindowType: AT = AT(originalElementType, nElementsInWindow)
  val originalXType: AT = AT(AT(AT(AT(AT(originalElementType, 3), 226), 226), 1), 1)

  //    val elementType: Type = if (vectorLen == 1) Float else VectorType(Float, vectorLen)
  val elementType: Type = if (vectorLen == 1) Float else AT(Float, vectorLen)
  val windowSeqTileType: AT = AT(elementType, seqElsPerThread / vectorLen)
  val windowType: AT = AT(windowSeqTileType, nSeqTilesInWindow)
  
  val kernelWWindowType: AT = /* weights */windowType
  val kernelWGroupType: AT = AT(kernelWWindowType, nKernelsPerWrg)

  def ReduceSeqMaybeUnroll(f: Lambda2, init: Expr): Lambda1 =
    if (unrollReduce) ReduceSeqUnroll(f, init)
    else ReduceSeq(f, init)
  
  def dotAndSumUp: UserFun = UserFun("dotAndSumUp", Array("acc", "l", "r"),
    "{ return acc + dot(l, r); }",
    vectorLen match {
      case 2 => Seq(Float, Float2, Float2)
      case 3 => Seq(Float, Float3, Float3)
      case 4 => Seq(Float, Float4, Float4)
      case _ => throw new NotImplementedError("dotAndSumUp() does not support vectors of size " + vectorLen)
    }, Float)
  
  def vectoriseNonContiguous(vectorLen: Int): UserFun = {
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
        asVector(vectorLen) $ arr
    })}

  def BadKiddo(): FunDecl =
    fun(AT(Float, 10), AT(Float, 10), (window, kernelWWindow) =>
        MapSeq(λ((SeqTileAndWeightsAndAcc) => {
            toGlobal(id) $ add(Get(SeqTileAndWeightsAndAcc, 0), Get(SeqTileAndWeightsAndAcc, 1))

          })) $ Zip(window, kernelWWindow))
    
  def main(args: Array[String]): Unit = {
//    val size = nSeqTilesInWindow * (seqElsPerThread / vectorLen)
//    val input1 = Array.fill(size)(0.0f)//util.Random.nextFloat())
//    val input2 = Array.fill(nKernelsPerWrg * size)(0.0f)//util.Random.nextFloat())
    val input1 = Array.fill(10)(0.0f)//util.Random.nextFloat())
    val input2 = Array.fill(10)(0.0f)//util.Random.nextFloat())
    //        println(Compile(Problematic))
//    val (output, _) = Execute(1, 1)[Array[Float]](BadKiddo, input1, input2)
    println(Compile(BadKiddo))
  }

}