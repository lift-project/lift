package nn.conv

/**
  * Created by nm on 09/01/17.
  */

import ir.ast._
import nn._

trait ConvCompanion {

  val expectDataShapeWHC: Boolean

  /* Parallel layer */
  def Par(activationF: UserFun, inputShape: Shape, inputTiling: SlidingWindowConfig, nKernels: Int,
          kernelSliding: SlidingWindowConfig,
          kernelsPerGroup: Int, elsPerThread: Int, vectorLen: Int, coalesce: Boolean, 
          unrollReduce: Boolean): Array[FunDecl]

  case class InitParameters(override val layerNo: Int,
                            liftFPropFactory: (UserFun, Shape, SlidingWindowConfig, Int,
                              SlidingWindowConfig, Int, Int, Int, Boolean, Boolean) => Array[FunDecl],
                            activationFun: UserFun,
                            optParams: conv.Experiment.Config.OptimisationalParams,
                            override val inputShape: Shape,
                            dim: conv.Experiment.Config.Dimensions,
                            padData: Boolean, testConfigFilename: String)
    extends Layer.InitParameters(layerNo, inputShape) {
    override def toString: String =
      f"\nconv.InitParameters(layerNo = $layerNo%d, optParams = " + optParams.toString + "," +
        f"\ninputShape = " + inputShape.toString +
        f"\ndim = " + dim.toString + "," +
        f"\npadData = $padData%b," +
        f"\ntestConfigFilename = $testConfigFilename%s)"}


  def exceptionMsgPrefix(iP: InitParameters): String = "In the Conv layer with the following configuration:\n" +
    conv.configToString(iP.inputShape, Shape(), nn.conv.Experiment.Config(iP.dim, iP.optParams))


  def apply(iP: InitParameters): Conv

  /* Padding */
  def pad(inputs: PaddedArray[Array5D[Float]], inputShape: Shape): Unit
}


/**
  * The companion object that contains the Lift expressions, configuration preprocessing and
  * verification, and helper functions.
  */
/**
  * Case class for storing the layer configuration.
  * Configuration is to be preprocessed and verified by the companion object below.
  * @param liftFProp
  * @param inputShape
  * @param outputShape* @param inputTiling
  * @param kernelSliding
  * @param elsPerThread
  * @param kernelsPerGroup
  * @param vectorLen
  * @param coalesce
  * @param localSize
  * @param globalSize
  */
abstract class Conv(val liftFProp: Array[FunDecl],
                    val inputShape: Shape, val outputShape: Shape,
                    val inputTiling: SlidingWindowConfig, val kernelSliding: SlidingWindowConfig,
                    val elsPerThread: Int, val kernelsPerGroup: Int, val vectorLen: Int, 
                    val coalesce: Boolean, val unrollReduce: Boolean,
                    val localSize: Array[Int], val globalSize: Array[Int]) extends Layer {
  var runtime: Double

  override def toString: String =
    conv.configToString(inputShape, outputShape, nn.conv.Experiment.Config(
      conv.Experiment.Config.Dimensions(
        nKernels = kernelSliding.nChannels,
        kernelSize = kernelSliding.size,
        kernelStride = kernelSliding.stride),
      conv.Experiment.Config.OptimisationalParams(
        inputTileSize = inputTiling.size,
        elsPerThread = elsPerThread,
        kernelsPerGroup = kernelsPerGroup,
        vectorLen = vectorLen,
        coalesce = coalesce,
        unrollReduce = unrollReduce)))

  def groupAndUnpad(outputsFlat: Array[Float], datasets: NetDatasets): Unit
}