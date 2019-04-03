package nn.conv

/**
  * Created by nm on 09/01/17.
  */

import ir.ast._
import nn.{Layer, _}
import patterns.nn.Array5D

/**
  * The companion object that contains the Lift expressions, configuration preprocessing and
  * verification, and helper functions.
  */
trait ConvCompanion {
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


  def apply(iP: InitParameters): Conv

  /* Padding */
  def pad(inputs: PaddedArray[Array5D[Float]], inputShape: Shape): Unit
}


/**
  * Case class for storing the layer configuration.
  * Configuration is to be preprocessed and verified by the companion object below.
  * @param liftFProp        an activation function (e.g. Linear, ReLU, etc)
  * @param inputShape       configuration of input data
  * @param outputShape      configuration of output data
  * @param inputTiling      configuration of input data overlapping tiling (tile, size, stride, etc)
  * @param kernelSliding    configuration of convolutional kernel sliding (tile, size, stride, etc)
  * @param elsPerThread     the number of input elements (pixels) to process (weigh) sequentially within a thread
  * @param kernelsPerGroup  the number of convolutional kernels (features) to process within a workgroup
  * @param vectorLen        the length of vectors that input data and weights are split into for memory accesses and computations
  * @param coalesce         switch to enable coalesced memory accesses
  * @param localSize        number of work items in respective dimensions of work groups
  * @param globalSize       number of work items in respective dimension of the global space
  */
abstract class Conv(val liftFProp: Array[FunDecl],
                    val inputShape: Shape, val outputShape: Shape,
                    val inputTiling: SlidingWindowConfig, val kernelSliding: SlidingWindowConfig,
                    val elsPerThread: Int, val kernelsPerGroup: Int, val vectorLen: Int, 
                    val coalesce: Boolean, val unrollReduce: Boolean,
                    val localSize: Array[Int], val globalSize: Array[Int]) extends Layer {
  var runtime: Double

  def groupAndUnpad(outputsFlat: Array[Float], datasets: NetDatasets): Unit
}