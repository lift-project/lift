package nn.conv

/**
  * Created by nm on 09/01/17.
  */

import ir.ast._
import ir.ast.debug.PrintType
import ir.{ArrayType, TupleType}
import nn._
import opencl.ir._
import opencl.ir.pattern.{ReorderStride, _}

trait ConvCompanion {
  val locA: Int
  val locB: Int
  val locC: Int


  /* Parallel layer */
  def Par(activation_f: UserFun, input_shape: Shape, input_tiling: SlidingWindowConfig, n_kernels: Int,
          kernel_sliding: SlidingWindowConfig,
          kernels_per_group: Int, els_per_thread: Int): FunDecl

  case class InitParameters(override val layerNo: Int,
                            liftFPropGenerator: (UserFun, Shape, SlidingWindowConfig, Int,
                              SlidingWindowConfig, Int, Int) => FunDecl,
                            activationFun: UserFun,
                            optParams: conv.Experiment.Config.OptimisationalParams,
                            //                            elsPerThread: Int,
                            //                            kernelsPerGroup: Int,
                            //                            inputTileSize: Int,
                            //                            nKernels: Int,
                            override val inputShape: Shape,
                            //                            kernelSize: Int,
                            dim: conv.Experiment.Config.Dimensions)
    extends Layer.InitParameters(layerNo, inputShape)


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
  * @param localSize
  * @param globalSize
  */
abstract class Conv(val liftFProp: FunDecl,
                    val inputShape: Shape, val outputShape: Shape,
                    val inputTiling: SlidingWindowConfig, val kernelSliding: SlidingWindowConfig,
                    val elsPerThread: Int, val kernelsPerGroup: Int,
                    val localSize: Array[Int], val globalSize: Array[Int]) extends Layer {
  val configToString: String
  var runtime: Double

  def groupAndUnpad(outputsFlat: Array[Float], datasets: NetDatasets): Unit
}