package benchmarks.conv.gpgpu20.layeroptimiser.ConvStencil3DOpt

import benchmarks.conv.gpgpu20
import benchmarks.conv.gpgpu20.ConvStencil3D.ConvStencil3DLayerConfig
import benchmarks.conv.gpgpu20.paramspaces.{ParamConstraint, ParamConstraints, ParameterSearch}
import benchmarks.conv.gpgpu20.paramspaces.layerconfig.AbstractConvStencil3DConfigSpace
import benchmarks.conv.gpgpu20.settings.Settings
import com.typesafe.scalalogging.Logger
import exploration.ParamConstraints.lessThanOrEqual
import ir.ast.Lambda
import lift.arithmetic._
import lift.arithmetic.NotEvaluableException._

case class ConvStencil3DConfigSpace(name: String,
                                    pythonFactoryName: String,
                                    private val params: ConvStencil3DLayerConfig,
                                    override val constraints: gpgpu20.paramspaces.ParamConstraints,
//                                    tuneParamConstraints: Vector[ParamConstraint],
                                    settings: benchmarks.conv.gpgpu20.settings.Settings)
  extends AbstractConvStencil3DConfigSpace(params, constraints)


object ConvStencil3DConfigSpace {
  /* Layer configuration name */
  val name = "ConvStencil3DConfigSpace"

  /* ONNX Python factory */
  val pythonFactoryName: String = "conv_layer_factory.py"

  /****** Parameters ******/
  val params = new ConvStencil3DLayerConfig(
//    nInputs = Var("nInputs", RangeMul(1, 2048, mul = 2)),
    nInputs = Var("nInputs", RangeMul(1, 256+1, mul = 2)),
    inputWidthHeight = Var("inputWidthHeight", RangeAdd(2, 224+1, step = 2)),
    inputChannels = Var("inputChannels", RangeMul(1, 2048+1, 2)),

    kernelWidthHeight = Var("kernelWidthHeight", RangeAdd(1, 16+1, 1)),
    kernelChannels = Var("kernelChannels", RangeMul(1, 2048+1, mul = 2)),
    kernelStride = Var("kernelStride", RangeAdd(1, 4+1, 1)),
    padFunc = Var("padFunc", RangeAdd(0, 8+1, 1))) // TODO: explore this parameter


  /****** Parameter validation constraints ******/
    // This should be covered by checking the same with tile size
//  private lazy val kernelSizeToPaddedInputSizeRule =
//    new ParamConstraint(
//      "kernelSizeToPaddedInputSize",
//      "Kernel size should be equal or less than padded input size",
//      Vector(params.kernelWidthHeight, params.inputWidthHeight, params.padFunc),
//      lhs = params.kernelWidthHeight,
//      rhs = params.inputWidthHeight + 2 * params.padFunc,
//      predicate = (lhs: ArithExpr, rhs: ArithExpr) => lessThanOrEqual(lhs, rhs))

  lazy val manualConstraints: Vector[ParamConstraint] = Vector()
//    kernelSizeToPaddedInputSizeRule)

  private val logger = Logger(this.getClass)

  /****** Space constructor ******/
  def apply(lambdas: Seq[Lambda],
            settings: Settings): ConvStencil3DConfigSpace = {

    logger.info("Searching for layer config constraints in lambdas")
    val (_, inferredConstraints0: Vector[ParamConstraint]) = ParameterSearch(lambdas(0))
    val (_, inferredConstraints1: Vector[ParamConstraint]) =
      if (ConvStencil3DTuneParamSpace.fuseLambdas) (Vector(), Vector()) else ParameterSearch(lambdas(1))

    val (layerConfigConstraints0, tuneParamConstraints0) = inferredConstraints0.partition(constraint =>
      constraint.params.forall(param => params.paramVector.contains(param)))

    val (layerConfigConstraints1, tuneParamConstraints1) = inferredConstraints1.partition(constraint =>
      constraint.params.forall(param => params.paramVector.contains(param)))

    val constraintsVector = ParamConstraints.removeDuplicateConstraints(manualConstraints ++
      /*ParameterSpace.rangeBasedConstraints(params.paramVector) ++*/
      layerConfigConstraints0 ++ layerConfigConstraints1)

    new ConvStencil3DConfigSpace(
      name, pythonFactoryName, params,
      ParamConstraints(params.paramVector, constraintsVector),
      /*tuneParamConstraints0 ++ tuneParamConstraints1,*/
      settings)
  }
}