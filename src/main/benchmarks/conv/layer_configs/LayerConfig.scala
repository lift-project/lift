package benchmarks.conv.layer_configs

import com.typesafe.scalalogging.Logger
import lift.arithmetic.{ArithExpr, Cst, Var}

import scala.collection.immutable.ListMap

object LayerConfigFactory {
  val inputChannels: Var = Var("inputChannels")
  val inputWidth: Var = Var("inputWidth")
  val inputHeight: Var = Var("inputHeight")
  val padFuncX: Var = Var("padFuncX")
  val padFuncY: Var = Var("padFuncY")
  val kernelWidth: Var = Var("kernelWidth")
  val kernelHeight: Var = Var("kernelHeight")
  val kernelStrideX: Var = Var("kernelStrideX")
  val kernelStrideY: Var = Var("kernelStrideY")
  val numKernels: Var = Var("numKernels")

  /** Derivative formulas */
  val outputWidthDepadded: ArithExpr = (inputWidth + padFuncX * 2 - (kernelWidth - kernelStrideX)) / kernelStrideX
  val outputHeightDepadded: ArithExpr = (inputHeight + padFuncY * 2 - (kernelHeight - kernelStrideY)) / kernelStrideY

  val layerConfigParams: Vector[Var] = Vector(
    inputChannels, inputWidth, inputHeight, padFuncX, padFuncY,
    kernelWidth, kernelHeight, kernelStrideX, kernelStrideY, numKernels)

  /* Macro names for the C++ test harness */
  val macroNamesForConfigVars: Predef.Map[Var, String] = collection.immutable.ListMap[Var, String](
    inputChannels -> "INPUT_CHANNELS", inputWidth -> "INPUT_WIDTH", inputHeight -> "INPUT_HEIGHT",
    padFuncX -> "PAD_FUNC_X", padFuncY -> "PAD_FUNC_Y",
    kernelWidth -> "KERNEL_WIDTH", kernelHeight -> "KERNEL_HEIGHT",
    kernelStrideX -> "KERNEL_STRIDE_X", kernelStrideY -> "KERNEL_STRIDE_Y",
    numKernels -> "NUM_KERNELS")

  /* Nicely formatted parameter value string for debugging */
  def configParamValuesToString(paramValues: Predef.Map[Var, Cst]): String =
    s"""inputChannels -> ${paramValues(inputChannels).c}, inputWidth -> ${paramValues(inputWidth).c}, inputHeight -> ${paramValues(inputHeight).c},
       |padFuncX -> ${paramValues(padFuncX).c}, padFuncY -> ${paramValues(padFuncY).c},
       |kernelWidth -> ${paramValues(kernelWidth).c}, kernelHeight -> ${paramValues(kernelHeight).c},
       |kernelStrideX -> ${paramValues(kernelStrideX).c}, kernelStrideY -> ${paramValues(kernelStrideY).c},
       |numKernels -> ${paramValues(numKernels).c}""".stripMargin
}

case class LayerConfig(values: ListMap[Var, Cst],
                       uid: String)

trait NetConfig {
  private val logger = Logger(this.getClass)

  val name: String
  protected val layerConfigs: Predef.Map[Int, LayerConfig]
  final def nLayers: Int = layerConfigs.size

  def getLayerConfig(layerConfigIdx: Int): LayerConfig = {
    logger.info(f"Getting layer configuration parameter value combination ${name}-$layerConfigIdx " +
      f"with hash code ${layerConfigs(layerConfigIdx).uid}")

    layerConfigs(layerConfigIdx)
  }

  def getLayerIds: Set[Int] = layerConfigs.keySet
}

object NetConfig {
  def apply(netName: String): NetConfig = {
    netName match {
      case s if s.equals(VGG.name) => VGG
//      case s if s.equals(Synthetic.name) => Synthetic
      case s => throw new IllegalArgumentException(s"Unknown network name '$s'")
    }
  }
}