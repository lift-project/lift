package patterns.nn

import exploration.ParameterRewrite.substituteVars
import lift.arithmetic.{Cst, Var}
import patterns.nn.conv.ConvStencil3D.ConvStencil3DLayerConfig

import scala.util.Random

package object conv {
  def generateTestData(convLambdaFactory: ConvStencil3D,
                       convStencil3DLayerConfig: ConvStencil3DLayerConfig,
                       substitutionTable: Map[Var, Cst],
                       inputSize: Int): (Array4D[Float], Array[Float], Array4D[Float]) = {
    val k: Array4D[Float] = Array.tabulate(
      substitutionTable(convStencil3DLayerConfig.kernelChannels).evalInt,
      substitutionTable(convStencil3DLayerConfig.kernelWidthHeight).evalInt,
      substitutionTable(convStencil3DLayerConfig.kernelWidthHeight).evalInt,
      substitutionTable(convStencil3DLayerConfig.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())
    val b: Array[Float] = Array.tabulate(
      substitutionTable(convStencil3DLayerConfig.kernelChannels).evalInt)(_ => Random.nextFloat())
    val x: Array4D[Float] = Array.tabulate(
      substitutionTable(convStencil3DLayerConfig.nInputs).evalInt,
      inputSize, //substitutionTable(convStencil3DLayerConfig.inputWidthHeight).evalInt,
      inputSize, //substitutionTable(convStencil3DLayerConfig.inputWidthHeight).evalInt,
      substitutionTable(convStencil3DLayerConfig.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())

    (k, b, x)
  }
}
