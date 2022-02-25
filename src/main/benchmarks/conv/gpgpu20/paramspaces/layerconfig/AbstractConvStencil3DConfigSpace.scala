package benchmarks.conv.gpgpu20.paramspaces.layerconfig

import lift.arithmetic.{ArithExpr, Cst, Var}
import benchmarks.conv.gpgpu20
import benchmarks.conv.gpgpu20.ConvStencil3D.ConvStencil3DLayerConfig

abstract class AbstractConvStencil3DConfigSpace(val configSpace: ConvStencil3DLayerConfig,
                                                constraints: gpgpu20.paramspaces.ParamConstraints)
  extends LayerConfigSpace(configSpace = configSpace, constraints) {

  private val outputWidthHeight: (String, ArithExpr) = ("outputWidthHeight", {
    val iWH = configSpace.inputWidthHeight
//    val pWH = configSpace.padWidthHeight
    val kWH = configSpace.kernelWidthHeight
    val kC = configSpace.kernelChannels

//    (iWH + 2 * pWH - (kWH - kC)) / kC
    Cst(0) // TODO: include padFunc and padOptTotal
  })

  override val dependentParameters: scala.Option[Map[String, ArithExpr]] = Some(Map(
    outputWidthHeight._1 -> outputWidthHeight._2))
}
