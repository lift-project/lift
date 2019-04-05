package patterns.nn.pad

import exploration.ParameterRewrite.substituteVars
import ir.{ArrayType, ArrayTypeWSWC}
import ir.ast.debug.AssertType
import ir.ast.{Join, Lambda, PadConstant2D, Split, Value, λ}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.generator.NDRange
import opencl.ir.Float
import opencl.ir.pattern.MapGlb
import patterns.nn.conv.ConvStencil3D
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DTuneParams}

object Pad {
  def AT = ArrayType // alias
  type AT = ArrayType // alias

//  val paddedInputWidthHeight = substituteVars(factory.paddedInputWidthHeight.get, substitutionTable)

  def apply(layerConfig: ConvStencil3DLayerConfig,
            tuneParams: ConvStencil3DTuneParams,
            convStencil3D: patterns.nn.conv.ConvStencil3D,
            padSize: Int): Lambda = {
    val nonpaddedXType = AT(AT(AT(AT(Float,
      layerConfig.inputChannels),
      layerConfig.inputWidthHeight),
      layerConfig.inputWidthHeight),
      layerConfig.nInputs)

    λ(nonpaddedXType,
      X => {
        AssertType(convStencil3D.originalXType.get,
          "Padded X type") o
          Split(convStencil3D.paddedInputWidthHeight.get) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(opencl.ir.id))) o Join() o
          //
          ir.ast.Map(
            PadConstant2D(padSize, padSize, padSize, padSize,
              Value("0",
                ArrayTypeWSWC(opencl.ir.Float, layerConfig.inputChannels)))) o
          AssertType(nonpaddedXType, "Nonpadded X type") $ X
      })
  }

  def paddingLambdaNDRanges(substitutionTable: Map[Var, Cst],
                            layerConfig: ConvStencil3DLayerConfig,
                            convStencil3D: patterns.nn.conv.ConvStencil3D): ( /* Local */ NDRange, /* Global */ NDRange) = {
    val paddedInputWidthHeight = ArithExpr.substitute(
      convStencil3D.paddedInputWidthHeight.get, substitutionTable.toMap)
    (
      /* Local */ NDRange(1, 1, 1),
      /* Global */ NDRange(
      substitutionTable(layerConfig.inputChannels).evalInt, // Dim 0
      paddedInputWidthHeight.evalInt, // Dim 1
      (paddedInputWidthHeight * substitutionTable(layerConfig.nInputs)).evalInt // Dim 2
    ))
  }
}
