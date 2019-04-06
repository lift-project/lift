package patterns.nn.pad

import ir.ast.debug.AssertType
import ir.ast.{Join, Lambda, PadConstant2D, Split, Value, λ}
import ir.{ArrayType, ArrayTypeWSWC}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.generator.NDRange
import opencl.ir.Float
import opencl.ir.pattern.MapGlb
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DTuneParams}

class PadConv(layerConfig: ConvStencil3DLayerConfig,
              tuneParams: ConvStencil3DTuneParams,
              convStencil3D: patterns.nn.conv.ConvStencil3D,
              originalSize: ArithExpr,
              padSize: ArithExpr) {
  def AT = ArrayType // alias
  type AT = ArrayType // alias

//  val paddedInputWidthHeight = substituteVars(factory.paddedInputWidthHeight.get, substitutionTable)

  def apply(): Lambda = {
    val newInputWidthHeight = originalSize + 2 * padSize

    val nonpaddedXType = AT(AT(AT(AT(Float,
      layerConfig.inputChannels),
      originalSize),
      originalSize),
      layerConfig.nInputs)

    val paddedXType = AT(AT(AT(AT(Float,
      layerConfig.inputChannels),
      newInputWidthHeight),
      newInputWidthHeight),
      layerConfig.nInputs)

    λ(nonpaddedXType,
      X => {
        AssertType(paddedXType,
          "Padded X type") o
          Split(newInputWidthHeight) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(opencl.ir.id))) o Join() o
          //
          ir.ast.Map(
            PadConstant2D(padSize.evalInt, padSize.evalInt, padSize.evalInt, padSize.evalInt,
              Value("0",
                ArrayTypeWSWC(opencl.ir.Float, layerConfig.inputChannels)))) o
          AssertType(nonpaddedXType, "Nonpadded X type") $ X
      })
  }

  def paddingLambdaNDRanges(substitutionTable: Map[Var, Cst]): ( /* Local */ NDRange, /* Global */ NDRange) = {
    val newInputWidthHeight = ArithExpr.substitute(
      originalSize + 2 * padSize, substitutionTable.toMap)
    (
      /* Local */ NDRange(1, 1, 1),
      /* Global */ NDRange(
      substitutionTable(layerConfig.inputChannels).evalInt, // Dim 0
      newInputWidthHeight.evalInt, // Dim 1
      (newInputWidthHeight * substitutionTable(layerConfig.nInputs)).evalInt // Dim 2
    ))
  }
}
