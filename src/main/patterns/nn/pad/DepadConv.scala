package patterns.nn.pad

import ir.ast.debug.AssertType
import ir.ast.{Join, Lambda, PadConstant2D, Split, Value, λ}
import ir.{ArrayType, ArrayTypeWSWC, Type}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.generator.NDRange
import opencl.ir.pattern.MapGlb
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DTuneParams}

class DepadConv(layerConfig: ConvStencil3DLayerConfig,
                tuneParams: ConvStencil3DTuneParams,
                convStencil3D: patterns.nn.conv.ConvStencil3D,
                originalWidth: ArithExpr,
                originalHeight: ArithExpr,
                originalType: Type,
                depadWidth: ArithExpr,
                depadHeight: ArithExpr,
                newType: Type) {
  def AT = ArrayType // alias
  type AT = ArrayType // alias

//  val paddedInputWidthHeight = substituteVars(factory.paddedInputWidthHeight.get, substitutionTable)

  def apply(): Lambda = {
    val newInputWidth = originalWidth - depadWidth
    val newInputHeight = originalHeight - depadHeight

    λ(originalType,
      X => {
        AssertType(newType,
          "Padded X type") o
          ir.ast.Map(Split(newInputWidth)) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(opencl.ir.id))) o ir.ast.Map(Join()) o
          //
          ir.ast.Map(ir.ast.Map(
            PadConstant2D(0, (-1 * depadHeight).evalInt, 0, (-1 * depadWidth).evalInt,
              Value("0", opencl.ir.Float))))  o
          AssertType(originalType, "Nonpadded X type") $ X
      })
  }

  def depaddingLambdaNDRanges(substitutionTable: Map[Var, Cst]): ( /* Local */ NDRange, /* Global */ NDRange) = {
    val newInputWidth = ArithExpr.substitute(
      originalWidth - depadWidth, substitutionTable.toMap)
    val newInputHeight = ArithExpr.substitute(
      originalHeight - depadHeight, substitutionTable.toMap)
    (
      /* Local */ NDRange(1, 1, 1),
      /* Global */ NDRange(
      newInputWidth.evalInt, // Dim 0
      (newInputHeight * substitutionTable(layerConfig.kernelChannels)).evalInt, // Dim 1
      substitutionTable(layerConfig.nInputs).evalInt // Dim 2
    ))
  }
}
