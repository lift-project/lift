package patterns.nn.pad

import ir.ast.debug.AssertType
import ir.ast.{Join, Lambda, PadConstant2D, Split, Value, λ}
import ir.{ArrayType, ArrayTypeWSWC, Type}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.generator.NDRange
import opencl.ir.Float
import opencl.ir.pattern.MapGlb
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DTuneParams}

class PadConv(layerConfig: ConvStencil3DLayerConfig,
              tuneParams: ConvStencil3DTuneParams,
              convStencil3D: patterns.nn.conv.ConvStencil3D,
              originalSize: ArithExpr,
              originalType: Type,
              padFunc: ArithExpr,
              padOptTotal: ArithExpr,
              newType: Type) {
  def AT = ArrayType // alias
  type AT = ArrayType // alias

//  val paddedInputWidthHeight = substituteVars(factory.paddedInputWidthHeight.get, substitutionTable)

  def apply(): Lambda = {
    val newInputWidthHeight = originalSize + 2 * padFunc + padOptTotal

    λ(originalType,
      X => {
        AssertType(newType,
          "Padded X type") o
          Split(newInputWidthHeight) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(opencl.ir.id))) o Join() o
          //
          ir.ast.Map(
            PadConstant2D(
              top = padFunc.evalInt,
              bottom = padFunc.evalInt + padOptTotal.evalInt,
              left = padFunc.evalInt,
              right = padFunc.evalInt + padOptTotal.evalInt,
              Value("0",
                ArrayTypeWSWC(opencl.ir.Float, layerConfig.inputChannels))

            )) o
          AssertType(originalType, "Nonpadded X type") $ X
      })
  }

  def paddingLambdaNDRanges(substitutionTable: Map[Var, Cst]): ( /* Local */ NDRange, /* Global */ NDRange) = {
    val newInputWidthHeight = ArithExpr.substitute(
      originalSize + 2 * padFunc + padOptTotal, substitutionTable.toMap)
    (
      /* Local */ NDRange(1, 1, 1),
      /* Global */ NDRange(
      substitutionTable(layerConfig.inputChannels).evalInt, // Dim 0
      newInputWidthHeight.evalInt, // Dim 1
      (newInputWidthHeight * substitutionTable(layerConfig.nInputs)).evalInt // Dim 2
    ))
  }
}
