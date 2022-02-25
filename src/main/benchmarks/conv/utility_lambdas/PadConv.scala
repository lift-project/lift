package benchmarks.conv.utility_lambdas

import ir.ast.debug.AssertType
import ir.ast.{Lambda, PadConstant2D, Value, fun}
import ir.{ArrayType, Type}
import lift.arithmetic.ArithExpr
import opencl.generator.NDRange
import opencl.ir.pattern.MapGlb

class PadConv(inputChannels: ArithExpr,
              inputWidth: ArithExpr,
              inputHeight: ArithExpr,

              padFuncX: ArithExpr,
              padFuncY: ArithExpr,
              padOptRight: ArithExpr,
              padOptBottom: ArithExpr,

              originalType: Type,
              newType: Type,
              newDataLayout: Boolean = true) {
  def AT = ArrayType // alias
  type AT = ArrayType // alias

  val newInputWidth: ArithExpr = inputWidth + 2 * padFuncX + padOptRight
  val newInputHeight: ArithExpr = inputHeight + 2 * padFuncY + padOptBottom

  val f: Lambda =
    fun(originalType,
      X => {
        AssertType(newType,
          "Padded X type") o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(opencl.ir.id))) o
          //
          (if (newDataLayout)
            ir.ast.Map(
              PadConstant2D(
                top = padFuncY.evalInt,
                bottom = padFuncY.evalInt + padOptBottom.evalInt,
                left = padFuncX.evalInt,
                right = padFuncX.evalInt + padOptRight.evalInt,
                //              Value("0",
                //                ArrayTypeWSWC(opencl.ir.Float, inputChannels))
                Value("0", opencl.ir.Float)

              )) else
            PadConstant2D(
              top = padFuncY.evalInt,
              bottom = padFuncY.evalInt + padOptBottom.evalInt,
              left = padFuncX.evalInt,
              right = padFuncX.evalInt + padOptRight.evalInt,
              //              Value("0",
              //                ArrayTypeWSWC(opencl.ir.Float, inputChannels))
              Value("0", ArrayType(opencl.ir.Float, inputChannels))
            )) o
//
          AssertType(originalType, "Nonpadded X type") $ X
      })

  val paddingLambdaNDRanges: ( /* Local */ NDRange, /* Global */ NDRange) =
    (
      /* Local */ NDRange(1, 1, 1),
      if (newDataLayout)
      /* Global */ NDRange(
        newInputWidth.evalInt, // Dim 0
        newInputHeight.evalInt, // Dim 1
        inputChannels.evalInt // Dim 2
      )
      else
      /* Global */ NDRange(
        inputChannels.evalInt, // Dim 0
        newInputWidth.evalInt, // Dim 1
        newInputHeight.evalInt)) // Dim 2
}
