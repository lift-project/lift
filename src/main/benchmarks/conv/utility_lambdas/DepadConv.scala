package benchmarks.conv.utility_lambdas

import ir.ast.debug.AssertType
import ir.ast.{Join, Lambda, PadConstant2D, Split, Value, fun}
import ir.{ArrayType, Type}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.generator.NDRange
import opencl.ir.pattern.MapGlb

class DepadConv(inputWidth: ArithExpr,
                inputHeight: ArithExpr,
                kernelChannels: ArithExpr,

                depadRight: ArithExpr,
                depadBottom: ArithExpr,

                originalType: Type,
                newType: Type,
                newDataLayout: Boolean = true) {
  def AT = ArrayType // alias
  type AT = ArrayType // alias

  val newInputWidth: ArithExpr = inputWidth - depadRight
  val newInputHeight: ArithExpr = inputHeight - depadBottom

  val f: Lambda =
    fun(originalType,
      X => {
        AssertType(newType, "Depadded X type") o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(opencl.ir.id))) o
          //
          // NB: depadding could be achieved without if conditionals, just by not reading all input data
          (if (newDataLayout)
            ir.ast.Map(
              PadConstant2D(0, (-1 * depadBottom).evalInt, 0, (-1 * depadRight).evalInt,
                Value("0", opencl.ir.Float)))
          else
            PadConstant2D(0, (-1 * depadBottom).evalInt, 0, (-1 * depadRight).evalInt,
              Value("0", ArrayType(opencl.ir.Float, kernelChannels)))) o
        //
          AssertType(originalType, "Padded X type before depad") $ X
      })

  val depaddingLambdaNDRanges: ( /* Local */ NDRange, /* Global */ NDRange) =
    (
      /* Local */ NDRange(1, 1, 1),
      if (newDataLayout)
      /* Global */ NDRange(
        newInputWidth.evalInt, // Dim 0
        newInputHeight.evalInt, // Dim 1
        kernelChannels.evalInt // Dim 2
      ) else
      /* Global */ NDRange(
          kernelChannels.evalInt, // Dim 0
          newInputWidth.evalInt, // Dim 1
          newInputHeight.evalInt // Dim 2
        ))
}
