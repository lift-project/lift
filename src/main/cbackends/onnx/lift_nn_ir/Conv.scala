package cbackends.onnx.lift_nn_ir

import ir.{ArrayType, ast}
import ir.ast.{Expr, FunCall, fun}
import lift.arithmetic.SizeVar
import opencl.ir.pattern.MapSeq
import opencl.ir.{Float, add, _}

object Conv {

  val incrementF = ast.fun(Float, x => add(Float).apply(1f, x))

  val N = SizeVar("N")

  def Seq( ): Expr = {
    FunCall(MapSeq(incrementF), 1)
  }


}
