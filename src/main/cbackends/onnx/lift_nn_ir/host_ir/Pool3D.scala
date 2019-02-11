package cbackends.onnx.lift_nn_ir.host_ir

import ir.ast.onnx.AveragePool
import ir.ast.{Expr, FunCall, Join, Param, Slide3D, Slide3D_R, fun}
import lift.arithmetic.Cst
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import opencl.ir.{Float, add, dividedBy, _}

object Pool3D {

  def apply(fc: FunCall, in: Expr) : Expr = {

    val p = fc.f.asInstanceOf[AveragePool]
    val kernel_shape = p.kernel_shape
    assert(kernel_shape.length == 3)

    val counts = kernel_shape.reduce(_*_)

    val steps = p.strides
    assert(steps.length == 3)

    MapSeq( MapSeq ( dividedBy(counts) )) o
        Join() o MapSeq( MapSeq( Join() o MapSeq(
        fun( y =>
          ReduceSeq(add, 0.0f) o
            Join() o Join() $ y )
      ) ) ) o Slide3D_R(kernel_shape(0),steps(0),kernel_shape(1),steps(1),kernel_shape(2),steps(2)) $ in
  }

}
