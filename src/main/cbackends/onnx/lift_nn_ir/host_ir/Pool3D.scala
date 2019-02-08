package cbackends.onnx.lift_nn_ir.host_ir

import ir.ast.onnx.AveragePool
import ir.ast.{Expr, FunCall, Join, Param, Slide3D, fun}
import lift.arithmetic.Cst
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import opencl.ir.{Float, add, dividedBy, _}

object Pool3D {

  def apply(fc: FunCall, in: Expr) : Expr = {

    val p = fc.f.asInstanceOf[AveragePool]
    val kernel_shape = p.kernel_shape

    val counts = kernel_shape.reduce(_+_)

    val steps = p.strides

    MapSeq( MapSeq ( dividedBy(counts) )) o
        Join() o MapSeq( MapSeq( Join() o MapSeq(
        fun( y =>
          ReduceSeq(add, 0.0f) o
            Join() o Join() $ y )
      ) ) ) o Slide3D(kernel_shape(0),steps(0),kernel_shape(1),steps(1),kernel_shape(2),steps(2)) $ in
  }

}
