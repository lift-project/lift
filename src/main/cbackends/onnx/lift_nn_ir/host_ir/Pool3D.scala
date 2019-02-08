package cbackends.onnx.lift_nn_ir.host_ir

import ir.ast.{Expr, Join, Param, Slide3D, fun}
import lift.arithmetic.Cst
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import opencl.ir.{Float, add, dividedBy, _}

object Pool3D {

  def apply(in: Expr) : Expr = {

    val counts = 6 * 6 * 8

    MapSeq( MapSeq ( dividedBy(counts) )) o
        Join() o MapSeq( MapSeq( Join() o MapSeq(
        fun( y =>
          ReduceSeq(add, 0.0f) o
            Join() o Join() $ y )
      ) ) ) o Slide3D(6,1,6,1,8,1) $ in
  }

}
