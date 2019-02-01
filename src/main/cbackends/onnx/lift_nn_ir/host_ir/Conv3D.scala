package cbackends.onnx.lift_nn_ir.host_ir

import ir.{ArrayTypeWSWC, ast}
import ir.ast.{Join, Lambda, Slide3D, fun}
import lift.arithmetic.SizeVar
import opencl.ir.pattern
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import opencl.ir.{Float, add, _}

object Conv3D {

  def apply() : Lambda = {

    val N = SizeVar("N")


    val f = ast.fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      in => MapSeq(  MapSeq( Join() o MapSeq(
        pattern.ReduceSeq(add, 0.0f)  o Join() o MapSeq( ReduceSeq(add, 0.0f) )  o MapSeq( Join() o MapSeq( ReduceSeq( add, 0.0f ) ))
      ) ) ) o Slide3D(3,1) $ in
    )

    f

  }

}
