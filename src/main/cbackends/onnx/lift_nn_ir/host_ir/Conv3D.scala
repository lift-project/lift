package cbackends.onnx.lift_nn_ir.host_ir

import ir.{ArrayTypeWSWC, ast}
import ir.ast.{Expr, FunCall, FunDecl, Get, Join, Lambda, Param, Slide3D, SlideND, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.ir.pattern
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import opencl.ir.{Float, add, _}

object Conv3D {

  def apply(input:Expr, weights:Expr) : Expr = {
    //def apply() : FunDecl = {

    //val N = SizeVar("N")


    /*
    val f = ast.fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      in => MapSeq(  MapSeq( Join() o MapSeq(
        pattern.ReduceSeq(add, 0.0f)  o Join() o MapSeq( ReduceSeq(add, 0.0f) )  o MapSeq( Join() o MapSeq( ReduceSeq( add, 0.0f ) ))
      ) ) ) o Slide3D(3,1) $ in
    )

    f
    */

    /*
    MapSeq(  MapSeq( Join() o MapSeq(
      pattern.ReduceSeq(add, 0.0f)  o Join() o MapSeq( ReduceSeq(add, 0.0f) )  o MapSeq( Join() o MapSeq( ReduceSeq( add, 0.0f ) ))
    ) ) ) o Slide3D(3,1)
    */

     /* val f = ast.fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 8), 8),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 6), 6), 6),
      (input, weights) =>*/

    val ArrayTypeWSWC( ArrayTypeWSWC( ArrayTypeWSWC(_, feature_x_size, _), feature_y_size, _), feature_z_size, _) = weights.t

    //val feature_y_size = 6
    //val feature_z_size = 6
    val step = 1

    MapSeq(  MapSeq( Join() o MapSeq(

        fun( cube =>

          ReduceSeq(add, 0.0f) o
            MapSeq( fun(y => mult.apply(Get(y,0), Get(y,1))) )
            $ Zip( Join() o Join() $ cube, Join() o Join() $ weights)

        )

      ) ) ) o Slide3D(feature_x_size, step, feature_y_size, step, feature_z_size, step) $ input
    //) ) ) o Slide3D(6,step) $ input
    /*
    )

    f */

  }

}
