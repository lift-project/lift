package patterns.nn.conv

import cbackends.host.host_ir.CPUFunc
import ir.ArrayTypeWSWC
import ir.ast.{Expr, FunCall, Get, Join, Slide3D_R, Zip, fun}
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import opencl.ir.{add, mult, _}

object ConvCPU3D {

  def apply(fc: FunCall, input:Expr, weights:Expr) : Expr = {
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

    CPUFunc( fun( (input, weights) => MapSeq(  MapSeq( Join() o MapSeq(

        fun( cube =>

          ReduceSeq(add, 0.0f) o
            MapSeq( fun(y => mult.apply(Get(y,0), Get(y,1))) )
            $ Zip( Join() o Join() $ cube, Join() o Join() $ weights)

        )

      ) ) ) o Slide3D_R(feature_z_size, step, feature_y_size, step, feature_x_size, step) $ input ) ).apply(input, weights)
    //) ) ) o Slide3D(6,step) $ input
    /*
    )

    f */

  }

}
