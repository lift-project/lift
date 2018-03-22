package tutorial.applications

import ir.ArrayTypeWSWC
import ir.ast.{Get, Transpose, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import lift.arithmetic._
import ir._
import ir.ast._
import opencl.executor.Utils
import opencl.ir._
import opencl.ir.pattern._

/**
  * Created by reese on 22/03/18.
  */
class MatrixMatrixMultiplucation {



  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")

  val naive = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
    (A, B) => {
      MapGlb(1)(fun( Arow =>
        MapGlb(0)(fun( Bcol =>
          toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
        )) o Transpose() $ B
      )) $ A
    })

}
