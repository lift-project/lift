import ir.ast._
import ir.ast.debug.AssertType
import ir.{ArrayType, ArrayTypeWSWC, TupleType, TypeChecker}
import nn.conv.versions.Conv3
import nn.conv.versions.Conv3.multAndSumUp
import opencl.executor.Compile
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal, toPrivate}
import opencl.ir.{Float, id}

/**
  * Created by nm on 11/04/18.
  */

object dontweAll {
  def AT = ArrayType
  type AT = ArrayType


  val nSeqTilesInWindow = 256 * 3 * 3

  def Thing(): FunDecl =
    λ(TupleType(AT(AT(Float, 1), nSeqTilesInWindow), AT(Float, 2304)),
      TupleType(/* weights */AT(AT(Float, 1), nSeqTilesInWindow), /* bias */Float),
      (windowAndAcc, params) =>
        MapSeq(λ(TupleType(AT(Float, 2304), AT(Float, 2304), Float),
          (tileAndWeightsAndAcc) => {
            toGlobal(MapSeq(id)) o
              ReduceSeq(λ((acc, y) =>

                /********* Reducing window tile BEGIN *********/

                multAndSumUp.apply(acc, /* X */ Get(y, 0), /* weights */ Get(y, 1))),
                toPrivate(id) $ /* accumulator */ Get(tileAndWeightsAndAcc, 2)) $
              Zip(Get(tileAndWeightsAndAcc, 0), Get(tileAndWeightsAndAcc, 1))

            /********* Reducing window tile END *********/
          })) $ Zip(Get(windowAndAcc, 0), Get(params, 0), Get(windowAndAcc, 1)))

  def main(args: Array[String]): Unit = {
    Compile(Thing)
  }

}