package rewriting.macrorules.nn

import ir.ArrayType
import ir.ast.{FunDecl, Map, UserFun, λ}
import lift.arithmetic.SizeVar
import opencl.ir.{Float, id}

/**
  * Expression for the Linear Rectifying Unit activation function, which is often expressed as a separate layer
  */
object ReLU {

  val ReLUFun: UserFun = UserFun("ReLU", "x", "{ return(max(0.0f, x)); }", Float, Float)

  val nChannels = SizeVar("nChannels")
  val spatialSize = SizeVar("spatialSize")
  val nInputs = SizeVar("nInputs")
  val nBatches = SizeVar("nBatches")

  def expr: FunDecl = {
    λ(ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float,
      nChannels), spatialSize), spatialSize), nInputs), nBatches),
      (X) => {
        Map(λ(batch =>
          Map(λ(singleInput =>
            Map(λ(firstSpatialDimElement =>

              Map(λ(secondSpatialDimElement =>

                Map(λ(elementChannel =>
                  ReLUFun(elementChannel)
                )) $ secondSpatialDimElement

              )) $ firstSpatialDimElement

            )) $ singleInput
          )) $ batch
        )) $ X
      })
  }
}
