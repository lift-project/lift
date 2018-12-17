package rewriting.macrorules.nn

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.ir._

/**
  * Expressions for the first fully connected layer in a convolutional network, which receives
  * multidimensional input unless the latter is explicitly flattened first
  */
object FC_ND {

  val nChannels = SizeVar("nChannels")
  val spatialSize = SizeVar("spatialSize")
  val nInputs = SizeVar("nInputs")
  val nBatches = SizeVar("nBatches")
  val nNeurons = SizeVar("nNeurons")
  val flattenedInputSize = SizeVar("flattenedInputSize")

  /**
    * First flatten the inputs, and then reduce
    */
  def exprWithReshapedInputs: FunDecl = {
    λ(ArrayType(ArrayType(Float, flattenedInputSize), nNeurons),

      ArrayType(Float, nNeurons),

      ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float,
        nChannels), spatialSize), spatialSize), nInputs), nBatches),

      (W, B, X) => {
        Map(λ(batch =>
          Map(λ(singleInput =>

            Join() o
              Map(λ(neuronWAndB =>

                Reduce(λ((acc, WAndXElement) =>
                  multAndSumUp(acc, Get(0) $ WAndXElement, Get(1) $ WAndXElement)
                ), /* bias */ Get(1) $ neuronWAndB)

                  $ Zip(
                  /* weights */ Get(0) $ neuronWAndB,
                  /* flattened inputs */ Join() o Join() $ singleInput)

              )) $ Zip(W, B)

          )) $ batch
        )) $ X
      })
  }


  /**
    * Split flat weights to add dimensions, then reduce
    */
  def exprWithReshapedWeights: FunDecl = {
    λ(ArrayType(ArrayType(Float, flattenedInputSize), nNeurons),

      ArrayType(Float, nNeurons),

      ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float,
        nChannels), spatialSize), spatialSize), nInputs), nBatches),

      (W, B, X) => {
        Map(λ(batch =>
          Map(λ(singleInput =>

            Join() o
              Map(λ(neuronWAndB =>

                Reduce(add, /* bias */ Get(1) $ neuronWAndB) o
                  Join() o

                  Map(λ(FirstSpatialWAndXDimElement =>

                    Reduce(add, 0.0f) o
                      Join() o

                      Map(λ(SecondSpatialWAndXDimElement =>

                        Reduce(λ((acc, WAndXElementChannel) =>
                          multAndSumUp(acc, Get(0) $ WAndXElementChannel, Get(1) $ WAndXElementChannel)
                        ), 0.0f) $
                          Zip(
                            /* weight channels */ Split(nChannels) o Get(0) $ SecondSpatialWAndXDimElement,
                            /* input channels */ Get(1) $ SecondSpatialWAndXDimElement)

                      )) $ Zip(
                      /* weight rows */ Split(spatialSize) o Get(0) $ FirstSpatialWAndXDimElement,
                      /* input elements */ Get(1) $ FirstSpatialWAndXDimElement)

                  )) $ Zip(
                  /* weight rows */ Split(spatialSize) o Get(0) $ neuronWAndB,
                  /* input rows */ singleInput)
              )) $ Zip(W, B)

          )) $ batch
        )) $ X
      })
  }
}
