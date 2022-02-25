package benchmarks.conv.conv_lambdas

import benchmarks.conv.ConvExplorationJSONSettings
import benchmarks.conv.tuning.DirectConvTuneParamSpaceFactory
import benchmarks.conv.util.BenchmarkStages
import ir.ast.debug.AssertType
import ir.ast.{Get, Join, Lambda, Let, Map, Slide2D, SplitND, Transpose, TransposeW, Value, Zip, ZipNDWithCompactViews, fun}
import ir.{ArrayType, GenerateIR, TupleType}
import lift.arithmetic.ArithExpr
import opencl.ir.pattern.{MapSeq, ReduceSeq}
import opencl.ir.{Float, add, mult}
import benchmarks.conv.ConvExplorationJSONSettings
import benchmarks.conv.tuning.DirectConvTuneParamSpaceFactory
import benchmarks.conv.util.BenchmarkStages
import benchmarks.conv.tuning.DirectConvTuneParamSpaceFactory
import benchmarks.conv.util.{BenchmarkStages, DirectConvBenchmarkStages}
import benchmarks.conv.{ConvExplorationJSONSettings, HighLevelRewriteParamValues}

object DirectConvLambdaFactory extends ConvImplementationFactory {

  type T = DirectConvTuneParamSpaceFactory
  def b: BenchmarkStages[T] = DirectConvBenchmarkStages()

  def apply()(implicit jsonSettings: ConvExplorationJSONSettings): ConvImplementation[T] = DirectConvLambda()
}

case class DirectConvLambda()(
  implicit val jsonSettings: ConvExplorationJSONSettings)
  extends ConvImplementation[DirectConvTuneParamSpaceFactory] {
  def Arr: ArrayType.type = ArrayType

  /* Helper symbols */
  val t: DirectConvLambdaFactory.T = DirectConvTuneParamSpaceFactory()

  private val divideAndConquerLevels: Int = 2

  /*********** High-level convolution with TVM-style data layout
   * NB: Although the lambda below uses MapSeqs and not Maps, the parallelization will be decided later
   * through rewriting. MapSeqs are used because compiler passes such as mem inference make some assumptions about
   * Maps that I don't want to deal with currently.
   *
   ***********/
  def apply(highLevelParamVals: HighLevelRewriteParamValues): Lambda = {

    fun(
      Arr(Arr(Arr(Float,  t.l.inputChannels), t.inputWidthPadded), t.inputHeightPadded),

      Arr(Arr(Arr(Arr(Float, t.l.inputChannels), t.l.kernelWidth), t.l.kernelHeight), t.l.numKernels),

      (inputData, kernelWeights) =>

        AssertType(
          Arr(Arr(Arr(Float, t.l.numKernels), t.nWindowsAcrossInputInXOptPadded), t.nWindowsAcrossInputInYOptPadded),  "After reduction") o
          TransposeW(1, 2, 0)
          o Map(Map(Join())) o


          MapSeq(fun(rowOfSlidingWindows =>
            MapSeq(fun(singleSlidingWindow =>
              MapSeq(fun(singleKernel =>

                ReduceSeq(add, init = Value("0.0f", Float)) o MapSeq(mult) $
                  Zip(
                    Join() o Join() $ singleSlidingWindow,
                    Join() o Join() $ singleKernel)

              )) $ kernelWeights

            )) $ rowOfSlidingWindows
          )) o
          // Slide input
          AssertType(expectedTypeAsORList = List(
            Arr(Arr(Arr(Arr(Arr(Float, t.l.inputChannels), t.l.kernelWidth), t.l.kernelHeight),
              t.nWindowsAcrossInputInXOptPadded), t.nWindowsAcrossInputInYOptPadded)),
            "Slided input") o
          Slide2D(t.l.kernelHeight, t.l.kernelStrideY, t.l.kernelWidth, t.l.kernelStrideX) $ inputData

    )
  }
}
