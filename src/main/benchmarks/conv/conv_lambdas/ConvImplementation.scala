package benchmarks.conv.conv_lambdas

import benchmarks.conv.tuning.ConvTuneParamSpaceFactory
import benchmarks.conv.util.BenchmarkStages
import ir.ast.Lambda
import benchmarks.conv.tuning.ConvTuneParamSpaceFactory
import benchmarks.conv.util.BenchmarkStages
import benchmarks.conv.{ConvExplorationJSONSettings, HighLevelRewriteParamValues}
import benchmarks.conv.tuning.ConvTuneParamSpaceFactory
import benchmarks.conv.util.BenchmarkStages

trait ConvImplementationFactory {
  type T <: ConvTuneParamSpaceFactory
  def b: BenchmarkStages[T]

  def apply()(implicit jsonSettings: ConvExplorationJSONSettings): ConvImplementation[T]
}

trait ConvImplementation[
  T <: ConvTuneParamSpaceFactory] {
  val t: T

  def apply(highLevelParamVals: HighLevelRewriteParamValues): Lambda
}
