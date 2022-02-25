package benchmarks.conv

import ConvExploration.debugRun

case class HighLevelRewriteParamValues(depthWiseTilingPartReduceOnly: Boolean,
                                       newDataLayout: Boolean,
                                       convertDirectToGEMM: Boolean) {
  override def toString: String =
    s"""depthWiseTilingPartReduceOnly -> $depthWiseTilingPartReduceOnly,
       |newDataLayout -> $newDataLayout,
       |convertDirectToGEMM -> $convertDirectToGEMM""".stripMargin
}

object HighLevelRewriteParamValues {
  def getNext: HighLevelRewriteParamValues = {
    new HighLevelRewriteParamValues(
      depthWiseTilingPartReduceOnly = false/*if (debugRun) false
      else math.random < 0.5*/, // TODO: re-add support
      newDataLayout = false,
      convertDirectToGEMM = false)
  }
}
