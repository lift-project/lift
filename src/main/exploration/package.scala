import exploration.Rules._


package object exploration {
  val mapLoweringRules =
    Seq(
      mapSeq,
      mapGlb,
      mapWrg,
      mapLcl,
      mapWarp,
      mapLane
    )

  val reduceLoweringRule = Seq(reduceSeq)

  val addressSpaceRules =
    Seq(
      privateMemory,
      localMemory,
      globalMemory
    )

  val simplificationRules =
    Seq(
      iterateId,
      iterate1,
      asScalarAsVectorId,
      asVectorAsScalarId,
      transposeTransposeId,
      joinSplitId,
      splitJoinId,
      gatherScatterId,
      scatterGatherId,
      removeEmptyMap
    )

  val reduceRules =
    Seq(
      partialReduceToReduce,
      partialReduceReorder,
      partialReduce,
      partialReduceSplitJoin,
      partialReduceReorder,
      partialReduceToReduce
    )

  val fusionRules =
    Seq(
      mapFusion,
      mapFusionWithZip,
      reduceMapFusion,
      reduceSeqMapSeqFusion
    )

  val fissionRules =
    Seq(
      mapFission,
      mapFissionWithZipInside,
      mapFissionWithZipOutside
    )

  val interchangeRules =
    Seq(
      mapReduceInterchange,
      mapReduceInterchangeWithZip,
      mapReducePartialReduce,
      mapMapTransposeZipInside,
      mapMapTransposeZipOutside,
      mapMapInterchange,
      transposeBothSides
    )

  val idRules =
    Seq(
      addId,
      addIdForCurrentValueInReduce,
      addCopy,
      implementOneLevelOfId,
      implementIdAsDeepCopy,
      dropId
    )

  val transposeRules =
    Seq(
      mapSplitTranspose,
      mapTransposeSplit,
      transposeMapSplit,
      splitTranspose,
      mapTransposeTransposeMapTranspose
    )

  val tupleRules =
    Seq(
      tupleMap,
      tupleFission
    )

  val otherRules = Seq(
    gatherToScatter,
    scatterToGather,
    splitJoin,
    vectorize,
    reorderBothSidesWithStride,
    splitZip
  )

  val allRules =
    otherRules ++ tupleRules ++ idRules ++ interchangeRules ++
      fissionRules ++ fusionRules++ reduceRules ++
      simplificationRules ++ addressSpaceRules ++ mapLoweringRules ++ reduceLoweringRule

}
