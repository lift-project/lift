package benchmarks.conv.layer_configs

import lift.arithmetic.Cst

object VGGLayerConfigs {
  val configNames: Vector[String] = Vector(
    "Conv0", "Conv2", "Conv5", "Conv7", "Conv10", "Conv12", "Conv14",
    "Conv17", "Conv19", "Conv21", "Conv24", "Conv26", "Conv28")

  val configs: Vector[Vector[Cst]] = {
    Vector( { // Conv0 // 0
      val nInputs = Cst(1); val inputWidthHeight = Cst(224); val inputChannels = Cst(3)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(64); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv2 // 1
      val nInputs = Cst(1); val inputWidthHeight = Cst(224); val inputChannels = Cst(64)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(64); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv5 // 2
      val nInputs = Cst(1); val inputWidthHeight = Cst(112); val inputChannels = Cst(64)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(128); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv7 // 3
      val nInputs = Cst(1); val inputWidthHeight = Cst(112); val inputChannels = Cst(128)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(128); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv10 // 4
      val nInputs = Cst(1); val inputWidthHeight = Cst(56); val inputChannels = Cst(128)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(256); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv12 // 5
      val nInputs = Cst(1); val inputWidthHeight = Cst(56); val inputChannels = Cst(256)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(256); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv14 // 6
      val nInputs = Cst(1); val inputWidthHeight = Cst(56); val inputChannels = Cst(256)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(256); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv17 // 7
      val nInputs = Cst(1); val inputWidthHeight = Cst(28); val inputChannels = Cst(256)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(512); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv19 // 8
      val nInputs = Cst(1); val inputWidthHeight = Cst(28); val inputChannels = Cst(512)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(512); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv21 // 9
      val nInputs = Cst(1); val inputWidthHeight = Cst(28); val inputChannels = Cst(512)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(512); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv24 // 10
      val nInputs = Cst(1); val inputWidthHeight = Cst(14); val inputChannels = Cst(512)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(512); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv26 // 11
      val nInputs = Cst(1); val inputWidthHeight = Cst(14); val inputChannels = Cst(512)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(512); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }, { // Conv28 // 12
      val nInputs = Cst(1); val inputWidthHeight = Cst(14); val inputChannels = Cst(512)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(512); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }
    )
  }

  var currentConfigIdx: Int = 0

  val nConfigs: Int = configs.length

  def next() : Vector[Cst] = {
    val result = configs(currentConfigIdx)

    currentConfigIdx += 1

    result
  }
}
