
package benchmarks.conv.gpgpu20.resources

import lift.arithmetic.Cst

object AlexNetLayerConfigs {
  val configNames: Vector[String] = Vector(


"Conv0"
, "Conv3"
, "Conv6"
, "Conv8"
, "Conv10"
)



val configs: Vector[Vector[Cst]] = {
    Vector(


  
  { 
      val nInputs = Cst(1); val inputWidthHeight = Cst(224); val inputChannels = Cst(3)
      val kernelWidthHeight = Cst(11); val kernelChannels = Cst(64); val kernelStride = Cst(4); val padFunc = Cst(2)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }
  
  
, 

    { 
      val nInputs = Cst(1); val inputWidthHeight = Cst(27); val inputChannels = Cst(64)
      val kernelWidthHeight = Cst(5); val kernelChannels = Cst(192); val kernelStride = Cst(1); val padFunc = Cst(2)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }
  
  
, 

    { 
      val nInputs = Cst(1); val inputWidthHeight = Cst(13); val inputChannels = Cst(192)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(384); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }
  
  
, 

    { 
      val nInputs = Cst(1); val inputWidthHeight = Cst(13); val inputChannels = Cst(384)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(256); val kernelStride = Cst(1); val padFunc = Cst(1)

      Vector(nInputs, inputWidthHeight, inputChannels, kernelWidthHeight, kernelChannels, kernelStride, padFunc)
    }
  
  
, 

    { 
      val nInputs = Cst(1); val inputWidthHeight = Cst(13); val inputChannels = Cst(256)
      val kernelWidthHeight = Cst(3); val kernelChannels = Cst(256); val kernelStride = Cst(1); val padFunc = Cst(1)

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



