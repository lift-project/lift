package nn.cnn

import nn.conv.Conv
import nn.fc.FC
import nn.{Layer, Shape, cnn}

/**
  * Created by s1569687 on 8/6/17.
  */
class CNN(val nConvLayers: Int,
          val nFCLayers: Int,
          val inputShape: Shape,
          val pathToResults: String) {

  var nLayers: Int = nConvLayers + nFCLayers
  var nPoolLayers: Int = 0
  val layers: Array[Layer] = new Array[Layer](nLayers + 1)
  // convLayers and fcLayers are pointers to the elements of the "layers" array to avoid casting
  // Layer to Conv and FC each time a layer-specific field needs to be accessed
  val convLayers: Array[Conv] = new Array[Conv](nConvLayers)
  val fcLayers: Array[FC] = new Array[FC](nFCLayers)

  def configToString: String = cnn.configToString(inputShape.nBatches, inputShape.nInputs, inputShape.size, nLayers) + {
    var i: Int = -1
    for (convLayer <- convLayers) yield {
      i = i + 1
      f"\nLayer $i%d (Conv):\n" + convLayer.configToString
    }}.mkString("") + {
    var i: Int = convLayers.length - 1
    for (fcLayer <- fcLayers) yield {
      i = i + 1
      f"\nLayer $i%d (FC):\n" + fcLayer.configToString
    }}.mkString("")
}
