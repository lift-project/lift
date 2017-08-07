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

  val nLayers: Int = nConvLayers + nFCLayers
  val layers: Array[Layer] = new Array[Layer](nLayers)
  // convLayers and fcLayers are pointers to the elements of the "layers" array to avoid casting
  // Layer to Conv and FC each time a layer-specific field needs to be accessed
  val convLayers: Array[Conv] = new Array[Conv](nConvLayers)
  val fcLayers: Array[FC] = new Array[FC](nFCLayers)

  def configToString: String = cnn.configToString(inputShape.nBatches, inputShape.nInputs, nLayers) + {
    for (convLayer <- convLayers) convLayer.configToString
    for (fcLayer <- fcLayers) fcLayer.configToString
  }
}
