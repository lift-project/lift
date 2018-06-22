package nn.caffe.proto

import _root_.caffe.caffe.{LayerParameter, NetParameter, V1LayerParameter}
import nn.caffe.proto.Config.{Version, getType}

import scala.io.Source
import scalapb.TextFormatError

case class Config(netParam: NetParameter) {
  val version: Config.Version.Value = if (netParam.layers.nonEmpty) Version.V1 else Version.NEW
  var dataLayerV1: Option[V1LayerParameter] = None
  var dataLayerVNew: Option[LayerParameter] = None

  val layersWithSizesV1: Option[Seq[(V1LayerParameter, Int)]] = {
    version match {
      case Version.V1 => Some(fillMissingParametersV1())
      case Version.NEW => None
    }
  }
  
  val layersWithSizesVNew: Option[Seq[(LayerParameter, Int)]] = {
    version match {
      case Version.V1 => None
      case Version.NEW => Some(fillMissingParametersVNew())
    }
  }

  def fillMissingParametersV1(): Seq[(V1LayerParameter, Int)] = {
    dataLayerV1 = netParam.layers.find(layer => layer.`type`.get == V1LayerParameter.LayerType.DATA &&
      (layer.include.isEmpty || layer.include.exists(_.phase match {
        case Some(_root_.caffe.caffe.Phase.TEST) => true
        case None => true
        case _ => false
      }))
    ) match {
      case Some(l) => Some(l)
      case None => throw new java.util.NoSuchElementException("Cannot find a data layer in Caffe protofile")
    }

    val processedLayers = scala.collection.mutable.Map[V1LayerParameter, Int]()

    processedLayers += ((dataLayerV1.get, dataLayerV1.get.transformParam.get.cropSize.get))
    for (layer <- netParam.layers) {
      if (!processedLayers.contains(layer)) {

        def computeImageSize(currentLayer: V1LayerParameter): Int = {
          if (processedLayers.contains(layer))
            processedLayers(layer)
          else {
            val parent = netParam.layers.find(layer => currentLayer.bottom.contains(layer.name.get)).get

            val currentLayerSize: Int = getType(parent) match {
              case V1LayerParameter.LayerType.RELU => computeImageSize(parent)
              case V1LayerParameter.LayerType.LRN => computeImageSize(parent)
              case V1LayerParameter.LayerType.DATA => parent.transformParam.get.cropSize.get

              case V1LayerParameter.LayerType.CONVOLUTION =>
                def getStride(stride: Seq[Int]): Int = if (stride.nonEmpty) stride.head else 1

                (computeImageSize(parent) -
                  (parent.convolutionParam.get.kernelSize.head - getStride(parent.convolutionParam.get.stride)) +
                  parent.convolutionParam.get.pad.head * 2) / getStride(parent.convolutionParam.get.stride)

              case V1LayerParameter.LayerType.POOLING =>
                (computeImageSize(parent) -
                  (parent.poolingParam.get.kernelSize.head - parent.poolingParam.get.stride.get) +
                  parent.poolingParam.get.pad.getOrElse(0) * 2) / parent.poolingParam.get.stride.get

              case V1LayerParameter.LayerType.INNER_PRODUCT => parent.innerProductParam.get.numOutput.get
              case V1LayerParameter.LayerType.CONCAT => computeImageSize(parent)
              case V1LayerParameter.LayerType.DROPOUT => computeImageSize(parent)
              case V1LayerParameter.LayerType.SOFTMAX => computeImageSize(parent)
              case V1LayerParameter.LayerType.SOFTMAX_LOSS => computeImageSize(parent)
              case V1LayerParameter.LayerType.ACCURACY => parent.accuracyParam.get.topK.get
              case V1LayerParameter.LayerType.ELTWISE => computeImageSize(parent)
              case _ =>
                parent.`type`.get.name match {
                  case "BatchNorm" => computeImageSize(parent)
                  case "Scale" => computeImageSize(parent)

                  case _ =>
                    throw new java.lang.IllegalArgumentException("Cannot recognize a layer from the proto file (" +
                      parent.`type`.get.name + ")")
                }
            }
            processedLayers += ((layer, currentLayerSize))

            currentLayerSize
          }
        }

        computeImageSize(layer)
      }
    }
    netParam.layers.map(layer => (layer, processedLayers(layer)))
  }

  def fillMissingParametersVNew(): Seq[(LayerParameter, Int)] = {
    dataLayerVNew = netParam.layer.find(layer => getType(layer) == V1LayerParameter.LayerType.DATA &&
      (layer.include.isEmpty || layer.include.exists(_.phase match {
        case Some(_root_.caffe.caffe.Phase.TEST) => true
        case None => true
        case _ => false
      }))
    ) match {
      case Some(l) => Some(l)
      case None => throw new java.util.NoSuchElementException("Cannot find a data layer in Caffe protofile")
    }

    val processedLayers = scala.collection.mutable.Map[LayerParameter, Int]()

    processedLayers += ((dataLayerVNew.get, dataLayerVNew.get.transformParam.get.cropSize.get))
    for (layer <- netParam.layer) {
      if (!processedLayers.contains(layer)) {

        def computeImageSize(currentLayer: LayerParameter): Int = {
          if (processedLayers.contains(layer))
            processedLayers(layer)
          else {
            val parent = netParam.layer.find(layer => currentLayer.bottom.contains(layer.name.get)).get

            val currentLayerSize: Int = getType(parent) match {
              case V1LayerParameter.LayerType.RELU => computeImageSize(parent)
              case V1LayerParameter.LayerType.LRN => computeImageSize(parent)
              case V1LayerParameter.LayerType.DATA => parent.transformParam.get.cropSize.get

              case V1LayerParameter.LayerType.CONVOLUTION =>
                def getStride(stride: Seq[Int]): Int = if (stride.nonEmpty) stride.head else 1

                (computeImageSize(parent) -
                  (parent.convolutionParam.get.kernelSize.head - getStride(parent.convolutionParam.get.stride)) +
                  parent.convolutionParam.get.pad.head * 2) / getStride(parent.convolutionParam.get.stride)

              case V1LayerParameter.LayerType.POOLING =>
                (computeImageSize(parent) -
                  (parent.poolingParam.get.kernelSize.head - parent.poolingParam.get.stride.get) +
                  parent.poolingParam.get.pad.getOrElse(0) * 2) / parent.poolingParam.get.stride.get

              case V1LayerParameter.LayerType.INNER_PRODUCT => parent.innerProductParam.get.numOutput.get
              case V1LayerParameter.LayerType.CONCAT => computeImageSize(parent)
              case V1LayerParameter.LayerType.DROPOUT => computeImageSize(parent)
              case V1LayerParameter.LayerType.SOFTMAX => computeImageSize(parent)
              case V1LayerParameter.LayerType.SOFTMAX_LOSS => computeImageSize(parent)
              case V1LayerParameter.LayerType.ACCURACY => parent.accuracyParam.get.topK.get
              case V1LayerParameter.LayerType.ELTWISE => computeImageSize(parent)
              case _ =>
                parent.`type`.get match {
                  case "BatchNorm" => computeImageSize(parent)
                  case "Scale" => computeImageSize(parent)

                  case _ =>
                    throw new java.lang.IllegalArgumentException("Cannot recognize a layer from the proto file (" +
                      parent.`type`.get + ")")
                }
            }
            processedLayers += ((layer, currentLayerSize))

            currentLayerSize
          }
        }

        computeImageSize(layer)
      }
    }
    netParam.layer.map(layer => (layer, processedLayers(layer)))
  }
  
}

/**
  * Created by nm on 11/06/18.
  */
object Config {
  def load(fileName: String): NetParameter = {
    val result: Either[TextFormatError, NetParameter] = 
      _root_.scalapb.TextFormat.fromAscii(NetParameter, Source.fromFile(fileName).getLines.mkString)
    result match {
      case Left(err) =>
        throw new java.lang.IllegalStateException(
          "Encountered text format error while parsing " + fileName + "\n" + err.msg)
      case Right(netParam) => netParam
    }    
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
//    val result = load("/home/nm/avus_remotes/lift/src/test/nn/caffe/proto/GoogleNet.prototxt")
    val result = load("/home/nm/avus_remotes/lift/src/test/nn/caffe/proto/ResNet-101-deploy-5.prototxt")
//    val result: NetParameter = load("/home/nm/avus_remotes/lift/src/test/nn/caffe/proto/VGG_ILSVRC_19_layers_deploy_1.prototxt")
    //result
  }
  
  // V1LayerParameter has a case class for each layer type. Newer LayerParameter stores layer type as string.
  // This type class matches the string to the respective case class of V1LayerParameter
  trait LayerParamType[T] {
    def getType(layerparam: T): V1LayerParameter.LayerType  
  }
  
  def getType[T](layerparam: T)(implicit lp: LayerParamType[T]) = lp.getType(layerparam)
  
  implicit val v1GetType: LayerParamType[V1LayerParameter] = new LayerParamType[V1LayerParameter] {
    def getType(layerparam: V1LayerParameter): V1LayerParameter.LayerType = layerparam.`type`.get
  }
  
  implicit val vGetType: LayerParamType[LayerParameter] = new LayerParamType[LayerParameter] {
    def getType(layerparam: LayerParameter): V1LayerParameter.LayerType = 
      V1LayerParameter.LayerType.values.find(_.name.toLowerCase() == layerparam.`type`.get.toLowerCase()).get
  }

  object Version extends Enumeration {
    type Version = Value
    val V1, NEW = Value
  }

  def apply(fileName: String): Config = new Config(load(fileName))
}
