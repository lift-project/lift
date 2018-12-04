package nn.caffe.proto

import _root_.caffe.caffe.{LayerParameter, NetParameter, V1LayerParameter}
import com.typesafe.scalalogging.Logger
import nn.caffe.proto.Config.{Version, getType}
import nn.cnn.ExperimentParams
import nn.{cnn, conv, fc}

import scala.io.Source
import scala.util.matching.UnanchoredRegex
import scalapb.TextFormatError

case class Config(netParam: NetParameter) {
  val version: Config.Version.Value = if (netParam.layers.nonEmpty) Version.V1 else Version.NEW
  var dataLayerV1: Option[V1LayerParameter] = None
  var dataLayerVNew: Option[LayerParameter] = None

  val layersWithSizesV1: Option[Vector[(V1LayerParameter, (Int, Int))]] = {
    version match {
      case Version.V1 => Some(fillMissingParametersV1())
      case Version.NEW => None
    }
  }
  
  val layersWithSizesVNew: Option[Vector[(LayerParameter, (Int, Int))]] = {
    version match {
      case Version.V1 => None
      case Version.NEW => Some(fillMissingParametersVNew())
    }
  }

  def fillMissingParametersV1(): Vector[(V1LayerParameter, (Int, Int))] = {
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

    val processedLayers = scala.collection.mutable.Map[V1LayerParameter, (Int, Int)]()

    // TODO: get input channels from Caffe
    processedLayers += ((dataLayerV1.get, (dataLayerV1.get.transformParam.get.cropSize.get, 3)))
    for (layer <- netParam.layers) {
      if (!processedLayers.contains(layer)) {

        def computeInputDimensions(currentLayer: V1LayerParameter): (Int, Int) = {
          if (processedLayers.contains(currentLayer))
            processedLayers(currentLayer)
          else {
            val parent = netParam.layers.find(l => currentLayer.bottom.contains(l.name.get)).get

            val currentLayerDimensions: (Int, Int) = getType(parent) match {
              case "ReLU" => computeInputDimensions(parent)
              case "LRN" => computeInputDimensions(parent)
              case "Data" => 
                (parent.transformParam.get.cropSize.get, 3) // TODO: get input channels from Caffe

              case "Convolution" =>
                def getStride(stride: Seq[Int]): Int = if (stride.nonEmpty) stride.head else 1

                ((computeInputDimensions(parent)._1 -
                  (parent.convolutionParam.get.kernelSize.head - getStride(parent.convolutionParam.get.stride)) +
                  parent.convolutionParam.get.pad.head * 2) / getStride(parent.convolutionParam.get.stride),
                parent.convolutionParam.get.numOutput.get)

              case "Pooling" =>
                val parentDimensions: (Int, Int) = computeInputDimensions(parent)
                ((parentDimensions._1 -
                  (parent.poolingParam.get.kernelSize.head - parent.poolingParam.get.stride.get) +
                  parent.poolingParam.get.pad.getOrElse(0) * 2) / parent.poolingParam.get.stride.get,
                  parentDimensions._2)

              case "InnerProduct" => 
                (parent.innerProductParam.get.numOutput.get, 1) // TODO: verify
              case "Concat" => computeInputDimensions(parent)
              case "Dropout" => computeInputDimensions(parent)
              case "Softmax" => computeInputDimensions(parent)
              case "Accuracy" => (parent.accuracyParam.get.topK.get, 1) // TODO: verify
              case "Eltwise" => computeInputDimensions(parent)
              case _ =>
                parent.`type`.get.name match {
                  case "BatchNorm" => computeInputDimensions(parent)
                  case "Scale" => computeInputDimensions(parent)

                  case _ =>
                    throw new java.lang.IllegalArgumentException("Cannot recognize a layer from the proto file (" +
                      parent.`type`.get.name + ")")
                }
            }
            processedLayers += ((currentLayer, currentLayerDimensions))

            currentLayerDimensions
          }
        }

        computeInputDimensions(layer)
      }
    }
    netParam.layers.map(layer => (layer, processedLayers(layer))).toVector
  }

  def fillMissingParametersVNew(): Vector[(LayerParameter, (Int, Int))] = {

    var skippedLayers = Set[LayerParameter]()
    val processedLayers = {
      val processedLayers = scala.collection.mutable.Map[LayerParameter, (Int, Int)]()

      val layers = netParam.layer.filter(layer => getType(layer) == "Data" &&
        (layer.include.isEmpty || layer.include.exists(_.phase match {
          case Some(_root_.caffe.caffe.Phase.TEST) => {
            dataLayerVNew = Some(layer)
            true
          }
          case Some(_root_.caffe.caffe.Phase.TRAIN) => {
            skippedLayers += layer
            true
          }
          case None => true
          case _ => false
        }))
      )
      if (layers.isEmpty)
        throw new java.util.NoSuchElementException("Cannot find a data layer in Caffe protofile")

      for (layer <- layers) {
        processedLayers += ((layer, (layer.transformParam.get.cropSize.get, 1)))
      }

      processedLayers
    }


    for (layer <- netParam.layer) {
      if (!processedLayers.contains(layer) && !skippedLayers.contains(layer)) {

        def computeInputDimensions(currentLayer: LayerParameter): (Int, Int) = {
          if (processedLayers.contains(layer))
            processedLayers(layer)
          else {
            val parent = netParam.layer.find(layer => currentLayer.bottom.contains(layer.name.get) &&
              !skippedLayers.contains(layer)).get

            val currentLayerDimensions: (Int, Int) = getType(parent) match {
              case "ReLU" => computeInputDimensions(parent)
              case "LRN" => computeInputDimensions(parent)
              case "Data" =>
                (parent.transformParam.get.cropSize.get, 3)

              case "Convolution" =>
                val pad = if (parent.convolutionParam.get.pad.nonEmpty) parent.convolutionParam.get.pad.head
                else 1
                val stride = if (parent.convolutionParam.get.stride.nonEmpty) parent.convolutionParam.get.stride.head
                else 1

                ((computeInputDimensions(parent)._1 -
                  (parent.convolutionParam.get.kernelSize.head - stride) + pad * 2) / stride,
                  parent.convolutionParam.get.numOutput.get)

              case "Pooling" =>
                val parentDimensions: (Int, Int) = computeInputDimensions(parent)
                ((parentDimensions._1 -
                  (parent.poolingParam.get.kernelSize.head - parent.poolingParam.get.stride.get) +
                  parent.poolingParam.get.pad.getOrElse(0) * 2) / parent.poolingParam.get.stride.get,
                  parentDimensions._2)

              case "InnerProduct" => 
                (parent.innerProductParam.get.numOutput.get, 1)
              case "Concat" => computeInputDimensions(parent)
              case "Dropout" => computeInputDimensions(parent)
              case "Softmax" => computeInputDimensions(parent)
              case "Accuracy" => (parent.accuracyParam.get.topK.get, 1)
              case "Eltwise" => computeInputDimensions(parent)
              case _ =>
                parent.`type`.get match {
                  case "BatchNorm" => computeInputDimensions(parent)
                  case "Scale" => computeInputDimensions(parent)

                  case _ =>
                    throw new java.lang.IllegalArgumentException("Cannot recognize a layer from the proto file (" +
                      parent.`type`.get + ")")
                }
            }
            processedLayers += ((layer, currentLayerDimensions))

            currentLayerDimensions
          }
        }

        computeInputDimensions(layer)
      }
    }
    netParam.layer.map(layer => (layer, processedLayers(layer)))
  }.toVector
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
    val paramTypePattern: UnanchoredRegex = raw"Some\(([A-z]+?)Parameter\)".r.unanchored
    def getType(layerparam: T): String
  }
  
  def getType[T](layerparam: T)(implicit lp: LayerParamType[T]) = lp.getType(layerparam)
  
  implicit val v1GetType: LayerParamType[V1LayerParameter] = new LayerParamType[V1LayerParameter] {
    def getType(layerparam: V1LayerParameter): String = 
      // e.g. INNER_PRODUCT => InnerProduct 
      layerparam.`type`.get.name.split('_').map(_.toLowerCase.capitalize).mkString("")
  }
  
  implicit val vGetType: LayerParamType[LayerParameter] = new LayerParamType[LayerParameter] {
    def getType(layerparam: LayerParameter): String = layerparam.`type`.get
//    {
//      layerparam.toString match {
//        case paramTypePattern(paramType: String) => paramType
//        case _ => "NOTRECOGNIZED"
//      }
//    }
  }

  object Version extends Enumeration {
    type Version = Value
    val V1, NEW = Value
  }

  def configToExperimentParams(protoFilePath: String): Vector[ExperimentParams] = {

    val logger = Logger(this.getClass)
    logger.info("Processing PROTO config file \"" + protoFilePath + "\"")

    val config: nn.caffe.proto.Config = new Config(load(protoFilePath))
    
    val netName: String = config.netParam.name.get

    {
      config.version match {
        case Config.Version.V1 => config.layersWithSizesV1.get
        case Config.Version.NEW => config.layersWithSizesVNew.get}
    }.zipWithIndex.filter(layerAndNo => {
      layerAndNo._1._1 match {
        case layerV1: V1LayerParameter =>
          nn.caffe.proto.Config.getType(layerV1) == "Convolution"
        case layerVNew: LayerParameter =>
          nn.caffe.proto.Config.getType(layerVNew) == "Convolution"
      }}).map{
      case ((layer, inputDimensions), i) =>
        val nInputs: Int = {
          config.version match {
            case Config.Version.V1 => config.dataLayerV1.get.dataParam
            case Config.Version.NEW => config.dataLayerVNew.get.dataParam
          }}.get.batchSize.get

        val layerName: String = config.version match {
          case Config.Version.V1 => layer.asInstanceOf[V1LayerParameter].name.get
          case Config.Version.NEW => layer.asInstanceOf[LayerParameter].name.get
        }

        val paddedInputSize: Int = inputDimensions._1 + 2 * {
          def getPad(pad: Seq[Int]): Int = if (pad.nonEmpty) pad.head else 0
          config.version match {
            case Config.Version.V1 => getPad(layer.asInstanceOf[V1LayerParameter].convolutionParam.get.pad)
            case Config.Version.NEW => getPad(layer.asInstanceOf[LayerParameter].convolutionParam.get.pad)
          }}

        val nKernels: Int = {
          config.version match {
            case Config.Version.V1 => layer.asInstanceOf[V1LayerParameter].convolutionParam.get.numOutput.get
            case Config.Version.NEW => layer.asInstanceOf[LayerParameter].convolutionParam.get.numOutput.get
          }}

        val kernelSize: Int = {
          config.version match {
            case Config.Version.V1 => layer.asInstanceOf[V1LayerParameter].convolutionParam.get.kernelSize.head
            case Config.Version.NEW => layer.asInstanceOf[LayerParameter].convolutionParam.get.kernelSize.head
          }}

        val kernelStride: Int = {
          def getStride(stride: Seq[Int]): Int = if (stride.nonEmpty) stride.head else 1
          config.version match {
            case Config.Version.V1 => getStride(layer.asInstanceOf[V1LayerParameter].convolutionParam.get.stride)
            case Config.Version.NEW => getStride(layer.asInstanceOf[LayerParameter].convolutionParam.get.stride)
          }}

        new ExperimentParams(
          netName = netName,
          kernelOutputSubfolder = i.toString,
          layerName = layerName,
          layerNo = i,

          // TODO: generalise nBatches and channels
          exactParams = Some(
            ExperimentParams.Exact(
              inputConfig = cnn.InputConfig(1, nInputs, paddedInputSize, inputDimensions._2),
              convDimensions = conv.Experiment.Config.Dimensions(nKernels, kernelSize, kernelStride),
              fcDimensions = fc.Experiment.Config.Dimensions(1))),

          dim = None,

          inputTileSizeRange = Vector(
            (in: cnn.InputConfig, c: conv.Experiment.Config.Dimensions) =>
              (c.kernelSize to in.inputSize by 1).toVector),

          elsPerThreadRange = Vector(
            (in: cnn.InputConfig, c: conv.Experiment.Config.Dimensions) =>
              (1 to (in.nChannels * c.kernelSize * c.kernelSize) by 1).toVector),

          kernelsPerGroupRange = Vector(
            (_: cnn.InputConfig, c: conv.Experiment.Config.Dimensions) =>
              (1 to c.nKernels by 1).toVector),

          coalesceRange = Vector(Vector(true, false)),
          unrollReduceRange = Vector(Vector(true, false)), //Vector(Vector(true, false)),
          vectorLenRange = Vector(Vector(1, 4)), //Vector(Vector(1, 2, 4)),

          multsPerThreadRange = Vector(
            (_: cnn.InputConfig, _: fc.Experiment.Config.Dimensions) =>
              Vector(1)),
          neuronsPerWrgRange = Vector(
            (_: cnn.InputConfig, _: fc.Experiment.Config.Dimensions) =>
              Vector(1))
//          inputTileSizeRange = Vector(
//            (in: cnn.InputConfig, c: conv.Experiment.Config.Dimensions) =>
//              /*(c.kernelSize to in.inputSize by 1).toVector*/Vector(3)),
//
//          elsPerThreadRange = Vector(
//            (in: cnn.InputConfig, c: conv.Experiment.Config.Dimensions) =>
//              /*(1 to (in.nChannels * c.kernelSize * c.kernelSize) by 1).toVector*/Vector(6)),
//
//          kernelsPerGroupRange = Vector(
//            (in: cnn.InputConfig, c: conv.Experiment.Config.Dimensions) =>
//              /*(1 to c.nKernels by 1).toVector*/Vector(1)),
//
//          coalesceRange = Vector(Vector(true)),
//          unrollReduceRange = Vector(Vector(true)),
//          vectorLenRange = Vector(Vector(2)),
//
//
//          multsPerThreadRange = Vector(
//            (_: cnn.InputConfig, _: fc.Experiment.Config.Dimensions) =>
//              Vector(1)),
//          neuronsPerWrgRange = Vector(
//            (_: cnn.InputConfig, _: fc.Experiment.Config.Dimensions) =>
//              Vector(1))
        )
    }
  }
}
