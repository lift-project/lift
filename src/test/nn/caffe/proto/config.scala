package nn.caffe.proto

import _root_.caffe.caffe.{NetParameter, V1LayerParameter, LayerParameter}

import scala.io.Source
import scalapb.TextFormatError

/**
  * Created by nm on 11/06/18.
  */
object config {
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
}
