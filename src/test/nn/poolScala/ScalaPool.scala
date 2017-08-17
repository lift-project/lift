package nn.poolScala

/**
  * Created by nm on 09/01/17.
  */

import ir.ast._
import nn._


case class ScalaPool(inputShape: Shape = null, outputShape: Shape = null,
                     localSize: Array[Int] = null, globalSize: Array[Int] = null) extends Layer {
  val configToString: String = "Scala Pooling layer"
  var runtime: Double = 0
  val liftFProp: FunDecl = null

  var mlpInputlenL2: Int = 0
  var mlpInputlenL2NonVerified: Int = 0
  var poolSize: Int = 0
  val mlpInputLenLimit: Int = 50000
  
  var nChannels: Int = 0
  
  var inputs: Array5D[Float] = null
  var outputs: Array5D[Float] = null

  def groupAndUnpad(outputsFlat: Array[Float], datasets: NetDatasets): Unit = {}
  
  def run(): Unit = {
    val nPools: Int = mlpInputlenL2 / poolSize
    
    outputs = Array.fill[Array4D[Float]](inputs.length)(
      Array.fill[Array3D[Float]](inputs(0).length)(
        Array.fill[Array2D[Float]](nPools)(
          Array.fill[Array[Float]](nPools)(
            Array.fill[Float](nChannels)(0)))))
    
    for (batchI <- inputs.indices) {
      for (inputI <- inputs(batchI).indices) {
        for (poolX <- 0 until nPools) {
          val startX = poolX * poolSize
          for (poolY <- 0 until nPools) {
            val startY = poolY * poolSize
            for (channelI <- 0 until nChannels) {
              outputs(batchI)(inputI)(poolX)(poolY)(channelI) =
                inputs(batchI)(inputI)(startX)(startY)(channelI)
              for (withinPoolX <- 0 until poolSize) {
                for (withinPoolY <- 0 until poolSize) {
                  if (inputs(batchI)(inputI)(startX + withinPoolX)(startY + withinPoolY)(channelI) >
                    outputs(batchI)(inputI)(poolX)(poolY)(channelI))
                    outputs(batchI)(inputI)(poolX)(poolY)(channelI) =
                      inputs(batchI)(inputI)(startX + withinPoolX)(startY + withinPoolY)(channelI)
                }
              }
            }
          }
        }
      }
    }
  }
}

//object ScalaPool {
//
//  def apply(): ScalaPool = {
// }
//}