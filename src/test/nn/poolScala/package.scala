package nn

/**
  * Created by nm on 08/02/17.
  */

package object poolScala {
  class PoolDatasets(in: PaddedArray[Array5D[Float]] = PaddedArray(Array.empty),
                     out: PaddedArray[Array5D[Float]] = PaddedArray(Array.empty),
                     targ: Array5D[Float] = Array.empty,
                     w: Array4D[Float] = Array.empty,
                     b: Array[Float] = Array.empty) extends NetDatasets {
    var inputs: PaddedArray[Array5D[Float]] = in
    var outputs: PaddedArray[Array5D[Float]] = out
    val targets: Array5D[Float] = targ
    val weights: Array4D[Float] = w
    val biases: Array[Float] = b
  }
}
