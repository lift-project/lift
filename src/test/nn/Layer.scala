package nn

import ir.ast.FunDecl

/**
  * Created by s1569687 on 8/5/17.
  */
trait Layer {
  val localSize: Array[Int]
  val globalSize: Array[Int]

  var runtime: Double

  val liftFProp: FunDecl

  def groupAndUnpad(outputsFlat: Array[Float], datasets: NetDatasets): Unit
}
