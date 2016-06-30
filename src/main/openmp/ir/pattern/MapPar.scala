package openmp.ir.pattern

import apart.arithmetic.PosVar
import ir.ast.{AbstractMap, Lambda, Lambda1, Pattern}
import opencl.ir.pattern.MapSeq

/**
  * Created by Federico on 29-Jun-16.
  */
class MapPar(override val f: Lambda1) extends MapSeq(f) {
}
object MapPar {
  def apply(f:Lambda1) = new MapPar(f)
}