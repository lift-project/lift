package openmp.ir.pattern

import apart.arithmetic.PosVar
import ir.ast.{AbstractMap, Lambda, Lambda1, Pattern}
import opencl.ir.pattern.MapSeq

/**
  * Created by Federico on 29-Jun-16.
  */

abstract class OmpMap(override val f: Lambda1) extends MapSeq(f)

class MapPar(override val f: Lambda1) extends OmpMap(f) {
}
object MapPar {
  def apply(f:Lambda1) = new MapPar(f)
}

class MapVec(override val f: Lambda1) extends OmpMap(f)
object MapVec{
  def apply(f:Lambda1) = new MapVec(f)
}