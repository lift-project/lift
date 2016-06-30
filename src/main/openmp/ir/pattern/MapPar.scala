package openmp.ir.pattern

import apart.arithmetic.PosVar
import ir.ast.{AbstractMap, Lambda, Lambda1, Pattern}
import opencl.ir.pattern.MapSeq

/**
  * Created by Federico on 29-Jun-16.
  */
case class MapPar(override val f: Lambda1) extends AbstractMap(f, "MapPar",
  PosVar("i")) {
  override def copy(f: Lambda): Pattern = MapSeq(f)
  var shouldUnroll = false
}