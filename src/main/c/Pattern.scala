package c

import apart.arithmetic.PosVar
import ir.ast.{AbstractMap, Lambda, Lambda1, Pattern}

/**
  * Created by Federico on 16-Jun-16.
  */
object Pattern {
  case class MapSeq(override val f:Lambda1) extends AbstractMap(f, "MapSeq", PosVar("i")) {
    override def copy(f: Lambda): Pattern = MapSeq(f)
  }
}
