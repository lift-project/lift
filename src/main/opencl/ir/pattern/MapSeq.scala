package opencl.ir.pattern

import apart.arithmetic.Var
import ir.ast._

case class MapSeq(override val f: Lambda1) extends AbstractMap(f, "MapSeq",
                                                               Var("i"))
                                                   with isGenerable {
  override def copy(f: Lambda): Pattern = MapSeq(f)
}
