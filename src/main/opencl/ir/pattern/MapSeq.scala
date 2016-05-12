package opencl.ir.pattern

import apart.arithmetic.{PosVar, Var}
import ir.ast._

case class MapSeq(override val f: Lambda1) extends AbstractMap(f, "MapSeq",
  PosVar("i")) {
  override def copy(f: Lambda): Pattern = MapSeq(f)
}
