package backends.spatial.accel.ir.pattern

import ir.ast.{AbstractMap, Join, Lambda, Lambda1, Pattern, Split, fun}
import lift.arithmetic.{ArithExpr, PosVar}

case class MapPar(override val f: Lambda1)
  extends AbstractMap(f, "MapPar", PosVar("p_id")) {
  override def copy(f: Lambda): Pattern = MapPar(f)
  var shouldUnroll = false
}

object MapPar {
  def apply(f: Lambda1) = new MapPar(f)
}

object MapParWithFactor {
  def apply(factor: ArithExpr, f: Lambda): Lambda = fun(arr =>
    Join() o MapSeq(MapPar(f)) o Split(factor) $ arr)
}
