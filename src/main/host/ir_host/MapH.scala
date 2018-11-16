package host.ir_host

import ir.ast.{AbstractMap, IRNode, Lambda, Pattern}
import lift.arithmetic.{PosVar}

case class MapH(override val f: Lambda) extends AbstractMap(f, "MapH", PosVar("i"))
{

  //override def _visit(prePost: IRNode => IRNode => Unit): Unit = {
    //f.visit(prePost)
  //}

  override def copy(f: Lambda): Pattern = MapH(f)
}
