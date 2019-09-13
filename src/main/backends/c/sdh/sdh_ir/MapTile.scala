package backends.c.sdh.sdh_ir

import ir.ast.{AbstractMap, IRNode, Lambda, Pattern}
import lift.arithmetic.{PosVar, Var}

case class MapTile(override val f: Lambda,
                   override  val num_hw_elements: Int = 2) extends AbstractSDHMap(f, "MapTile",  num_hw_elements)
  //extends AbstractMap(f, "MapHost", PosVar("i"))
{

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = {
    f.visit_pp(prePost)
  }

  override def copy(f: Lambda): Pattern = MapTile(f)
}

