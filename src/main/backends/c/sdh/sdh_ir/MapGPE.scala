package backends.c.sdh.sdh_ir


import ir.ast.{AbstractMap, IRNode, Lambda, Pattern}
import lift.arithmetic.PosVar

case class MapGPE(override val f: Lambda, override val num_hw_elements: Int = 4) extends AbstractSDHMap(f, "MapTile",  num_hw_elements)
  //AbstractMap(f, "MapGPE", PosVar("i"))
{

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)

  override def copy(f: Lambda): Pattern = MapGPE(f, num_hw_elements)
}


