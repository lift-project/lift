package opencl.ir.pattern

import ir.ast._
import lift.arithmetic.PosVar

case class MapGlb(dim: Int, override val f: Lambda1)
  extends AbstractMap(f, "MapGlbl", PosVar("gl_id")) {
  override def copy(f: Lambda): Pattern = MapGlb(dim, f)

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)
  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = MapGlb(dim, f.visitAndRebuild(pre,post).asInstanceOf[Lambda])
}

object MapGlb {
  def apply(f: Lambda1) = new MapGlb(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapGlb(dim, f)
}
