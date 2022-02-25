package opencl.ir.pattern

import ir.Type
import lift.arithmetic.PosVar
import ir.ast._

case class MapSeq(override val f: Lambda1) extends AbstractMap(f, "MapSeq",
  PosVar("i")) {
  override def copy(f: Lambda): Pattern = MapSeq(f)
  var shouldUnroll = false

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)
  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = MapSeq(f.visitAndRebuild(pre,post).asInstanceOf[Lambda])
}

class MapSeq2D_(override val f: Lambda1) extends AbstractMap(f, "MapSeq2D_",
  PosVar("i")) {
  override def copy(f: Lambda): Pattern = MapSeq2D_(f)
  var shouldUnroll = false

  var expanded_lambda : Lambda = null

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    expanded_lambda = MapSeq(MapSeq(f))
    expanded_lambda.checkType(argType, setType)
  }

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)
  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = MapSeq2D_(f.visitAndRebuild(pre,post).asInstanceOf[Lambda])

  override def toString: String = s"MapSeq2D_($f)"
}

object MapSeq2D_{
  def apply(f: Lambda1): MapSeq2D_ = new MapSeq2D_(f)

  def unapply(arg: Pattern): Option[Lambda1] = arg match {
    case m: MapSeq2D_ => Some(m.f)
    case MapSeq(Lambda1(outerMapParam, FunCall(MapSeq(f), innerMapArg))) if innerMapArg == outerMapParam.head => Some(f)
    case NotDataReusageButTiling(f, _) => Some(f)
    case _ => None
  }
}


class MapSeqUnroll(override val f: Lambda1) extends MapSeq(f) {
  override def copy(f: Lambda): Pattern = MapSeqUnroll(f)
  shouldUnroll = true
}

object MapSeqUnroll {
  def apply(f: Lambda1) = new MapSeqUnroll(f)
}

