package backends.spatial.common.ir.view

import lift.arithmetic.ArithExpr

trait ArrayAddressor {
  def index: Index
  def +(that: ArithExpr): ArrayAddressor
  def +(that: Index): ArrayAddressor
  def *(that: ArithExpr): ArrayAddressor
  def *(that: Index): ArrayAddressor
  def visitAndRebuild(f: ArithExpr => ArithExpr): ArrayAddressor
}

object ArrayAddressor {
  def getNonSlicedAccess(arrayAccessStack: List[ArrayAddressor]): Option[List[Index]] = {
    Some(arrayAccessStack.map {
      case idx: Index => idx
      case _ => return None
    })
  }
}

private[view] case class Index(ae: ArithExpr) extends ArrayAddressor {
  def index: Index = this
  def +(that: ArithExpr): Index = Index(ae + that)
  def +(that: Index): Index = Index(ae + that.ae)
  def -(that: ArithExpr): Index = Index(ae - that)
  def -(that: Index): Index = Index(ae - that.ae)
  def *(that: ArithExpr): Index = Index(ae * that)
  def *(that: Index): Index = Index(ae * that.ae)
  def /(that: ArithExpr): Index = Index(ae / that)
  def /(that: Index): Index = Index(ae / that.ae)
  def %(that: ArithExpr): Index = Index(ae % that)
  def %(that: Index): Index = Index(ae % that.ae)

  override def visitAndRebuild(f: ArithExpr => ArithExpr): Index =
    Index(ae.visitAndRebuild(f))
}

private[view] case class Slice(start: ArithExpr, step: ArithExpr, end: ArithExpr) extends ArrayAddressor {
  def index: Index = Index(start)
  def +(that: ArithExpr): Slice = Slice(start + that, step, end + that)
  def +(that: Index): Slice = Slice(start + that.ae, step, end + that.ae)
  def *(that: ArithExpr): Slice = Slice(start * that, step, start * that + (start - end))
  def *(that: Index): Slice = Slice(start * that.ae, step, start * that.ae + (start - end))

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArrayAddressor =
    Slice(start.visitAndRebuild(f), step.visitAndRebuild(f), end.visitAndRebuild(f))
}

private[view] object Slice {
  def continuous(end: ArithExpr): Slice = new Slice(0, 1, end)
  def continuous(start: ArithExpr, end: ArithExpr): Slice = new Slice(start, 1, end)
}
