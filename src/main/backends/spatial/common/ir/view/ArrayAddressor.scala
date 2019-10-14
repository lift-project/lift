package backends.spatial.common.ir.view

import backends.spatial.accel.ir.ast.SpatialAccelAST
import backends.spatial.accel.ir.ast.SpatialAccelAST.AddressorT
import core.generator.GenericAST
import core.generator.GenericAST.ArithExpression
import lift.arithmetic.{ArithExpr, RangeAdd}

trait ArrayAddressor {
  def startIdx: Index
  def +(that: ArithExpr): ArrayAddressor
  def +(that: Index): ArrayAddressor
  def *(that: ArithExpr): ArrayAddressor
  def *(that: Index): ArrayAddressor
  def visitAndRebuild(f: ArithExpr => ArithExpr): ArrayAddressor
  def toTargetAST: AddressorT
  def eval(): List[Int]
}

object ArrayAddressor {
  def getNonSlicedAccess(arrayAccessStack: List[ArrayAddressor]): Option[List[Index]] = {
    Some(arrayAccessStack.map {
      case idx: Index => idx
      case _ => return None
    })
  }
}

case class Index(ae: ArithExpr) extends ArrayAddressor {
  def startIdx: Index = this
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

  def toTargetAST: SpatialAccelAST.ArrIndex = SpatialAccelAST.ArrIndex(ae)

  def eval(): List[Int] = List(ae.evalInt)
}

object Index {
  def apply(idxInTargetAST: GenericAST.ArithExpression): Index = Index(idxInTargetAST.content)
}

case class Slice(start: ArithExpr, step: ArithExpr, end: ArithExpr) extends ArrayAddressor {
  def startIdx: Index = Index(start)
  def +(that: ArithExpr): Slice = Slice(start + that, step, end + that)
  def +(that: Index): Slice = Slice(start + that.ae, step, end + that.ae)
  def *(that: ArithExpr): Slice = Slice(start * that, step, start * that + (start - end))
  def *(that: Index): Slice = Slice(start * that.ae, step, start * that.ae + (start - end))

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArrayAddressor =
    Slice(start.visitAndRebuild(f), step.visitAndRebuild(f), end.visitAndRebuild(f))

  def toTargetAST: SpatialAccelAST.ArrSlice =
    SpatialAccelAST.ArrSlice(ArithExpression(start), ArithExpression(step), ArithExpression(end))

  def eval(): List[Int] = utils.RangeValueGenerator.generateAllValues(RangeAdd(start, step, end)).map(_.evalInt).toList
}

object Slice {
  def continuous(end: ArithExpr): Slice = new Slice(0, 1, end)
  def continuous(start: ArithExpr, end: ArithExpr): Slice = new Slice(start, 1, end)

  def apply(sliceInTargetAST: SpatialAccelAST.ArrSlice): Slice =
    Slice(sliceInTargetAST.start.content, sliceInTargetAST.step.content, sliceInTargetAST.end.content)
}
