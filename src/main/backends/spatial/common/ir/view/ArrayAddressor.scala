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

class Slice(val start: ArithExpr, val end: ArithExpr,
            var burstFactor: Option[ArithExpr] = None) extends ArrayAddressor {
  def startIdx: Index = Index(start)
  def +(that: ArithExpr): Slice = Slice(start + that, end + that)
  def +(that: Index): Slice = Slice(start + that.ae, end + that.ae)
  def *(that: ArithExpr): Slice = Slice(start * that, start * that + (end - start))
  def *(that: Index): Slice = Slice(start * that.ae, start * that.ae + (end - start))

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArrayAddressor =
    Slice(start.visitAndRebuild(f), end.visitAndRebuild(f))

  def toTargetAST: SpatialAccelAST.ArrSlice =
    SpatialAccelAST.ArrSlice(ArithExpression(start), ArithExpression(end),
      if (burstFactor.isDefined) Some(ArithExpression(burstFactor.get)) else None)

  def eval(): List[Int] = utils.RangeValueGenerator.generateAllValues(RangeAdd(start, 1, end)).map(_.evalInt).toList
}

object Slice {
  def continuous(end: ArithExpr): Slice = new Slice(0, end)
  def continuous(start: ArithExpr, end: ArithExpr): Slice = new Slice(start, end)

  def apply(start: ArithExpr, end: ArithExpr, burstFactor: Option[ArithExpr] = None): Slice =
    new Slice(start, end, burstFactor)

  def apply(sliceInTargetAST: SpatialAccelAST.ArrSlice): Slice =
    Slice(sliceInTargetAST.start.content, sliceInTargetAST.end.content)

  def unapply(arg: Slice): Option[(ArithExpr, ArithExpr)] = Some((arg.start, arg.end))
}
