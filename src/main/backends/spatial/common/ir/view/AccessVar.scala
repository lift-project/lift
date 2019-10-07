package backends.spatial.common.ir.view

import lift.arithmetic.{ArithExpr, ExtensibleVar, Range, RangeUnknown, SimplifiedExpr, Var}

/**
 * An arithmetic expression that performs an access to `array[idx]` or `array[start::stop]`
 *
 * @param array variable referencing the array
 * @param idx multidimensional index
 */
case class AccessVar(array: Var, idx: List[Index], r: Range = RangeUnknown, override val fixedId: Option[Long] = None)
  extends ExtensibleVar("", r, fixedId) {

  override def copy(r: Range): AccessVar = AccessVar(array.copy(array.range), idx, r, Some(id))

  override def cloneSimplified() = new AccessVar(array, idx, r, Some(id)) with SimplifiedExpr

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr =
    f(AccessVar(
      array.visitAndRebuild(f).asInstanceOf[Var],
      idx.map(_.visitAndRebuild(f)),
      range.visitAndRebuild(f),
      Some(id)
    ))
}