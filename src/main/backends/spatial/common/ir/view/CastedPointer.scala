package backends.spatial.common.ir.view

import backends.spatial.common.ir.SpatialAddressSpace
import ir.ScalarType
import lift.arithmetic.{ArithExpr, ExtensibleVar, Range, RangeUnknown, SimplifiedExpr, Var}

/**
 * Variable storing a casted pointer.
 * `CastedPointer(v, type, offset)` generates the following Spatial Scala code: `(v(offset + ..).asInstanceOf[type])`
 */
case class CastedPointer(ptr: Var, ty: ScalarType, offset: Index, addressSpace: SpatialAddressSpace,
                         override val fixedId: Option[Long] = None)
  extends ExtensibleVar("", RangeUnknown, fixedId) {

  override def copy(r: Range): CastedPointer = {
    CastedPointer(ptr.copy(ptr.range), ty, offset, addressSpace)
  }

  override def cloneSimplified() = new CastedPointer(ptr, ty, offset, addressSpace, Some(id)) with SimplifiedExpr

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr =
    f(CastedPointer(
      ptr.visitAndRebuild(f).asInstanceOf[Var],
      ty,
      offset.visitAndRebuild(f),
      addressSpace
    ))

  override lazy val toString: String =
    s"#error THE VARIABLE $ptr THIS SHOULD NEVER BE PRINTED. USE Printer" +
      s".ToString(...) INSTEAD!\n" +
      s"($addressSpace ${ty
        .name}*)($ptr" +
      s" + " +
      s"$offset)"
}
