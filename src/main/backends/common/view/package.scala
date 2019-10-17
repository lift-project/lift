package backends.common

import ir.{AddressSpace, ArrayType, Type}
import lift.arithmetic.ArithExpr

package object view {
  /**
   * A shorthand used to talk about AccessInfo.
   * It represents the access information for one dimension whereas AccessInfo
   * gives the access information for an array of arbitrary dimension in
   * different address spaces. Hence, AccessInfo can be seen as a 3-tuple of
   * `List[SingleAccess]`
   */
  type SingleAccess = (Type => ArrayType, ArithExpr)
  type MemoryAccessInfoCL = collection.mutable.ListMap[AddressSpace, List[SingleAccess]]

  abstract class AccessInfo() {
    def collection: Seq[AccessInfo]
  }
}
