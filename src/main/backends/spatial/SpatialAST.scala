package backends.spatial

import backends.spatial.ir.SpatialAddressSpace

object SpatialAST {
  trait SpatialAddressSpaceOperator {
    val addressSpace: SpatialAddressSpace
  }
}
