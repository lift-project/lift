package backends.spatial.common

import backends.spatial.common.ir.SpatialAddressSpace

object SpatialAST {
  trait SpatialAddressSpaceOperator {
    val addressSpace: SpatialAddressSpace
  }
}
