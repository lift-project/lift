package backends.spatial.ir

abstract class SpatialAddressSpace {
  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean
}

object SRAMMemory extends SpatialAddressSpace {
  override def toString = "SRAM"

  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean =
    spatialAddressSpace == this
}

object RegMemory extends SpatialAddressSpace {
  override def toString = "Reg"

  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean =
    spatialAddressSpace == this
}

object UndefAddressSpace extends SpatialAddressSpace {
  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean =
    false
}

case class AddressSpaceCollection(spaces: Seq[SpatialAddressSpace])
  extends SpatialAddressSpace {

  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace) =
    spaces.exists(_.containsAddressSpace(spatialAddressSpace))

  def findCommonAddressSpace(): SpatialAddressSpace = {
    // Try to find common address space which is not the register memory ...
    val noRegMem = spaces.filterNot(_== RegMemory)
    if (noRegMem.isEmpty) // Everything is in private memory
      return RegMemory

    val addressSpaces = noRegMem.map({
      case coll: AddressSpaceCollection => coll.findCommonAddressSpace()
      case space => space
    })

    if (addressSpaces.distinct.size == 1)
      addressSpaces.head
    else
      SRAMMemory // TODO: decide which memory should be default
  }
}