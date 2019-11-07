package backends.spatial.common.ir

import ir.AddressSpace
import scala.language.implicitConversions

abstract class SpatialAddressSpace extends AddressSpace {
  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean
}

object SpatialAddressSpace {
  implicit def asSpatialAddressSpace(addressSpace: AddressSpace): SpatialAddressSpace = {
    addressSpace match {
      case spatialAddressSpace: SpatialAddressSpace => spatialAddressSpace
      case _ => throw new IllegalArgumentException
    }
  }
}

trait HostAllocatedMemory
trait WritableMemory
trait SharedMemory // Among Accel block PEs, not between host and accel
trait ScalarMemory

object DRAMMemory extends SpatialAddressSpace with WritableMemory with SharedMemory with HostAllocatedMemory {
  override def toString = "DRAM"

  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean =
    spatialAddressSpace == this
}

object SRAMMemory extends SpatialAddressSpace with WritableMemory with SharedMemory {
  override def toString = "SRAM"

  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean =
    spatialAddressSpace == this
}

object ArgOutMemory extends SpatialAddressSpace with WritableMemory with SharedMemory
                                                with ScalarMemory with HostAllocatedMemory {
  override def toString = "ArgOut"

  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean =
    spatialAddressSpace == this
}

object RegMemory extends SpatialAddressSpace with WritableMemory with ScalarMemory {
  override def toString = "Reg"

  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean =
    spatialAddressSpace == this
}

object LiteralMemory extends SpatialAddressSpace with ScalarMemory {
  override def toString = "Literal"

  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean =
    spatialAddressSpace == this
}

object UndefAddressSpace extends SpatialAddressSpace {
  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean =
    false
}

class UnexpectedAddressSpaceException(message: String) extends Exception(message)

object UnexpectedAddressSpaceException {
  def apply(found: String, expected: String) =
    new UnexpectedAddressSpaceException(s"Found $found, expected: $expected")

  def apply(message: String) = new UnexpectedAddressSpaceException(message)
}

case class AddressSpaceCollection(spaces: Seq[SpatialAddressSpace])
  extends SpatialAddressSpace {

  def containsAddressSpace(spatialAddressSpace: SpatialAddressSpace): Boolean =
    spaces.exists(_.containsAddressSpace(spatialAddressSpace))

  def findCommonAddressSpace(): SpatialAddressSpace = {
    // Try to find common address space which is not a private memory ...
    val (sharedMem, privateMem) =
      spaces.partition(s => s.isInstanceOf[SharedMemory] && s.isInstanceOf[WritableMemory])

    if (sharedMem.isEmpty) // Everything is in private memory
      return privateMem.head

    val addressSpaces = sharedMem.map({
      case coll: AddressSpaceCollection => coll.findCommonAddressSpace()
      case space => space
    })

    if (addressSpaces.distinct.size == 1)
      addressSpaces.head
    else
      DRAMMemory
  }
}