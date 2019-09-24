package opencl.ir

import ir.AddressSpace
import scala.language.implicitConversions

/**
  * Represents OpenCL address spaces: private, local, global or
  * a collection them in case of tuples.
  *
  * UndefAddressSpace is used when the address space hasn't yet
  * been inferred.
  *
  */
abstract class OpenCLAddressSpace extends AddressSpace {
  def containsAddressSpace(openCLAddressSpace: OpenCLAddressSpace): Boolean
}

object OpenCLAddressSpace {
  implicit def asOpenCLAddressSpace(addressSpace: AddressSpace): OpenCLAddressSpace = {
    addressSpace match {
      case openclAddressSpace: OpenCLAddressSpace => openclAddressSpace
      case _ => throw new IllegalArgumentException
    }
  }
}

object LocalMemory extends OpenCLAddressSpace {
  override def toString = "local"

  def containsAddressSpace(openCLAddressSpace: OpenCLAddressSpace) =
    openCLAddressSpace == this
}

object GlobalMemory extends OpenCLAddressSpace {
  override def toString = "global"

  def containsAddressSpace(openCLAddressSpace: OpenCLAddressSpace) =
    openCLAddressSpace == this
}

object PrivateMemory extends OpenCLAddressSpace {
  override def toString = "private"

    def containsAddressSpace(openCLAddressSpace: OpenCLAddressSpace) =
    openCLAddressSpace == this
}

object UndefAddressSpace extends OpenCLAddressSpace {
  def containsAddressSpace(openCLAddressSpace: OpenCLAddressSpace): Boolean =
    false
}

class UnexpectedAddressSpaceException(message: String) extends Exception(message)

object UnexpectedAddressSpaceException {
  def apply(found: String, expected: String) =
    new UnexpectedAddressSpaceException(s"Found $found, expected: $expected")

  def apply(message: String) = new UnexpectedAddressSpaceException(message)
}

case class AddressSpaceCollection(spaces: Seq[OpenCLAddressSpace])
  extends OpenCLAddressSpace {

  def containsAddressSpace(openCLAddressSpace: OpenCLAddressSpace) =
    spaces.exists(_.containsAddressSpace(openCLAddressSpace))

  def findCommonAddressSpace(): OpenCLAddressSpace = {
    // Try to find common address space which is not the private memory ...
    val noPrivateMem = spaces.filterNot(_== PrivateMemory)
    if (noPrivateMem.isEmpty) // Everything is in private memory
      return PrivateMemory

    val addressSpaces = noPrivateMem.map({
      case coll: AddressSpaceCollection => coll.findCommonAddressSpace()
      case space => space
    })

    // FIXME: Document that the default address space
    // FIXME: is global when the tuple has mixed address space.
    if (addressSpaces.distinct.size == 1)
      addressSpaces.head
    else
      GlobalMemory
  }
}
