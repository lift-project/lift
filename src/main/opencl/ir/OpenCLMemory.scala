package opencl.ir

import arithmetic.TypeVar
import ir._
import ir.ast._
import lift.arithmetic._

private class MemoryAllocationException(msg: String)
  extends IllegalArgumentException(msg)

/** Represents memory in OpenCL as a raw collection of bytes allocated in an
  * OpenCL address space.
  *
  * @constructor Create a new OpenCLMemory object
  * @param variable The variable associated with the memory
  * @param size The size of the memory as numbers bytes
  * @param addressSpace The address space where the memory has been allocated
  */
sealed class OpenCLMemory(var variable: Var,
                          val size: ArithExpr,
                          val addressSpace: OpenCLAddressSpace) extends Memory {

  // size cannot be 0 unless it is the null memory
  try {
    if (size.evalLong == 0)
      throw new MemoryAllocationException("Cannot have a memory of 0 bytes!")
  } catch {
    case NotEvaluableException() => // nothing to do
    case e: Exception => throw e
  }

  // no type variable allowed in the size
  if (TypeVar.getTypeVars(size).nonEmpty)
    throw new MemoryAllocationException("Cannot allocate memory for abstract types")

  {
    // Unknown in the size is likely to be a mistake but can be legit with Array2.0
    // See top comment in OpenCLMemoryAllocator.scala
    val hasUnknown = ArithExpr.visitUntil(size, _ == ?)
    if (hasUnknown)
      println(s"Warning: saw memory with unknown size $this")
  }

  def copy(): OpenCLMemory = {
    addressSpace match {
      case GlobalMemory => OpenCLMemory.allocGlobalMemory(size)
      case LocalMemory => OpenCLMemory.allocLocalMemory(size)
      case PrivateMemory => OpenCLMemory.allocPrivateMemory(size)
      case AddressSpaceCollection(_) => this match {
        case coll: OpenCLMemoryCollection =>
          OpenCLMemoryCollection(coll.subMemories.map(_.copy()))
        case _ => throw new IllegalArgumentException()
      }
      case UndefAddressSpace => this
    }
  }

  /** Debug output */
  override def toString: String = {
    this match {
      case coll: OpenCLMemoryCollection =>
        "[" + coll.subMemories.map(_.toString).reduce(_ + ", " + _) + "]"
      case _ =>
        "{" + variable + "; " + addressSpace + "; " + size + "}"
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[OpenCLMemory]

  override def equals(other: Any): Boolean = other match {
    case that: OpenCLMemory =>
      (that canEqual this) &&
        variable == that.variable &&
        size == that.size &&
        addressSpace == that.addressSpace
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(variable, size, addressSpace)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

case class OpenCLMemoryCollection(subMemories: Vector[OpenCLMemory],
                                  override val addressSpace: AddressSpaceCollection)
  extends OpenCLMemory(Var("Tuple"), subMemories.distinct.map(_.size).reduce(_+_), addressSpace)
    with MemoryCollection

object OpenCLMemoryCollection {
  def apply(mems: Seq[OpenCLMemory]): OpenCLMemoryCollection = {
    val addressSpace = AddressSpaceCollection(mems.map(_.addressSpace))
    new OpenCLMemoryCollection(mems.toVector, addressSpace)
  }
}

/** Represents the NULL OpenCL memory object */
object OpenCLNullMemory
  extends OpenCLMemory(Var("NULL"), Cst(-1), UndefAddressSpace)


object OpenCLMemory {

  def apply(variable: Var, size: ArithExpr,
            addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    new OpenCLMemory(variable, size, addressSpace)
  }

  def getAllMemories(memory: OpenCLMemory): Seq[OpenCLMemory] = memory match {
    case OpenCLMemoryCollection(subMemories, _) => subMemories.flatMap(getAllMemories)
    case _ => Seq(memory)
  }

  def asOpenCLMemory(m: Memory): OpenCLMemory = {
    m match {
      case oclm: OpenCLMemory => oclm
      case UnallocatedMemory => OpenCLNullMemory
      case _ => throw new IllegalArgumentException
    }
  }

  // checking for address spaces
  def containsAddressSpace(mem: Memory,
                           memType: OpenCLAddressSpace): Boolean = {
    mem match {
      case coll: OpenCLMemoryCollection =>
        coll.subMemories.exists(x => x.addressSpace == memType)
      case m: OpenCLMemory => m.addressSpace == memType
      case _ => false
    }
  }

  def getAllMemoryVars(memory: Memory): Seq[Var] = {
    memory match {
      case OpenCLMemoryCollection(subMemories, _) => subMemories.flatMap(getAllMemoryVars)
      case _ => Seq(memory.variable)
    }
  }

  def containsGlobalMemory(mem: Memory): Boolean =
    containsAddressSpace(mem, GlobalMemory)

  def containsLocalMemory(mem: Memory): Boolean =
    containsAddressSpace(mem, LocalMemory)

  def containsPrivateMemory(mem: Memory): Boolean =
    containsAddressSpace(mem, PrivateMemory)

  /** Return newly allocated memory based on the given sizes and the address
    * space of the input memory
    *
    * @param glbOutSize Size in bytes to allocate in global memory
    * @param lclOutSize Size in bytes to allocate in local memory
    * @param addressSpace Address space for allocation
    * @return The newly allocated memory object
    */
  @scala.annotation.tailrec
  def allocMemory(glbOutSize: ArithExpr,
                  lclOutSize: ArithExpr,
                  pvtOutSize: ArithExpr,
                  addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    if (addressSpace == UndefAddressSpace)
      throw new IllegalArgumentException(s"Can't allocate memory in $addressSpace")

    addressSpace match {
      case GlobalMemory => allocGlobalMemory(glbOutSize)
      case LocalMemory => allocLocalMemory(lclOutSize)
      case PrivateMemory => allocPrivateMemory(pvtOutSize)
      case co: AddressSpaceCollection =>
        allocMemory(glbOutSize, lclOutSize, pvtOutSize, co.findCommonAddressSpace())
      case UndefAddressSpace =>
        throw new MemoryAllocationException("Cannot allocate memory in UndefAddressSpace")
    }
  }

  /**
    * Return newly allocated memory of `size` bytes in `addressSpace`
    *
    * @param size Size of the memory to allocate in bytes
    * @param addressSpace Address space for the allocated memory
    */
  def allocMemory(size: ArithExpr, addressSpace: OpenCLAddressSpace) =
    OpenCLMemory(Var("", ContinuousRange(Cst(0), size)), size, addressSpace)

  /** Return newly allocated global memory */
  def allocGlobalMemory(glbOutSize: ArithExpr): OpenCLMemory =
    allocMemory(glbOutSize, GlobalMemory)

  /** Return newly allocated local memory */
  def allocLocalMemory(lclOutSize: ArithExpr): OpenCLMemory =
    allocMemory(lclOutSize, LocalMemory)

  def allocPrivateMemory(size: ArithExpr): OpenCLMemory =
    allocMemory(size, PrivateMemory)
}

/** Represents an OpenCLMemory object combined with a type.
  *
  * @constructor Create a new TypedOpenCLMemory object
  * @param mem The underlying memory object
  * @param t The type associated with the memory object
  */
case class TypedOpenCLMemory(mem: OpenCLMemory, t: Type) {
  override def toString: String = s"($mem: $t)"
}

object TypedOpenCLMemory {
  def apply(expr: Expr): TypedOpenCLMemory = {
    new TypedOpenCLMemory(OpenCLMemory.asOpenCLMemory(expr.mem), expr.t)
  }

  def apply(mem: Memory, t: Type): TypedOpenCLMemory = {
    new TypedOpenCLMemory(OpenCLMemory.asOpenCLMemory(mem), t)
  }
}
