package backends.spatial.ir

import arithmetic.TypeVar
import ir.ast.Expr
import ir.{Memory, Type, UnallocatedMemory}
import lift.arithmetic._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

private class MemoryAllocationException(msg: String)
  extends IllegalArgumentException(msg)

/** Represents memory in Spatial.
  *
  * @constructor Create a new SpatialMemory object
  * @param variable The variable associated with the memory
  * @param size The size of the memory as numbers bytes
  * @param addressSpace The address space where the memory has been allocated
  */
sealed class SpatialMemory(var variable: Var,
                          val size: ArithExpr,
                          val addressSpace: SpatialAddressSpace) extends Memory {

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
    // See top comment in SpatialMemoryAllocator.scala
    val hasUnknown = ArithExpr.visitUntil(size, _ == ?)
    if (hasUnknown)
      println(s"Warning: saw memory with unknown size $this")
  }

  def copy(): SpatialMemory = {
    addressSpace match {
      case SRAMMemory => SpatialMemory.allocSRAMMemory(size)
      case RegMemory => SpatialMemory.allocRegMemory(size)
      case AddressSpaceCollection(_) => this match {
        case coll: SpatialMemoryCollection =>
          SpatialMemoryCollection(coll.subMemories.map(_.copy()))
        case _ => throw new IllegalArgumentException()
      }
      case UndefAddressSpace => this
    }
  }

  /** Debug output */
  override def toString: String = {
    this match {
      case coll: SpatialMemoryCollection =>
        "[" + coll.subMemories.map(_.toString).reduce(_ + ", " + _) + "]"
      case _ =>
        "{" + variable + "; " + addressSpace + "; " + size + "}"
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[SpatialMemory]

  override def equals(other: Any): Boolean = other match {
    case that: SpatialMemory =>
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

case class SpatialMemoryCollection(subMemories: Array[SpatialMemory],
                                  override val addressSpace: AddressSpaceCollection)
  extends SpatialMemory(Var("SpatialMemoryCollectionTuple"),
    subMemories.distinct.map(_.size).reduce(_+_), addressSpace)

object SpatialMemoryCollection {
  def apply(mems: Seq[SpatialMemory]): SpatialMemoryCollection = {
    val addressSpace = AddressSpaceCollection(mems.map(_.addressSpace))
    new SpatialMemoryCollection(mems.toArray, addressSpace)
  }
}

/** Represents the NULL Spatial memory object */
object SpatialNullMemory
  extends SpatialMemory(Var("NULL"), Cst(-1), UndefAddressSpace)


object SpatialMemory {

  def apply(variable: Var, size: ArithExpr,
            addressSpace: SpatialAddressSpace): SpatialMemory = {
    new SpatialMemory(variable, size, addressSpace)
  }

  def getAllMemories(memory: SpatialMemory): Seq[SpatialMemory] = memory match {
    case SpatialMemoryCollection(subMemories, _) => subMemories.flatMap(getAllMemories)
    case _ => Seq(memory)
  }

  def asSpatialMemory(m: Memory): SpatialMemory = {
    m match {
      case spatMem: SpatialMemory => spatMem
      case UnallocatedMemory => SpatialNullMemory
      case _ => throw new IllegalArgumentException
    }
  }

  // checking for address spaces
  def containsAddressSpace(mem: Memory,
                           memType: SpatialAddressSpace): Boolean = {
    mem match {
      case coll: SpatialMemoryCollection =>
        coll.subMemories.exists(x => x.addressSpace == memType)
      case m: SpatialMemory => m.addressSpace == memType
      case _ => false
    }
  }

  def getAllMemoryVars(memory: Memory): Seq[Var] = {
    memory match {
      case SpatialMemoryCollection(subMemories, _) => subMemories.flatMap(getAllMemoryVars)
      case _ => Seq(memory.variable)
    }
  }

  def containsSRAMMemory(mem: Memory): Boolean =
    containsAddressSpace(mem, SRAMMemory)

  def containsRegMemory(mem: Memory): Boolean =
    containsAddressSpace(mem, RegMemory)

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
                  addressSpace: SpatialAddressSpace): SpatialMemory = {
    if (addressSpace == UndefAddressSpace)
      throw new IllegalArgumentException(s"Can't allocate memory in $addressSpace")

    addressSpace match {
      case SRAMMemory => allocSRAMMemory(glbOutSize)
      case RegMemory => allocRegMemory(pvtOutSize)
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
  def allocMemory(size: ArithExpr, addressSpace: SpatialAddressSpace) =
    SpatialMemory(Var("", ContinuousRange(Cst(0), size)), size, addressSpace)

  /** Return newly allocated global memory */
  def allocSRAMMemory(glbOutSize: ArithExpr): SpatialMemory =
    allocMemory(glbOutSize, GlobalMemory)

  /** Return newly allocated local memory */
  def allocLocalMemory(lclOutSize: ArithExpr): SpatialMemory =
    allocMemory(lclOutSize, LocalMemory)

  def allocRegMemory(size: ArithExpr): SpatialMemory =
    allocMemory(size, PrivateMemory)
}

/** Represents an SpatialMemory object combined with a type.
  *
  * @constructor Create a new TypedSpatialMemory object
  * @param mem The underlying memory object
  * @param t The type associated with the memory object
  */
case class TypedSpatialMemory(mem: SpatialMemory, t: Type) {
  override def toString: String = s"($mem: $t)"
}

object TypedSpatialMemory {
  def apply(expr: Expr): TypedSpatialMemory = {
    new TypedSpatialMemory(SpatialMemory.asSpatialMemory(expr.mem), expr.t)
  }

  def apply(mem: Memory, t: Type): TypedSpatialMemory = {
    new TypedSpatialMemory(SpatialMemory.asSpatialMemory(mem), t)
  }
}
