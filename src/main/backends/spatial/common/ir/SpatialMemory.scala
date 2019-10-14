package backends.spatial.common.ir

import arithmetic.TypeVar
import ir.ast.Expr
import ir.{Memory, MemoryCollection, TupleType, Type, UnallocatedMemory, UndefType}
import lift.arithmetic._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

private class MemoryAllocationException(msg: String)
  extends IllegalArgumentException(msg)

/**
 * Represents memory in Spatial. Compared to OpenCLMemory, which stores the
 * flat size of the buffer, SpatialMemory stores type to preserve the information
 * about multiple dimensions of the buffer. The size is to be inferred later when needed.
 *
 * @constructor Create a new SpatialMemory object
 * @param variable The variable associated with the memory
 * @param t The type of the variable instantiated in memory
 * @param addressSpace The address space where the memory has been allocated
 */
sealed class SpatialMemory(var variable: Var,
                           val t: Type,
                           val addressSpace: SpatialAddressSpace) extends Memory {
  val size: ArithExpr = t match {
    case UndefType => Cst(-1)
    case aType => Type.getAllocatedSize(aType)
  }

  // size cannot be 0 unless it is the null memory
  try {
    if (size.evalLong == 0)
      throw new MemoryAllocationException("Cannot have a memory of 0 elements!")
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
      case DRAMMemory => SpatialMemory.allocDRAMMemory(t)
      case SRAMMemory => SpatialMemory.allocSRAMMemory(t)
      case RegMemory => SpatialMemory.allocRegMemory(t)
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

case class SpatialMemoryCollection(subMemories: Vector[SpatialMemory],
                                   override val addressSpace: AddressSpaceCollection)
  extends SpatialMemory(Var("SpatialMemoryCollectionTuple"), TupleType(subMemories.map(_.t): _*), addressSpace)
    with MemoryCollection

object SpatialMemoryCollection {
  def apply(mems: Seq[SpatialMemory]): SpatialMemoryCollection = {
    val addressSpace = AddressSpaceCollection(mems.map(_.addressSpace))
    new SpatialMemoryCollection(mems.toVector, addressSpace)
  }
}

/** Represents the NULL Spatial memory object */
object SpatialNullMemory
  extends SpatialMemory(Var("NULL"), UndefType, UndefAddressSpace)


object SpatialMemory {

  def apply(variable: Var, t: Type,
            addressSpace: SpatialAddressSpace): SpatialMemory = {
    new SpatialMemory(variable, t, addressSpace)
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

  def containsDRAMMemory(mem: Memory): Boolean = containsAddressSpace(mem, DRAMMemory)
  def containsSRAMMemory(mem: Memory): Boolean = containsAddressSpace(mem, SRAMMemory)
  def containsRegMemory(mem: Memory): Boolean  = containsAddressSpace(mem, RegMemory)

  /**
    * Return newly allocated memory of `t.size` elements in `addressSpace`
    *
    * @param t The type of the variable associated with the memory object
    * @param addressSpace Address space for the allocated memory
    */
  def allocMemory(t: Type, addressSpace: SpatialAddressSpace) =
    SpatialMemory(Var("", ContinuousRange(Cst(0), Type.getAllocatedSize(t))), t, addressSpace)

  /** Return newly allocated DRAM memory */
  def allocDRAMMemory(dramOutType: Type): SpatialMemory =
    allocMemory(dramOutType, DRAMMemory)

  /** Return newly allocated SRAM memory */
  def allocSRAMMemory(sramOutType: Type): SpatialMemory =
    allocMemory(sramOutType, SRAMMemory)

  /** Return newly allocated Register memory */
  def allocRegMemory(regOutType: Type): SpatialMemory =
    allocMemory(regOutType, RegMemory)
}

/**
 * Represents a SpatialMemory object combined with some metadata additional to the one stored by SpatialMemory.
 * This includes:
 * 1. Whether the memory has been declared yet during code generation
 * 2. The type of the writes to the memory object,
 *    i.e. the type of UserFun that writes to memory or Value that it is initialised with.
 *    For example, in "Map(add(_, 2)) $ (X: ArrayType(Float, N))", the memory object written to
 *    by add has type ArrayType(_, N), but the write type is Float.
 *    The write type can be non-scalar for batch functions or multidimensional Values
 * 3. Whether the memory requires materialisation. The memories of the map bodies of FPatterns such as
 *    SpForeach/SpFold/SpReduce do not require materialisation unless they are written to more than
 *    once (in reduction accumulator).
 *    The memories of the map bodies of FPatterns such as SpMemReduce and SpMemFold always require materialisation.
 * 4. Whether the writes to the memory are implicit such as in the case of the Reduce accumulator, which is
 *    declared (materialised), but not assigned values to explicitly.
 *
 * @constructor Create a new TypedSpatialMemory object
 * @param mem The underlying memory object
 * @param writeT The type of each write to the memory object
 * @param materialised Whether this memory requires materialisation
  */
case class TypedSpatialMemory(mem: SpatialMemory, writeT: Type,
                              var materialised: Boolean,
                              var implicitlyWrittenTo: Boolean) {
  lazy val lengths: Seq[ArithExpr] = Type.getAllocatedLengths(writeT)

  var declared: Boolean = false

  override def toString: String = s"($mem: $writeT)"
}

object TypedSpatialMemory {
  def apply(expr: Expr, materialised: Boolean = true, implicitlyWrittenTo: Boolean = false): TypedSpatialMemory = {
    new TypedSpatialMemory(SpatialMemory.asSpatialMemory(expr.mem), expr.t, materialised, implicitlyWrittenTo)
  }

  def apply(mem: Memory, t: Type, materialised: Boolean, implicitlyWrittenTo: Boolean): TypedSpatialMemory = {
    new TypedSpatialMemory(SpatialMemory.asSpatialMemory(mem), t, materialised, implicitlyWrittenTo)
  }
}
