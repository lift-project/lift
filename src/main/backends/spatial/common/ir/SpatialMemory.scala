package backends.spatial.common.ir

import arithmetic.TypeVar
import ir.ast.{Expr, FunCall}
import ir.{Memory, MemoryCollection, TupleType, Type, UnallocatedMemory, UndefType}
import lift.arithmetic._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.collection.mutable

private class MemoryAllocationException(msg: String)
  extends IllegalArgumentException(msg)

/**
 * Represents memory in Spatial. Compared to OpenCLMemory, which stores the
 * flat size of the buffer, SpatialMemory stores the type to preserve the information
 * about multiple dimensions of the buffer.
 *
 * @constructor Create a new SpatialMemory object
 * @param variable The variable associated with the memory
 * @param t The type of the variable instantiated in memory
 * @param addressSpace The address space where the memory has been allocated
 * @param bufferHazard Whether this memory has a potential buffer hazard from an undefined order
 *                     of writing to it by nodes like Reduce/Fold
 */
sealed class SpatialMemory(var variable: Var,
                           val t: Type,
                           val addressSpace: SpatialAddressSpace,
                           var bufferHazard: Boolean = false) extends Memory {
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
      case LiteralMemory => SpatialMemory.allocLiteralMemory(t)
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
  def containsRegMemory(mem: Memory): Boolean = containsAddressSpace(mem, RegMemory)
  def containsLiteralMemory(mem: Memory): Boolean = containsAddressSpace(mem, LiteralMemory)
  def containsPrivateMemory(mem: Memory): Boolean = containsRegMemory(mem) || containsLiteralMemory(mem)

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

  /** Return newly allocated Literal memory */
  def allocLiteralMemory(literalOutType: Type): SpatialMemory =
    allocMemory(literalOutType, LiteralMemory)
}

/**
 * TODO: update
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
 * 4. The scopes within which all writes to the memory are implicit. In case of the Fold accumulator, its
 *    initial value might be explicitly written to outside the scope of the Fold; the reduce function inside the
 *    scope writes to the accumulator implicitly by returning the result
 *    TODO: potentially reuse views or AccessInfo for typeInMem
 *    TODO: infer implicitlyReadFrom in a cleaner way
 *
 * @constructor Create a new TypedSpatialMemory object
 * @param mem The underlying memory object
 * @param typeInMem The type of each write to the memory object
// * @param implicitlyReadFrom Whether the reads are explicit or implicit through a placeholder in an anonymous
 *                           function. TODO: add scopes or infer it in a cleaner way (not all reads might be implicit)
 * @param implicitWriteScope The scope within which all writes are implicit (might need more than one in the future)
 */
case class ContextualSpatialMemory(mem: SpatialMemory, typeInMem: Type,
                                   var implicitReadScope: Option[FunCall],
                                   var implicitWriteScope: Option[FunCall]) {
  lazy val lengths: Seq[ArithExpr] = Type.getAllocatedLengths(typeInMem)

  var declared: Boolean = false

  override def toString: String = s"($mem: $typeInMem)"

  def inImplicitReadScope(scope: mutable.Stack[FunCall]): Boolean =
    implicitReadScope.isDefined && scope.contains(implicitReadScope.get)

  def inImplicitWriteScope(scope: mutable.Stack[FunCall]): Boolean =
    implicitWriteScope.isDefined && scope.contains(implicitWriteScope.get)
}

object ContextualSpatialMemory {
  def apply(expr: Expr,
            implicitReadScope: Option[FunCall] = None,
            implicitWriteScope: Option[FunCall] = None): ContextualSpatialMemory = {
    new ContextualSpatialMemory(SpatialMemory.asSpatialMemory(expr.mem), expr.t,
      implicitReadScope, implicitWriteScope)
  }

  def apply(mem: Memory, t: Type,
            implicitReadScope: Option[FunCall],
            implicitWriteScope: Option[FunCall]): ContextualSpatialMemory = {
    new ContextualSpatialMemory(SpatialMemory.asSpatialMemory(mem), t,
      implicitReadScope, implicitWriteScope)
  }
}
