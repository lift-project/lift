package opencl.ir

import apart.arithmetic._
import arithmetic.TypeVar
import ir._
import ir.ast._

import scala.collection.mutable

/** Represents OpenCL address spaces either: local or global;
  * UndefAddressSpace should be used in case of errors */
abstract class OpenCLAddressSpace

object LocalMemory extends OpenCLAddressSpace {
  override def toString = "local"
}

object GlobalMemory extends OpenCLAddressSpace {
  override def toString = "global"
}

object PrivateMemory extends OpenCLAddressSpace {
  override def toString = "private"
}

class AddressSpaceCollection(spaces: Seq[OpenCLAddressSpace])
  extends OpenCLAddressSpace {

  def findCommonAddressSpace(): OpenCLAddressSpace = {
    // try to find common address space which is not the private memory ...
    val noPrivateMem = spaces.filterNot(_== PrivateMemory)
    if (noPrivateMem.isEmpty) { // everything is in private memory
      return PrivateMemory
    }

    val addessSpaces = noPrivateMem.map({
      case coll: AddressSpaceCollection => coll.findCommonAddressSpace()
      case space => space
    })

    if (addessSpaces.forall(_ == addessSpaces.head)) {
      addessSpaces.head
    } else {
      throw new IllegalArgumentException("Could not determine " +
                                         "common addressSpace")
    }
  }
}

object UndefAddressSpace extends OpenCLAddressSpace

/** Represents memory in OpenCL as a raw collection of bytes allocated in an
  * OpenCL address space.
  *
  * @constructor Create a new OpenCLMemory object
  * @param variable The variable associated with the memory
  * @param size The size of the memory as numbers bytes
  * @param addressSpace The address space where the memory has been allocated
  */
class OpenCLMemory(var variable: Var,
                   val size: ArithExpr,
                   val addressSpace: OpenCLAddressSpace) extends Memory {

  // size cannot be 0 unless it is the null memory
  try {
    if (size.eval == 0)
      throw new IllegalArgumentException
  } catch {
    case _: NotEvaluableException => // nothing to do
    case e: Exception => throw e
  }

  // noe type variable allowed in the size
  if (TypeVar.getTypeVars(size).nonEmpty)
    throw new IllegalArgumentException

  def copy(): OpenCLMemory = {
    addressSpace match {
      case GlobalMemory => OpenCLMemory.allocGlobalMemory(size)
      case LocalMemory => OpenCLMemory.allocLocalMemory(size)
      case PrivateMemory => OpenCLMemory.allocPrivateMemory(size)
      case _ => this
    }
  }

  /** Debug output */
  override def toString: String = {
    this match {
      case coll: OpenCLMemoryCollection =>
        "coll(" + coll.subMemories.map(_.toString).reduce(_ + ", " + _) +
        " | " + coll.addressSpace + ")"
      case _ =>
        "size: " + size + "; addressspace: " + addressSpace
    }
  }
}

class OpenCLMemoryCollection(val subMemories: Array[OpenCLMemory],
                             override val addressSpace: AddressSpaceCollection)
  extends OpenCLMemory(Var("Tuple"), subMemories.map(_.size).reduce(_+_),
                       addressSpace)

object OpenCLMemoryCollection {
  def apply(mems: Seq[OpenCLMemory]) = {
    val addressSpace = new AddressSpaceCollection(mems.map(_.addressSpace))
    new OpenCLMemoryCollection(mems.toArray, addressSpace)
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
  def allocMemory(glbOutSize: ArithExpr,
                  lclOutSize: ArithExpr,
                  pvtOutSize: ArithExpr,
                  addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    assert(addressSpace != UndefAddressSpace)

    addressSpace match {
      case GlobalMemory => allocGlobalMemory(glbOutSize)
      case LocalMemory => allocLocalMemory(lclOutSize)
      case PrivateMemory => allocPrivateMemory(pvtOutSize)
    }
  }

  /** Return newly allocated global memory */
  def allocGlobalMemory(glbOutSize: ArithExpr): OpenCLMemory = {
    OpenCLMemory(Var(ContinuousRange(Cst(0), glbOutSize)),
                 glbOutSize, GlobalMemory)
  }

  /** Return newly allocated local memory */
  def allocLocalMemory(lclOutSize: ArithExpr): OpenCLMemory = {
    OpenCLMemory(Var(ContinuousRange(Cst(0), lclOutSize)),
                 lclOutSize, LocalMemory)
  }

  def allocPrivateMemory(size: ArithExpr): OpenCLMemory = {
    OpenCLMemory(Var(ContinuousRange(Cst(0), size)), size, PrivateMemory)
  }

  def getMaxSizeInBytes(t: Type): ArithExpr = {
    ArithExpr.max(getSizeInBytes(t))
  }

  def getSizeInBytes(t: Type): ArithExpr = t match {
    case st: ScalarType => st.size
    case vt: VectorType => vt.len * getSizeInBytes(vt.scalarT)
    case at: ArrayType => at.len * getSizeInBytes(at.elemT)
    case tt: TupleType => tt.elemsT.map(getSizeInBytes).reduce(_ + _)
    case _ => throw new TypeException(t, "??")
  }
}

/** Represents an OpenCLMemory object combined with a type.
  *
  * @constructor Create a new TypedOpenCLMemory object
  * @param mem The underlying memory object
  * @param t The type associated with the memory object
  */
case class TypedOpenCLMemory(mem: OpenCLMemory, t: Type) {
  override def toString = "(" + mem.toString +"; " + t.toString + ")"
}

object TypedOpenCLMemory {
  def apply(mem: Memory, t: Type): TypedOpenCLMemory =
    new TypedOpenCLMemory(OpenCLMemory.asOpenCLMemory(mem), t)

  /** Gathers all allocated memory objects of the given Fun f.
    *
    * @param expr Expression for witch the allocated memory objects should be
    *             gathered.
    * @return All memory objects which have been allocated for f.
    */
  def getAllocatedMemory(expr: Expr,
                         params: Array[Param],
                         includePrivate: Boolean = false
                        ): Array[TypedOpenCLMemory] = {

    // recursively visit all functions and collect input and output
    // (and swap buffer for the iterate)
    val result = Expr.visitWithState(Array[TypedOpenCLMemory]())(expr,
                                                                 (exp, arr) =>
      exp match {
        case call: FunCall =>
          call.f match {
            case it: Iterate =>
              arr :+
              TypedOpenCLMemory(call.args.head.mem, call.args.head.t) :+
              TypedOpenCLMemory(call.mem, call.t) :+
              TypedOpenCLMemory(it.swapBuffer, ArrayType(call.args.head.t, ?))

            case r: AbstractPartRed =>
              // exclude the iniT (TODO: only if it is already allocated ...)

              val inMem = TypedOpenCLMemory( call.args(1).mem, call.args(1).t )

              arr :+ inMem

            case z: Zip =>
              arr ++ call.args.map( e => TypedOpenCLMemory(e.mem, e.t) )

            case f: Filter =>
              arr ++ call.args.map( e => TypedOpenCLMemory(e.mem, e.t) )

            case uf: UserFun =>
              arr :+
              TypedOpenCLMemory(call.argsMemory, call.argsType) :+
              TypedOpenCLMemory(call.mem, call.t)

            case _ =>
              arr :+ TypedOpenCLMemory(call.mem, call.t)
          }
        case p: Param => arr :+ TypedOpenCLMemory(p.mem, p.t)
      })

    val resultWithoutCollections = result.flatMap(tm => tm.mem match {
      case coll: OpenCLMemoryCollection =>
        coll.subMemories.zipWithIndex.map({
          case (m, i) => TypedOpenCLMemory(m, Type.getTypeAtIndex(tm.t, i))
        })
      case ocl: OpenCLMemory => Array(tm)
    })

    val seen = mutable.HashSet[OpenCLMemory]()
    // remove null memory and duplicates while preserving the original order
    resultWithoutCollections.foldLeft(Array[TypedOpenCLMemory]())(
      (arr, mem) => {
        val m = mem.mem
        if (   seen.contains(m)
            || m == OpenCLNullMemory
            || m.isInstanceOf[OpenCLMemoryCollection]

            // m is in private memory but not an parameter => trow away
            || (   m.addressSpace == PrivateMemory
                && !params.exists(p => p.mem == m))
                && !includePrivate) {
          arr
        } else {
          seen += m
          params.find(param => m == param.mem) match {
            case Some(param) => arr :+ TypedOpenCLMemory(m, param.t)
            case _ => arr :+ mem
          }
        }
    })
  }
}
