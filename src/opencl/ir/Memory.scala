package opencl.ir

import scala.collection.mutable

import ir._
import ir.Fun

/** represents OpenCL address spaces either: local or global; UndefAddressSpace should be used in case of errors */
abstract class OpenCLAddressSpace

object LocalMemory extends OpenCLAddressSpace {
  override def toString = "local"
}

object GlobalMemory extends OpenCLAddressSpace {
  override def toString = "global"
}

object UndefAddressSpace extends OpenCLAddressSpace

/** Represents memory in OpenCL as a raw collection of bytes allocated in an OpenCL address space.
  *
  * @constructor Create a new OpenCLMemory object
  * @param variable The variable associated with the memory
  * @param size The size of the memory as numbers bytes
  * @param addressSpace The address space where the memory has been allocated
  */
case class OpenCLMemory(var variable: Var, size: Expr, addressSpace: OpenCLAddressSpace) extends Memory {

  // size cannot be 0 unless it is the null memory
  try {
    if (size.eval() == 0)
      throw new IllegalArgumentException
  } catch {
    case _: NotEvaluableException => // nothing to do
    case e: Exception => throw e
  }

  // noe type variable allowed in the size
  if (TypeVar.getTypeVars(size).nonEmpty)
    throw new IllegalArgumentException

  /** Debug output */
  override def toString = "(" + variable + ", " + ExprSimplifier.simplify(size) + ", " + addressSpace + ")"
}

class OpenCLSubMemory(parent: OpenCLMemory, size: Expr, accessFun: (Expr) => Expr)
  extends OpenCLMemory(parent.variable, size, parent.addressSpace)

// Allocate memory globally
class GlobalAllocator {

}
/** Represents the NULL OpenCL memory object */
object OpenCLNullMemory extends OpenCLMemory(Var("NULL"), Cst(-1), UndefAddressSpace)


object OpenCLMemory {

  def getMaxSizeInBytes(t: Type): Expr = {
    Expr.max(getSizeInBytes(t))
  }

  private def getSizeInBytes(t: Type): Expr = {
    ExprSimplifier.simplify(
      t match {
        case st: ScalarType => st.size
        case vt: VectorType => vt.len * getSizeInBytes(vt.scalarT)
        case at: ArrayType => at.len * getSizeInBytes(at.elemT)
        case tt: TupleType => tt.elemsT.map(getSizeInBytes).reduce(_ + _)
        case _ => throw new TypeException(t, "??")
      }
    )
  }

  def asOpenCLMemory(m: Memory): OpenCLMemory = {
    m match {
      case oclm: OpenCLMemory => oclm
      case UnallocatedMemory => OpenCLNullMemory
      case _ => throw new IllegalArgumentException
    }
  }


  /** Return the memory to be used as input of Fun f.
    * If inputMem is NULL new memory is allocated and returned, otherwise inputMem is returned.
    *
    * This function should only allocate memory for a top-level function.
    *
    * @param f Function for which the input should be determined.
    * @param inputMem If NULL memory will be allocated used as input for f.
    *                 If NOT NULL no memory will be allocated and this object will be used as input for f.
    * @return The OpenCLMemory object to be used as input for f.
    *         This is either inputMem or a fresh allocated memory object.
    */
  def fixInput(f: Fun, inputMem: OpenCLMemory): OpenCLMemory = {
    if (inputMem == OpenCLNullMemory) {
      f match {
        case _: Input => OpenCLNullMemory
        case _: CompFun => OpenCLNullMemory
        case _ => OpenCLMemory(Var(ContinousRange(Cst(0), getMaxSizeInBytes(f.inT))), getMaxSizeInBytes(f.inT),
                               GlobalMemory)
      }
    }
    else
      inputMem
  }

  /** Return newly allocated memory based on the given sizes and the address space of the input memory
    *
    * @param glbOutSize Size in bytes to allocate in global memory
    * @param lclOutSize Size in bytes to allocate in local memory
    * @param addressSpace Address space for allocation
    * @param outputType Type used for the memory allocation (TODO: remove this ...)
    * @return The newly allocated memory object
    */
  def allocMemory(glbOutSize: Expr, lclOutSize: Expr,
                  addressSpace: OpenCLAddressSpace, outputType: Type): OpenCLMemory = {
    assert(addressSpace != UndefAddressSpace)

    addressSpace match {
      case GlobalMemory => allocGlobalMemory(glbOutSize, outputType)
      case LocalMemory => allocLocalMemory(lclOutSize, outputType)
    }
  }

  //** Return newly allocated global memory */
  def allocGlobalMemory(glbOutSize: Expr, outputType: Type): OpenCLMemory = {
    OpenCLMemory(Var(ContinousRange(Cst(0), glbOutSize)), glbOutSize, GlobalMemory)
  }

  //** Return newly allocated local memory */
  def allocLocalMemory(lclOutSize: Expr, outputType: Type): OpenCLMemory = {
    OpenCLMemory(Var(ContinousRange(Cst(0), lclOutSize)), lclOutSize, LocalMemory)
  }

  /** Allocate OpenCLMemory objects for a given Fun f
    *
    * @param f The function for which memory should be allocated
    * @param numGlb Number of ...
    * @param numLcl Number of ..
    * @param argInMem The OpenCLMemory object to be used as input by f. Can be NULL then memory will be allocated.
    * @param outputMem The OpenCLMemory object to be used as output by f. Can be NULL then memory will be allocated.
    * @return The OpenCLMemory used as output by f
    */
  def alloc(f: Fun, numGlb: Expr = 1, numLcl: Expr = 1,
            argInMem: OpenCLMemory = OpenCLNullMemory, outputMem: OpenCLMemory = OpenCLNullMemory): OpenCLMemory = {

    // determine the input memory of f
    val inMem = fixInput(f, argInMem)
    assert(inMem != OpenCLNullMemory || f.isInstanceOf[Input] || f.isInstanceOf[CompFun])
    f.inM = inMem

    // size in bytes necessary to hold the result of f in global and local memory
    val maxGlbOutSize = getMaxSizeInBytes(f.ouT) * numGlb
    val maxLclOutSize = getMaxSizeInBytes(f.ouT) * numLcl

    // maximum length of the output array
    val maxLen = Expr.max(Type.getLength(f.ouT))

    // determine the output memory based on the type of f ...
    val result: OpenCLMemory = f match {

      // ... for Input always allocate a new global memory
      case Input(_, _) =>
        assert(outputMem == OpenCLNullMemory)
        allocGlobalMemory(maxGlbOutSize, f.ouT)

      // ... for MapGlbl or MapWrg recurs with the same input memory, but update the global factor
      case MapGlb(_) | MapWrg(_) =>
        alloc(f.asInstanceOf[AbstractMap].f, numGlb * maxLen, numLcl,          inMem, outputMem)

      // ... for MapLcl or MapWarp recurs with the same input memory, but update the global and local factor
      case MapLcl(_) | MapWarp(_) | MapLane(_) =>
        alloc(f.asInstanceOf[AbstractMap].f, numGlb * maxLen, numLcl * maxLen, inMem, outputMem)

      // ... for toGlobal allocate 'mem' in global if output is not yet set or not in global memory ...
      case tg: toGlobal =>
        val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != GlobalMemory)
            allocGlobalMemory(maxGlbOutSize, f.ouT)
          else
            outputMem
        // ... recurs with fresh allocated 'mem' set as output
        alloc(tg.f, numGlb, numLcl, inMem, mem)

      // ... for toLocal allocate 'mem' in local memory if output is not yet set or not in local memory ...
      case tl: toLocal =>
        val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != LocalMemory)
          allocLocalMemory(maxLclOutSize, f.ouT)
        else
          outputMem
        // ... recurs with fresh allocated 'mem' set as output
        alloc(tl.f, numGlb, numLcl, inMem, mem)

      // ... for CompFun allocate from right to left (as the data flows)
      case cf: CompFun =>
        cf.funs.foldRight(inMem)((f, mem) => alloc(f, numGlb, numLcl, mem))

      // ... for Iterate ...
      case it: Iterate => {
        // get sizes in bytes necessary to hold the input and output of the function inside the iterate
        val inSize = getMaxSizeInBytes(it.inT)
        val outSize = getMaxSizeInBytes(it.ouT)
        // get the max from those two
        val largestSize = Expr.max(inSize, outSize)

        // create a swap buffer
        it.swapBuffer = allocMemory(largestSize, largestSize, inMem.addressSpace, ArrayType(it.inT, ?))

        // recurs to allocate memory for the function(s) inside
        alloc(it.f, numGlb, numLcl, inMem)
      }

      // .. for any pattern with a function nested inside recurs
      case fp: FPattern => alloc(fp.f, numGlb, numLcl, inMem, outputMem)

      // ... some function do not allocate anything => return the input memory
      // TODO: add toVector, ToScalar, reorder,...
      case Split(_) | Join() => inMem

      // ... for all remaining functions (e.g. MapSeq and RedSeq) allocate new memory if output is not yet set
      case _ =>
        if (outputMem == OpenCLNullMemory)
          f.ouT match {

            // TODO: could maybe allocated in private memory (need to change slightly the allocator and add toPrivate)
            case ScalarType(_, _) | VectorType(_, _) =>
              allocMemory(maxGlbOutSize, maxLclOutSize, inMem.addressSpace, ArrayType(f.ouT, ?))

            case _ =>
              allocMemory(maxGlbOutSize, maxLclOutSize, inMem.addressSpace, f.ouT)
          }
        else
          outputMem
    }

    // set the output
    assert(result != OpenCLNullMemory)
    f.outM = result

    // finally return the output
    result
  }

}

/** Represents an OpenCLMemory object combined with a type.
  *
  * @constructor Create a new TypedOpenCLMemory object
  * @param mem The underlying memory object
  * @param t The type associated with the memory object
  */
case class TypedOpenCLMemory(val mem: OpenCLMemory, val t: Type)

object TypedOpenCLMemory {
  def apply(mem: Memory, t: Type) = new TypedOpenCLMemory(OpenCLMemory.asOpenCLMemory(mem), t)

  /** Gathers all allocated memory objects of the given Fun f.
    *
    * @param f Function for witch the allocated memory objects should be gathered.
    * @return All memory objects which have been allocated for f.
    */
  def getAllocatedMemory(f: Fun): Array[TypedOpenCLMemory] = {

    // recursively visit all functions and collect input and output (and swap buffer for the iterate)
    val result = Fun.visit(Array[TypedOpenCLMemory]())(f, (f, arr) => f match {

      case it: Iterate => arr :+
                          TypedOpenCLMemory(f.inM, f.inT) :+
                          TypedOpenCLMemory(f.outM, f.ouT) :+
                          TypedOpenCLMemory(it.swapBuffer, ArrayType(it.inT, ?))

      // exclude the user functions (they don't allocate memory and don't work on array types)
      case _: UserFun => arr

      case _ => arr :+ TypedOpenCLMemory(f.inM, f.inT) :+ TypedOpenCLMemory(f.outM, f.ouT)

    })

    val seen = mutable.HashSet[OpenCLMemory]()
    // remove null memory and duplicates while preserving the original order
    result.foldLeft(Array[TypedOpenCLMemory]())( (arr, mem) => {
      val m = mem.mem
      if (seen.contains(m) || m == OpenCLNullMemory)
        arr
      else {
        seen += m
        arr :+ mem
      }
    })
  }
}
