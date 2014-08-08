package opencl.ir

import scala.collection.mutable

import ir._
import ir.FunCall

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
case class OpenCLMemory(var variable: Var, size: ArithExpr, addressSpace: OpenCLAddressSpace) extends Memory {

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

class OpenCLSubMemory(parent: OpenCLMemory, size: ArithExpr, accessFun: (ArithExpr) => ArithExpr)
  extends OpenCLMemory(parent.variable, size, parent.addressSpace)

class OpenCLMemoryCollection(val subMemories: Array[OpenCLMemory])
  extends OpenCLMemory(Var("Tuple"), subMemories.map(_.size).reduce(_+_), subMemories.head.addressSpace) // TODO: think about address space ...

object OpenCLMemoryCollection {
  def apply(subMemories: OpenCLMemory*) = new OpenCLMemoryCollection(Array(subMemories:_*))
}

// Allocate memory globally
class GlobalAllocator {

}
/** Represents the NULL OpenCL memory object */
object OpenCLNullMemory extends OpenCLMemory(Var("NULL"), Cst(-1), UndefAddressSpace)


object OpenCLMemory {

  def getMaxSizeInBytes(t: Type): ArithExpr = {
    ArithExpr.max(getSizeInBytes(t))
  }

  private def getSizeInBytes(t: Type): ArithExpr = {
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

/*
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
  def fixInput(f: FunExpr, inputMem: OpenCLMemory): OpenCLMemory = {
    if (inputMem == OpenCLNullMemory) {
      f.f match {
        case _: Zip => OpenCLNullMemory
        case _ if f.inT == NoType => OpenCLNullMemory
        case _ =>
          println("ALLOCATE FOR INPUT")
          OpenCLMemory(Var(ContinousRange(Cst(0), getMaxSizeInBytes(f.inT))), getMaxSizeInBytes(f.inT), GlobalMemory)
      }
    }
    else
      inputMem
  }
  */

  /** Return newly allocated memory based on the given sizes and the address space of the input memory
    *
    * @param glbOutSize Size in bytes to allocate in global memory
    * @param lclOutSize Size in bytes to allocate in local memory
    * @param addressSpace Address space for allocation
    * @return The newly allocated memory object
    */
  def allocMemory(glbOutSize: ArithExpr, lclOutSize: ArithExpr,
                  addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    assert(addressSpace != UndefAddressSpace)

    addressSpace match {
      case GlobalMemory => allocGlobalMemory(glbOutSize)
      case LocalMemory => allocLocalMemory(lclOutSize)
    }
  }

  //** Return newly allocated global memory */
  def allocGlobalMemory(glbOutSize: ArithExpr): OpenCLMemory = {
    OpenCLMemory(Var(ContinousRange(Cst(0), glbOutSize)), glbOutSize, GlobalMemory)
  }

  //** Return newly allocated local memory */
  def allocLocalMemory(lclOutSize: ArithExpr): OpenCLMemory = {
    OpenCLMemory(Var(ContinousRange(Cst(0), lclOutSize)), lclOutSize, LocalMemory)
  }

  /** Allocate OpenCLMemory objects for a given Fun f
    *
    * @param expr The expression for which memory should be allocated
    * @param numGlb Number of ...
    * @param numLcl Number of ..
    * @param inputMem The OpenCLMemory object to be used as input by f. Can be NULL then memory will be allocated.
    * @param outputMem The OpenCLMemory object to be used as output by f. Can be NULL then memory will be allocated.
    * @return The OpenCLMemory used as output by f
    */
  def alloc(expr: Expr, numGlb: ArithExpr = 1, numLcl: ArithExpr = 1,
            inputMem: OpenCLMemory = OpenCLNullMemory, outputMem: OpenCLMemory = OpenCLNullMemory): OpenCLMemory = {

    val result = expr match {
      // if this is a parameter or value set the output type to ...
      case Param() | Value(_) =>
        // ... the output of the parameter if it is set, or ...
        if (expr.inM != UnallocatedMemory) {
          // ... set the input to the output (so reuse the given input) ...
          expr.outM = expr.inM
        } else if (inputMem != OpenCLNullMemory) {
          // ... use the provided given input memory
          expr.outM = inputMem
        } else throw new IllegalArgumentException("PANIC!")

        // .. and return the output memory
        OpenCLMemory.asOpenCLMemory(expr.outM)

      case call: FunCall =>

        val inMs = if (call.args.nonEmpty) {
          // allocate arguments and get the output memories from there
          call.args.map(alloc(_, numGlb, numLcl, inputMem))
        } else {
          Seq(inputMem)
        }

        // determine the input memory of f
        if (call.inM == UnallocatedMemory) {
          assert(inMs.forall(_ != OpenCLNullMemory) || call.isInstanceOf[Zip] || call.inT == NoType)
          call.inM = inMs(0) // TODO: Should this really just be the first Memory ???
        }

        // a shortcut to the output memory of the first argument
        val inMem = OpenCLMemory.asOpenCLMemory(call.inM)

        // size in bytes necessary to hold the result of f in global and local memory
        val maxGlbOutSize = getMaxSizeInBytes(call.outT) * numGlb
        val maxLclOutSize = getMaxSizeInBytes(call.outT) * numLcl

        // maximum length of the output array
        val maxLen = ArithExpr.max(Type.getLength(call.outT))

        // determine the output memory based on the type of f ...
        call.f match {

          case z: Zip =>
            if (inMs.length != 2) throw new NumberOfArgumentsException
            call.inM = OpenCLMemoryCollection(inMs(0), inMs(1))
            OpenCLMemory.asOpenCLMemory(call.inM) // input == output


          // ... for MapGlbl or MapWrg recurs with the same input memory, but update the global factor
          case MapGlb(_) | MapWrg(_) =>
            if (inMs.length != 1) throw new NumberOfArgumentsException
            alloc(call.f.asInstanceOf[AbstractMap].f.body, numGlb * maxLen, numLcl, inMem, outputMem)

          // ... for MapLcl or MapWarp recurs with the same input memory, but update the global and local factor
          case MapLcl(_) | MapWarp(_) | MapLane(_) =>
            if (inMs.length != 1) throw new NumberOfArgumentsException
            alloc(call.f.asInstanceOf[AbstractMap].f.body, numGlb * maxLen, numLcl * maxLen, inMem, outputMem)

          // ... for toGlobal allocate 'mem' in global if output is not yet set or not in global memory ...
          case tg: toGlobal =>
            if (inMs.length != 1) throw new NumberOfArgumentsException
            val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != GlobalMemory)
              allocGlobalMemory(maxGlbOutSize)
            else
              outputMem
            // ... recurs with fresh allocated 'mem' set as output
            alloc(tg.f.body, numGlb, numLcl, inMem, mem)

          // ... for toLocal allocate 'mem' in local memory if output is not yet set or not in local memory ...
          case tl: toLocal =>
            if (inMs.length != 1) throw new NumberOfArgumentsException
            val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != LocalMemory)
              allocLocalMemory(maxLclOutSize)
            else
              outputMem
            // ... recurs with fresh allocated 'mem' set as output
            alloc(tl.f.body, numGlb, numLcl, inMem, mem)

          // ... for CompFun allocate from right to left (as the data flows)
          case cf: CompFunDef =>
            if (inMs.length != 1) throw new NumberOfArgumentsException
            // combine the parameter of the first function to call with the type inferred from the argument
            cf.funs.last.params(0).inM = inMem

            cf.funs.foldRight(inMem)((f, mem) => alloc(f.body, numGlb, numLcl, mem))

          // ... for Iterate ...
          case it: Iterate =>
            if (inMs.length != 1) throw new NumberOfArgumentsException
            val fIter = call.asInstanceOf[IterateCall]

            // get sizes in bytes necessary to hold the input and output of the function inside the iterate
            val inSize = getMaxSizeInBytes(fIter.inT)
            val outSize = getMaxSizeInBytes(fIter.outT)
            // get the max from those two
            val largestSize = ArithExpr.max(inSize, outSize)

            // create a swap buffer
            fIter.swapBuffer = allocMemory(largestSize, largestSize, inMem.addressSpace)

            // recurs to allocate memory for the function(s) inside
            alloc(it.f.body, numGlb, numLcl, inMem)

          // .. for any pattern with a function nested inside recurs
          case fp: FPattern =>
            if (inMs.length != 1) throw new NumberOfArgumentsException
            alloc(fp.f.body, numGlb, numLcl, inMem, outputMem)

          // ... some function do not allocate anything => return the input memory
          case Split(_) | Join() | ReorderStride() | asVector(_) | asScalar() /*| Vectorize(_,_)*/ =>
            if (inMs.length != 1) throw new NumberOfArgumentsException
            inMem

          // ... for all remaining functions (e.g. MapSeq and RedSeq) allocate new memory if output is not yet set
          case _ =>
            if (inMs.length != 1) throw new NumberOfArgumentsException
            if (outputMem == OpenCLNullMemory)
              call.outT match {

                // TODO: could maybe allocated in private memory (need to change slightly the allocator and add toPrivate)
                case ScalarType(_, _) | VectorType(_, _) =>
                  allocMemory(maxGlbOutSize, maxLclOutSize, inMem.addressSpace)

                case _ =>
                  allocMemory(maxGlbOutSize, maxLclOutSize, inMem.addressSpace)
              }
            else
              outputMem
        }

      }

      // set the output
      assert(result != OpenCLNullMemory)
      expr.outM = result

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
case class TypedOpenCLMemory(mem: OpenCLMemory, t: Type) {
  override def toString = "(" + mem.toString +"; " + t.toString + ")"
}

object TypedOpenCLMemory {
  def apply(mem: Memory, t: Type) = new TypedOpenCLMemory(OpenCLMemory.asOpenCLMemory(mem), t)

  /** Gathers all allocated memory objects of the given Fun f.
    *
    * @param expr Expression for witch the allocated memory objects should be gathered.
    * @return All memory objects which have been allocated for f.
    */
  def getAllocatedMemory(expr: Expr): Array[TypedOpenCLMemory] = {

    // recursively visit all functions and collect input and output (and swap buffer for the iterate)
    val result = Expr.visit(Array[TypedOpenCLMemory]())(expr, (exp, arr) =>
      exp match {
        case call: FunCall =>
          call.f match {
            case it: Iterate =>
            val fIter = call.asInstanceOf[IterateCall]
            arr :+
            TypedOpenCLMemory(fIter.inM, fIter.inT) :+
            TypedOpenCLMemory(fIter.outM, fIter.outT) :+
            TypedOpenCLMemory(fIter.swapBuffer, ArrayType(fIter.inT, ?))

            // exclude the user functions (they don't allocate memory and don't work on array types)
            case _: UserFunDef => arr

            case z: Zip =>
              arr ++ call.args.map( e => TypedOpenCLMemory(e.inM, e.inT) )

            case _ => arr :+ TypedOpenCLMemory(call.inM, call.inT) :+ TypedOpenCLMemory(call.outM, call.outT)

          }
        case p: Param => arr :+ TypedOpenCLMemory(p.inM, p.inT)
        case v: Value => arr
      })

    val seen = mutable.HashSet[OpenCLMemory]()
    // remove null memory and duplicates while preserving the original order
    result.foldLeft(Array[TypedOpenCLMemory]())( (arr, mem) => {
      val m = mem.mem
      if (seen.contains(m) || m == OpenCLNullMemory || m.isInstanceOf[OpenCLMemoryCollection]) {
        arr
      } else {
        seen += m
        arr :+ mem
      }
    })
  }
}
