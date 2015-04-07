package opencl.ir

import arithmetic._

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

// TODO: This currently is used only for scalar values!!!
object PrivateMemory extends OpenCLAddressSpace {
  override def toString = "private"
}

object UndefAddressSpace extends OpenCLAddressSpace

/** Represents memory in OpenCL as a raw collection of bytes allocated in an OpenCL address space.
  *
  * @constructor Create a new OpenCLMemory object
  * @param variable The variable associated with the memory
  * @param size The size of the memory as numbers bytes
  * @param addressSpace The address space where the memory has been allocated
  */
class OpenCLMemory(var variable: Var, val size: ArithExpr, val addressSpace: OpenCLAddressSpace) extends Memory {

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
  override def toString: String = {
    this match {
      case coll: OpenCLMemoryCollection =>
        "coll(" + coll.subMemories.map(_.toString).reduce(_ + ", " + _) + " | " + coll.addressSpace + ")"
      case _ =>
        "size: " + size + "; addressspace: " + addressSpace
    }
  }
}

class OpenCLSubMemory(parent: OpenCLMemory, size: ArithExpr, accessFun: (ArithExpr) => ArithExpr)
  extends OpenCLMemory(parent.variable, size, parent.addressSpace)

class OpenCLMemoryCollection(val subMemories: Array[OpenCLMemory], override val addressSpace: OpenCLAddressSpace)
  extends OpenCLMemory(Var("Tuple"), subMemories.map(_.size).reduce(_+_), addressSpace) {

  def findCommonAddressSpace(): OpenCLAddressSpace = {
    // try to find common address space which is not the private memory ...
    val noPrivateMem = this.subMemories.filterNot(_.addressSpace == PrivateMemory)
    assert(noPrivateMem.nonEmpty)

    val addessSpaces = noPrivateMem.map({
      case coll: OpenCLMemoryCollection => coll.findCommonAddressSpace()
      case m: OpenCLMemory => m.addressSpace
    })

    if (addessSpaces.forall(_ == addessSpaces(0))) {
      addessSpaces(0)
    } else {
      throw new IllegalArgumentException("Could not determine common addressSpace")
    }
  }
}

object OpenCLMemoryCollection {
  def apply(addressSpace : OpenCLAddressSpace, subMemories: OpenCLMemory*) =
    new OpenCLMemoryCollection(Array(subMemories:_*), addressSpace)
}

// Allocate memory globally
class GlobalAllocator {

}
/** Represents the NULL OpenCL memory object */
object OpenCLNullMemory extends OpenCLMemory(Var("NULL"), Cst(-1), UndefAddressSpace)


object OpenCLMemory {

  def apply(variable: Var, size: ArithExpr, addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    new OpenCLMemory(variable, size, addressSpace)
  }

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
    OpenCLMemory(Var(ContinuousRange(Cst(0), glbOutSize)), glbOutSize, GlobalMemory)
  }

  //** Return newly allocated local memory */
  def allocLocalMemory(lclOutSize: ArithExpr): OpenCLMemory = {
    OpenCLMemory(Var(ContinuousRange(Cst(0), lclOutSize)), lclOutSize, LocalMemory)
  }

  def allocPrivateMemory(size: ArithExpr): OpenCLMemory = {
    OpenCLMemory(Var(ContinuousRange(Cst(0), size)), size, PrivateMemory)
  }

  /** Allocate OpenCLMemory objects for a given Fun f
    *
    * @param expr The expression for which memory should be allocated
    * @param numGlb Number of ...
    * @param numLcl Number of ..
    * @param outputMem The OpenCLMemory object to be used as output by f. Can be NULL then memory will be allocated.
    * @return The OpenCLMemory used as output by f
    */
  def alloc(expr: Expr, numGlb: ArithExpr = 1, numLcl: ArithExpr = 1,
            outputMem: OpenCLMemory = OpenCLNullMemory): OpenCLMemory = {

    val result = expr match {
      case v: Value => allocValue(v)
      case p: Param => allocParam(p)
      case call: FunCall => allocFunCall(call, numGlb, numLcl, outputMem)
    }
    // set the output
    assert(result != OpenCLNullMemory)
    expr.mem = result

    // finally return the output
    result
  }

  private def allocValue(v: Value): OpenCLMemory = {
    if (v.mem != UnallocatedMemory) {
      OpenCLMemory.asOpenCLMemory(v.mem)
    } else {
      allocPrivateMemory(getSizeInBytes(v.t))
    }
  }

  private def getMemAtIndex(mem: Memory, index: Int): OpenCLMemory = {
    mem match {
      case coll: OpenCLMemoryCollection =>
        assert(index < coll.subMemories.length)
        coll.subMemories(index)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocParam(param: Param): OpenCLMemory = {
    val res = param match {
      case pr: ParamReference =>
        if (pr.p.mem == UnallocatedMemory) throw new IllegalArgumentException("PANIC!")
        getMemAtIndex(pr.p.mem, pr.i)
      case vp: VectorParam =>
        if (vp.p.mem == UnallocatedMemory) throw new IllegalArgumentException("PANIC!")
        vp.p.mem
      case p: Param =>
        if (p.mem == UnallocatedMemory) throw new IllegalArgumentException("PANIC!")
        p.mem
    }

    OpenCLMemory.asOpenCLMemory(res)
  }

  private def allocFunCall(call: FunCall, numGlb: ArithExpr, numLcl: ArithExpr,
                           outputMem: OpenCLMemory): OpenCLMemory = {
    // get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call, numGlb, numLcl)

    // size in bytes necessary to hold the result of f in global and local memory
    val maxGlbOutSize = getMaxSizeInBytes(call.t) * numGlb
    val maxLclOutSize = getMaxSizeInBytes(call.t) * numLcl

    // maximum length of the output array
    val maxLen = ArithExpr.max(Type.getLength(call.t))

    // determine the output memory based on the type of f ...
    call.f match {

      case l: Lambda =>           allocLambda(l, call, numGlb, numLcl, inMem, outputMem)

      case MapGlb(_, _) |MapWrg(_,_) | Map(_)
                               => allocMapGlb(call.f.asInstanceOf[AbstractMap], numGlb, numLcl, inMem, outputMem, maxLen)
      case MapLcl(_,_) | MapWarp(_)| MapLane(_) | MapSeq(_)
                               => allocMapLcl(call.f.asInstanceOf[AbstractMap], numGlb, numLcl, inMem, outputMem, maxLen)

      case r: AbstractPartRed =>  allocReduce(r, numGlb, numLcl, inMem, outputMem)

      case cf: CompFunDef =>      allocCompFunDef(cf, numGlb, numLcl, inMem)

      case Zip(_) | Tuple(_) =>   allocZipTuple(inMem)
      case f: Filter =>           allocFilter(f, numGlb, numLcl, inMem)

      case tg: toGlobal =>        allocToGlobal(tg,   numGlb, numLcl, inMem, outputMem, maxGlbOutSize)
      case tl: toLocal =>         allocToLocal(tl,    numGlb, numLcl, inMem, outputMem, maxLclOutSize)

      case g: Gather =>           allocGather(g, numGlb, numLcl, inMem, outputMem)
      case s: Scatter =>          allocScatter(s, numGlb, numLcl, inMem, outputMem)

      case it: Iterate =>         allocIterate(it, call.asInstanceOf[IterateCall], numGlb, numLcl, inMem)

      case Split(_) | Join() | ReorderStride(_) | asVector(_) |
           asScalar() | Transpose() | Unzip() | TransposeW() | Barrier() =>
        inMem
      case uf: UserFunDef =>
        allocUserFun(maxGlbOutSize, maxLclOutSize, outputMem, call.t, inMem)

    }
  }

  private def getInMFromArgs(call: FunCall, numGlb: ArithExpr, numLcl: ArithExpr): OpenCLMemory = {
    if (call.args.isEmpty) {
      OpenCLNullMemory
    } else if (call.args.length == 1) {
      alloc(call.args(0), numGlb, numLcl)
    } else {
      val mems = call.args.map(alloc(_, numGlb, numLcl))

      call.f match {
        // TODO: not sure if this is necessary!!
        case r: AbstractPartRed =>
          if (mems.length != 2) throw new NumberOfArgumentsException
          OpenCLMemoryCollection(mems(1).addressSpace, mems: _*)

        case _ =>
          OpenCLMemoryCollection(UndefAddressSpace, mems: _*)
      }
    }
  }

  private def allocLambda(l: Lambda, call: FunCall, numGlb: ArithExpr, numLcl: ArithExpr, inMem: OpenCLMemory, outputMem: OpenCLMemory): OpenCLMemory = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).mem = inMem
    } else {
      val coll = inMem match { case coll: OpenCLMemoryCollection => coll}
      if (l.params.length != coll.subMemories.length) throw new NumberOfArgumentsException

      (l.params zip coll.subMemories).foreach({case (p, m) => p.mem = m})
    }
    alloc(l.body, numGlb, numLcl, outputMem)
  }

  private def allocMapGlb(am: AbstractMap, numGlb: ArithExpr, numLcl: ArithExpr,
                          inMem: OpenCLMemory, outputMem: OpenCLMemory, maxLen: ArithExpr): OpenCLMemory = {
    if (am.f.params.length != 1) throw new NumberOfArgumentsException
    am.f.params(0).mem = inMem
    alloc(am.f.body, numGlb * maxLen, numLcl, outputMem)
  }


  private def allocMapLcl(am: AbstractMap, numGlb: ArithExpr, numLcl: ArithExpr,
                          inMem: OpenCLMemory, outputMem: OpenCLMemory, maxLen: ArithExpr): OpenCLMemory = {
    if (am.f.params.length != 1) throw new NumberOfArgumentsException
    am.f.params(0).mem = inMem
    alloc(am.f.body, numGlb * maxLen, numLcl * maxLen, outputMem)
  }

  private def allocReduce(r: AbstractPartRed, numGlb: ArithExpr, numLcl: ArithExpr, inMem: OpenCLMemory, outputMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        if (coll.subMemories.length != 2) throw new NumberOfArgumentsException
        val initM = coll.subMemories(0)
        val elemM = coll.subMemories(1)
        if (r.f.params.length != 2) throw new NumberOfArgumentsException
        r.f.params(0).mem = initM
        r.f.params(1).mem = elemM
        alloc(r.f.body, numGlb, numLcl, outputMem)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocGather(g: Gather, numGlb: ArithExpr, numLcl: ArithExpr, inMem: OpenCLMemory, outputMem: OpenCLMemory): OpenCLMemory = {
    if (g.f.params.length != 1) throw new NumberOfArgumentsException
    g.f.params(0).mem = inMem
    alloc(g.f.body, numGlb, numLcl, outputMem)
  }

  private def allocScatter(s: Scatter, numGlb: ArithExpr, numLcl: ArithExpr, inMem: OpenCLMemory, outputMem: OpenCLMemory): OpenCLMemory = {
    if (s.f.params.length != 1) throw new NumberOfArgumentsException
    s.f.params(0).mem = inMem
    alloc(s.f.body, numGlb, numLcl, outputMem)
  }

  private def allocToGlobal(tg: toGlobal, numGlb: ArithExpr, numLcl: ArithExpr,
                            inMem: OpenCLMemory, outputMem: OpenCLMemory, maxGlbOutSize: ArithExpr): OpenCLMemory = {
    if (tg.f.params.length != 1) throw new NumberOfArgumentsException
    tg.f.params(0).mem = inMem

    val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != GlobalMemory) {
      allocGlobalMemory(maxGlbOutSize)
    } else {
      outputMem
    }
    // ... recurs with fresh allocated 'mem' set as output
    alloc(tg.f.body, numGlb, numLcl, mem)
  }

  private def allocToLocal(tl: toLocal, numGlb: ArithExpr, numLcl: ArithExpr,
                           inMem: OpenCLMemory, outputMem: OpenCLMemory, maxLclOutSize: ArithExpr): OpenCLMemory = {
    if (tl.f.params.length != 1) throw new NumberOfArgumentsException
    tl.f.params(0).mem = inMem

    val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != LocalMemory) {
      allocLocalMemory(maxLclOutSize)
    } else {
      outputMem
    }
    // ... recurs with fresh allocated 'mem' set as output
    alloc(tl.f.body, numGlb, numLcl, mem)
  }

  private def allocCompFunDef(cf: CompFunDef, numGlb: ArithExpr, numLcl: ArithExpr,
                              inMem: OpenCLMemory): OpenCLMemory = {
    // combine the parameter of the first function to call with the type inferred from the argument

    cf.funs.foldRight(inMem)((f, mem) => {
      if (f.params.length != 1) throw new NumberOfArgumentsException
      f.params(0).mem = mem
      alloc(f.body, numGlb, numLcl)
    })
  }

  private def allocZipTuple(inMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        if (coll.subMemories.length < 2) throw new NumberOfArgumentsException
        coll
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocFilter(f: Filter, numGlb: ArithExpr, numLcl: ArithExpr, inMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        if (coll.subMemories.length != 2) throw new NumberOfArgumentsException
        coll.subMemories(0)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocIterate(it: Iterate, call: IterateCall, numGlb: ArithExpr, numLcl: ArithExpr,
                           inMem: OpenCLMemory): OpenCLMemory = {
    // get sizes in bytes necessary to hold the input and output of the function inside the iterate
    val inSize = getMaxSizeInBytes(call.argsType)
    val outSize = getMaxSizeInBytes(call.t)
    // get the max from those two
    val largestSize = ArithExpr.max(inSize, outSize)

    // create a swap buffer
    call.swapBuffer = allocMemory(largestSize, largestSize, inMem.addressSpace)

    // recurs to allocate memory for the function(s) inside
    if (it.f.params.length != 1) throw new NumberOfArgumentsException
    it.f.params(0).mem = inMem
    alloc(it.f.body, numGlb, numLcl)
  }

  private def allocUserFun(maxGlbOutSize: ArithExpr, maxLclOutSize: ArithExpr,
                           outputMem: OpenCLMemory, outT: Type, inputMem: OpenCLMemory): OpenCLMemory = {
    if (outputMem == OpenCLNullMemory) {
      val addressSpace =
        if (!inputMem.isInstanceOf[OpenCLMemoryCollection]) {
          inputMem.addressSpace
        } else {
          val coll = inputMem match {
            case coll: OpenCLMemoryCollection => coll
          }
          coll.findCommonAddressSpace()
        }

      outT match {

        // TODO: could maybe allocated in private memory (need to change slightly the allocator and add toPrivate)
        case ScalarType(_, _) | VectorType(_, _) =>
          allocMemory(maxGlbOutSize, maxLclOutSize, addressSpace)

        case _ =>
          allocMemory(maxGlbOutSize, maxLclOutSize, addressSpace)
      }
    } else {
      outputMem
    }
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
  def getAllocatedMemory(expr: Expr, params: Array[Param]): Array[TypedOpenCLMemory] = {

    // recursively visit all functions and collect input and output (and swap buffer for the iterate)
    val result = Expr.visit(Array[TypedOpenCLMemory]())(expr, (exp, arr) =>
      exp match {
        case call: FunCall =>
          call.f match {
            case it: Iterate =>
              val fIter = call.asInstanceOf[IterateCall]
              arr :+
              TypedOpenCLMemory(fIter.arg.mem, fIter.arg.t) :+
              TypedOpenCLMemory(fIter.mem, fIter.t) :+
              TypedOpenCLMemory(fIter.swapBuffer, ArrayType(fIter.arg.t, ?))

            case r: AbstractPartRed =>
              // exclude the iniT (TODO: only if it is already allocated ...)

              val inMem = TypedOpenCLMemory( call.args(1).mem, call.args(1).t )

              arr :+ inMem :+ TypedOpenCLMemory(call.mem, call.t)

            case z: Zip => arr ++ call.args.map( e => TypedOpenCLMemory(e.mem, e.t) )

            case f: Filter => arr ++ call.args.map( e => TypedOpenCLMemory(e.mem, e.t))

            case _ => arr :+ TypedOpenCLMemory(call.argsMemory, call.argsType) :+ TypedOpenCLMemory(call.mem, call.t)
          }
        case p: Param => arr :+ TypedOpenCLMemory(p.mem, p.t)
        case v: Value => arr
      })

    val resultWithoutCollections = result.map(tm => tm.mem match {
      case coll: OpenCLMemoryCollection =>
        coll.subMemories.zipWithIndex.map({ case (m, i) => TypedOpenCLMemory(m, Type.getTypeAtIndex(tm.t, i))})
      case ocl: OpenCLMemory => Array(tm)
    }).flatten

    val seen = mutable.HashSet[OpenCLMemory]()
    // remove null memory and duplicates while preserving the original order
    resultWithoutCollections.foldLeft(Array[TypedOpenCLMemory]())( (arr, mem) => {
      val m = mem.mem
      if (   seen.contains(m)
          || m == OpenCLNullMemory
          || m.isInstanceOf[OpenCLMemoryCollection]

          // m is in private memory but not an parameter => trow away
          || (   m.addressSpace == PrivateMemory
              && params.find(p => p.mem == m) == None)) {
        arr
      } else {
        seen += m
        arr :+ mem
      }
    })
  }
}
