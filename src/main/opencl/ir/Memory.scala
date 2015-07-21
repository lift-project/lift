package opencl.ir

import arithmetic._
import ir._
import ir.ast._
import opencl.ir.ast._

import scala.collection.mutable
import opencl.ir.pattern._

/** Represents OpenCL address spaces either: local or global; UndefAddressSpace should be used in case of errors */
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
object OpenCLNullMemory
  extends OpenCLMemory(Var("NULL"), Cst(-1), UndefAddressSpace)


object OpenCLMemory {

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

  def apply(variable: Var, size: ArithExpr,
            addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    new OpenCLMemory(variable, size, addressSpace)
  }

  def getMaxSizeInBytes(t: Type): ArithExpr = {
    ArithExpr.max(getSizeInBytes(t))
  }

  private def getSizeInBytes(t: Type): ArithExpr = {
    ExprSimplifier(
      t match {
        case st: ScalarType => st.size
        case vt: VectorType => vt.len * getSizeInBytes(vt.scalarT)
        case at: ArrayType => at.len * getSizeInBytes(at.elemT)
//        case mt: MatrixType => mt.dx * mt.dy * getSizeInBytes(mt.elemT)
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
                  pvtOutSize: ArithExpr, addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    assert(addressSpace != UndefAddressSpace)

    addressSpace match {
      case GlobalMemory => allocGlobalMemory(glbOutSize)
      case LocalMemory => allocLocalMemory(lclOutSize)
      case PrivateMemory => allocPrivateMemory(pvtOutSize)
    }
  }

  /** Return newly allocated global memory */
  def allocGlobalMemory(glbOutSize: ArithExpr): OpenCLMemory = {
    OpenCLMemory(Var(ContinuousRange(Cst(0), glbOutSize)), glbOutSize, GlobalMemory)
  }

  /** Return newly allocated local memory */
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
  def alloc(expr: Expr, numGlb: ArithExpr = 1, numLcl: ArithExpr = 1, numPvt: ArithExpr = 1,
            outputMem: OpenCLMemory = OpenCLNullMemory): OpenCLMemory = {

    val result = expr match {
      case v: Value => allocValue(v)
      case p: Param => allocParam(p)
      case call: FunCall => allocFunCall(call, numGlb, numLcl, numPvt, outputMem)
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

  private def allocFunCall(call: FunCall, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr,
                           outputMem: OpenCLMemory): OpenCLMemory = {
    // get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call, numGlb, numLcl, numPvt, outputMem)

    val maxSizeInBytes = getMaxSizeInBytes(call.t)
    // size in bytes necessary to hold the result of f in global and local memory
    val maxGlbOutSize = maxSizeInBytes * numGlb
    val maxLclOutSize = maxSizeInBytes * numLcl
    val maxPvtOutSize = maxSizeInBytes * numPvt

    // maximum length of the output array
    val maxLen = ArithExpr.max(Type.getLength(call.t))

    // determine the output memory based on the type of f ...
    call.f match {

      case l: Lambda =>           allocLambda(l, call, numGlb, numLcl, numPvt, inMem, outputMem)

      case MapGlb(_, _) |MapWrg(_,_) | Map(_)
                               => allocMapGlb(call.f.asInstanceOf[AbstractMap], numGlb, numLcl, numPvt, inMem, outputMem, maxLen)
      case MapLcl(_,_) | MapWarp(_)| MapLane(_) | MapSeq(_)
                               =>
        var privateMultiplier = call.f.asInstanceOf[AbstractMap].iterationCount
        privateMultiplier = if (privateMultiplier == ?) 1 else privateMultiplier
        allocMapLcl(call.f.asInstanceOf[AbstractMap], numGlb, numLcl, numPvt * privateMultiplier, inMem, outputMem, maxLen)

      case r: AbstractPartRed =>  allocReduce(r, numGlb, numLcl, numPvt, inMem, outputMem)

      case cf: CompFun =>      allocCompFunDef(cf, numGlb, numLcl, numPvt, inMem, outputMem)

      case Zip(_) | Tuple(_) =>   allocZipTuple(inMem, outputMem)
      case f: Filter =>           allocFilter(f, numGlb, numLcl, inMem)

      case tg: toGlobal =>        allocToGlobal(tg, numGlb, numLcl, numPvt, inMem, outputMem, maxGlbOutSize)
      case tl: toLocal =>         allocToLocal(tl, numGlb, numLcl, numPvt, inMem, outputMem, maxLclOutSize)
      case tp: toPrivate =>       allocToPrivate(tp, numGlb, numLcl, numPvt, inMem, outputMem, maxPvtOutSize)

      case it: Iterate =>         allocIterate(it, call, numGlb, numLcl, numPvt, inMem)

      case Split(_) | Join() | asVector(_) | asScalar() |
           Transpose() | Unzip() | TransposeW() | Barrier() | Group(_,_,_) |
           Head() | Tail() | Gather(_) | Scatter(_) =>
        inMem
      case uf: UserFun =>
        allocUserFun(maxGlbOutSize, maxLclOutSize, maxPvtOutSize, outputMem, call.t, inMem)

    }
  }

  private def getInMFromArgs(call: FunCall, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr, outputMem: OpenCLMemory): OpenCLMemory = {
    if (call.args.isEmpty) {
      OpenCLNullMemory
    } else if (call.args.length == 1) {
      alloc(call.args(0), numGlb, numLcl, numPvt, outputMem)
    } else {

      val mems = if (outputMem != OpenCLNullMemory)
        call.args.map(arg => {
          val maxSize = getMaxSizeInBytes(arg.t)
          alloc(arg, numGlb, numLcl, numPvt, allocMemory(numGlb * maxSize, numLcl * maxSize, numPvt * maxSize, outputMem.addressSpace))
        })
      else
        call.args.map(alloc(_, numGlb, numLcl, numPvt))

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

  private def allocLambda(l: Lambda, call: FunCall, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr,
                          inMem: OpenCLMemory, outputMem: OpenCLMemory): OpenCLMemory = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).mem = inMem
    } else {
      val coll = inMem match { case coll: OpenCLMemoryCollection => coll}
      if (l.params.length != coll.subMemories.length) throw new NumberOfArgumentsException

      (l.params zip coll.subMemories).foreach({case (p, m) => p.mem = m})
    }
    alloc(l.body, numGlb, numLcl, numPvt, outputMem)
  }

  private def allocMapGlb(am: AbstractMap, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr,
                          inMem: OpenCLMemory, outputMem: OpenCLMemory, maxLen: ArithExpr): OpenCLMemory = {
    if (am.f.params.length != 1) throw new NumberOfArgumentsException
    am.f.params(0).mem = inMem
    alloc(am.f.body, numGlb * maxLen, numLcl, numPvt, outputMem)
  }

  private def allocMapLcl(am: AbstractMap, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr,
                          inMem: OpenCLMemory, outputMem: OpenCLMemory, maxLen: ArithExpr): OpenCLMemory = {
    if (am.f.params.length != 1) throw new NumberOfArgumentsException
    am.f.params(0).mem = inMem
    alloc(am.f.body, numGlb * maxLen, numLcl * maxLen, numPvt , outputMem)
  }

  private def allocReduce(r: AbstractPartRed, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr,
                          inMem: OpenCLMemory, outputMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        if (coll.subMemories.length != 2) throw new NumberOfArgumentsException
        val initM = coll.subMemories(0)
        val elemM = coll.subMemories(1)
        if (r.f.params.length != 2) throw new NumberOfArgumentsException
        r.f.params(0).mem = initM
        r.f.params(1).mem = elemM
        alloc(r.f.body, numGlb, numLcl, numPvt, initM)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocToGlobal(tg: toGlobal, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr,
                            inMem: OpenCLMemory, outputMem: OpenCLMemory, maxGlbOutSize: ArithExpr): OpenCLMemory = {
    if (tg.f.params.length != 1) throw new NumberOfArgumentsException
    tg.f.params(0).mem = inMem

    val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != GlobalMemory) {
      allocGlobalMemory(maxGlbOutSize)
    } else {
      outputMem
    }
    // ... recurse with the freshly allocated 'mem' set as output
    alloc(tg.f.body, numGlb, numLcl, numPvt, mem)
  }

  private def allocToLocal(tl: toLocal, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr,
                           inMem: OpenCLMemory, outputMem: OpenCLMemory, maxLclOutSize: ArithExpr): OpenCLMemory = {
    if (tl.f.params.length != 1) throw new NumberOfArgumentsException
    tl.f.params(0).mem = inMem

    val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != LocalMemory) {
      allocLocalMemory(maxLclOutSize)
    } else {
      outputMem
    }
    // ... recurse with the  freshly allocated 'mem' set as output
    alloc(tl.f.body, numGlb, numLcl, numPvt, mem)
  }

  private def allocToPrivate(tp: toPrivate, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr,
                           inMem: OpenCLMemory, outputMem: OpenCLMemory, maxPvtOutSize: ArithExpr): OpenCLMemory = {
    if (tp.f.params.length != 1) throw new NumberOfArgumentsException
    tp.f.params(0).mem = inMem

    val mem = if (outputMem == OpenCLNullMemory || outputMem.addressSpace != PrivateMemory) {
      allocPrivateMemory(maxPvtOutSize)
    } else {
      outputMem
    }
    // ... recurse with the freshly allocated 'mem' set as output
    alloc(tp.f.body, numGlb, numLcl, numPvt, mem)
  }

  private def allocCompFunDef(cf: CompFun, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr,
                              inMem: OpenCLMemory, outMem: OpenCLMemory): OpenCLMemory = {
    // combine the parameter of the first function to call with the type inferred from the argument

    val lastConcrete = cf.funs.find(_.body.isConcrete) match {
      case Some(c) => c
      case None => None
    }

    cf.funs.foldRight(inMem)((f, mem) => {
      if (f.params.length != 1) throw new NumberOfArgumentsException
      f.params(0).mem = mem
      if (f == lastConcrete) {
        alloc(f.body, numGlb, numLcl, numPvt, outMem)
      } else
        alloc(f.body, numGlb, numLcl, numPvt)
    })
  }

  private def allocZipTuple(inMem: OpenCLMemory, outMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        if (coll.subMemories.length < 2) throw new NumberOfArgumentsException
        coll
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocIterate(it: Iterate, call: FunCall, numGlb: ArithExpr, numLcl: ArithExpr, numPvt: ArithExpr,
                           inMem: OpenCLMemory): OpenCLMemory = {
    // get sizes in bytes necessary to hold the input and output of the function inside the iterate
    val inSize = getMaxSizeInBytes(call.argsType)
    val outSize = getMaxSizeInBytes(call.t)
    // get the max from those two
    val largestSize = ArithExpr.max(inSize, outSize)

    // create a swap buffer
    it.swapBuffer = allocMemory(largestSize, largestSize, largestSize, inMem.addressSpace)

    // recurs to allocate memory for the function(s) inside
    if (it.f.params.length != 1) throw new NumberOfArgumentsException
    it.f.params(0).mem = inMem
    alloc(it.f.body, numGlb, numLcl, numPvt)
  }

  private def allocFilter(f: Filter, numGlb: ArithExpr, numLcl: ArithExpr, inMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        if (coll.subMemories.length != 2) throw new NumberOfArgumentsException
        coll.subMemories(0)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocUserFun(maxGlbOutSize: ArithExpr, maxLclOutSize: ArithExpr, maxPvtOutSize: ArithExpr,
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

      allocMemory(maxGlbOutSize, maxLclOutSize, maxPvtOutSize, addressSpace)
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
  def getAllocatedMemory(expr: Expr, params: Array[Param], includePrivate: Boolean = false): Array[TypedOpenCLMemory] = {

    // recursively visit all functions and collect input and output (and swap buffer for the iterate)
    val result = Expr.visitWithState(Array[TypedOpenCLMemory]())(expr, (exp, arr) =>
      exp match {
        case call: FunCall =>
          call.f match {
            case it: Iterate =>
              arr :+
              TypedOpenCLMemory(call.args(0).mem, call.args(0).t) :+
              TypedOpenCLMemory(call.mem, call.t) :+
              TypedOpenCLMemory(it.swapBuffer, ArrayType(call.args(0).t, ?))

            case r: AbstractPartRed =>
              // exclude the iniT (TODO: only if it is already allocated ...)

              val inMem = TypedOpenCLMemory( call.args(1).mem, call.args(1).t )

              arr :+ inMem

            case z: Zip => arr ++ call.args.map( e => TypedOpenCLMemory(e.mem, e.t) )

            case f: Filter => arr ++ call.args.map( e => TypedOpenCLMemory(e.mem, e.t))

            case _ => arr :+ TypedOpenCLMemory(call.argsMemory, call.argsType) :+ TypedOpenCLMemory(call.mem, call.t)
          }
        case v: Value => arr
        case p: Param => arr :+ TypedOpenCLMemory(p.mem, p.t)
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
              && params.find(p => p.mem == m) == None)
              && !includePrivate) {
        arr
      } else {
        seen += m
        arr :+ mem
      }
    })
  }
}
