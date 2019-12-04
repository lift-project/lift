package opencl.ir

/**
 * About memory allocation
 * The memory allocation process is decomposed in two steps:
 *
 * 1. The `OpenCLMemoryAllocator` class computes an arithmetic expression
 *    representing the amount of space (in bytes) we need two allocate for each
 *    input, output and temporary result depending on its **type**.
 * 2. The `Executor` tries to infer a value for each variable appearing in
 *    these sizes given the value of the inputs of a kernel, replace all theses
 *    variables by their actual value and actually allocate OpenCL buffers.
 *    (For example if an input array has type `ArrayType(Int, N)` and the
 *    actual input given to the executor is an array of size 128, the
 *    `Executor` will infer that N = 128.)
 *
 *
 * This class implements the first point:
 *
 * The arithmetic expression computed for allocating memory only depends on the
 * type of the lift expression and is derived from the capacity of the array to
 * be allocated. We can only compute a meaningfull arithmetic expression
 * **when the capacity is part of the array's type**. If the type of the
 * expression for allocation contains an array for which the capacity is not
 * part of the type, we cannot produce an expression for its size and the
 * OpenCLMemoryAllocator returns the `?` special value.
 *
 * At the moment, arrays are the only structure for which we may not know how
 * much space to allocated judging only from the type. Therefore, the only
 * scenario in which the memory allocator returns `?` is when there is an
 * `ArrayType` without all capacity information available in the type.
 *
 * In general, the special `?` value means: "Dynamic memory allocation is
 * required". But:
 *
 * 1. OpenCL does not support dynamic allocation. We want to emulate this in
 *    the future but this is **not implemented** yet.
 * 2. There is a special case where we still can derive the allocation size in
 *    the executor: In a case, where an input of the kernel has not been
 *    allocated memory by the OpenCLMemoryAllocator, the `Executor` will look
 *    at both the type **and** the actual value of the input to infer the
 *    allocation size. Since the `Executor` can investigate the inputs of the
 *    kernel it can use the lengths of the arrays for which the capacity is not
 *    part of the type as their capacity and use them where the memory
 *    allocator was stuck. This special memory allocation is performed by the
 *    `allocArgumentWithoutFixedAllocatedSize` method in the executor.
 */

import arithmetic.TypeVar
import ir.Type.size_t
import ir._
import ir.ast._
import lift.arithmetic.{?, ArithExpr, Cst}
import opencl.ir.pattern._
import opencl.ir.OpenCLAddressSpace.asOpenCLAddressSpace

import scala.collection.mutable

object RemoveRedundantMemory {
  def apply(f: Lambda): Unit = {
    // 1. collect all redundant memory
    var replacementMap = mutable.Map[Memory, Memory]()
    Expr.visit(f.body, {
      case call: FunCall => call.f match {
        case c: Concat => replacementMap ++= c.replacementMap
        case sw: SkipW => replacementMap ++= sw.replacementMap
        case _ =>
      }
      case _ =>
    }, (_: Expr) => _)

    // 2. apply all replacements
    Expr.visit(f.body, e =>
      if (replacementMap.isDefinedAt(e.mem)) e.mem = replacementMap(e.mem)
    , (_: Expr) => _)
  }
}

object OpenCLMemoryAllocator {
  /** (baseSize, innerSize) => totalSize */
  type Allocator = (ArithExpr, ArithExpr) => ArithExpr

  /**
    * Allocate memory for both the body and the parameters of a lambda
    * expression
    *
    * @param f the lambda expression
    * @return the OpenCLMemory used as output by f
    */
  def apply(f: Lambda): OpenCLMemory = {
    f.params.foreach((p) =>
      p.mem = OpenCLMemory.allocMemory(Type.getAllocatedSize(p.t), p.addressSpace)
    )

    alloc(f.body)
  }

  /**
   * Allocate OpenCLMemory objects for a given expression
   *
   * @param expr   The expression for which memory should be allocated
   * @param numGlb Function computing the number of bytes to allocate in global
   *               memory given the sizes of the elements and of the base
   *               elements in case this is an array.
   * @param numLcl Idem in local memory
   * @param numPvt Idem in private memory
   * @return The OpenCLMemory used by expr
   */
  def alloc(expr: Expr,
    numGlb: Allocator = (_, x) => x,
    numLcl: Allocator = (_, x) => x,
    numPvt: Allocator = (_, x) => x): OpenCLMemory = {

    val result = expr match {
      case ArrayFromExpr(e) => alloc(e, numGlb, numLcl, numPvt)
      case _: ArrayConstructors => OpenCLNullMemory // an array constructor is not backed by memory

      case v: Value => allocValue(v)
      case vp: VectorParam => allocParam(vp.p)
      case p: Param => allocParam(p)
      case call: FunCall =>
        allocFunCall(call, numGlb, numLcl, numPvt, UndefAddressSpace)
    }
    // set the output
    expr.mem = result

    // finally return the output
    result
  }

  private def allocValue(v: Value): OpenCLMemory = {
    if (v.mem != UnallocatedMemory) {
      val oclMem = OpenCLMemory.asOpenCLMemory(v.mem)
      assert(oclMem.addressSpace == PrivateMemory)
      oclMem
    } else {
      OpenCLMemory.allocPrivateMemory(Type.getAllocatedSize(v.t))
    }
  }

  private def allocParam(p: Param): OpenCLMemory = {
    if (p.mem == UnallocatedMemory)
      throw new IllegalArgumentException(s"Param $p has UnallocatedMemory")

    OpenCLMemory.asOpenCLMemory(p.mem)
  }

  private def allocFunCall(call: FunCall,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator,
    addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    // Get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call, numGlb, numLcl, numPvt)

    // Determine the output memory based on the type of f ...
    call.f match {
      // Here is where the actual allocation happens
      case _: UserFun | _: VectorizeUserFun =>
        allocUserFun(call.t, numGlb, numLcl, numPvt, call)

        // TODO: do not generate memory for Map(_) (add a safe fix).
        //       Map(_) does not need memory allocation since no code can be generated
      case MapGlb(_, _) |
           MapWrg(_, _) |
           Map(_) => allocMapGlb(call.f.asInstanceOf[AbstractMap],
        call.t, numGlb, numLcl, numPvt, inMem)

      case MapAtomWrg(_, _, _) => allocMapAtomWrg(call.f.asInstanceOf[AbstractMap],
        call.t, numGlb, numLcl, numPvt, inMem)

      case MapLcl(_, _) | MapAtomLcl(_, _, _) |
           MapWarp(_) | MapLane(_) |
           MapSeq(_) =>
        allocMapSeqLcl(call.f.asInstanceOf[AbstractMap],
          call.t, numGlb, numLcl, numPvt, inMem)

      case iss: InsertionSortSeq => allocInsertionSort(iss, call, numGlb, numLcl, numPvt, inMem)

      case fs: FilterSeq => allocFilterSeq(fs, call, numGlb, numLcl, numPvt, inMem)

      case r: AbstractPartRed => allocReduce(r, numGlb, numLcl, numPvt, inMem)

      case sp: MapSeqSlide => allocMapSeqSlide(sp,call.t, numGlb, numLcl, numPvt, inMem)

      case s: AbstractSearch => allocSearch(s, call, numGlb, numLcl, numPvt, inMem)

      case it: Iterate => allocIterate(it, call, numGlb, numLcl, numPvt, inMem)

      case cc: Concat => allocConcat(cc, call, numGlb, numLcl, numPvt, inMem)

      case scan: ScanSeq => allocScanSeq(scan, call, numGlb, numLcl, numPvt, inMem)

      case l: Lambda => allocLambda(l, numGlb, numLcl, numPvt, inMem)
      case toGlobal(f) => allocLambda(f, numGlb, numLcl, numPvt, inMem)
      case toLocal(f) => allocLambda(f, numGlb, numLcl, numPvt, inMem)
      case toPrivate(f) => allocLambda(f, numGlb, numLcl, numPvt, inMem)

      case Zip(_) | Tuple(_) => allocZipTuple(inMem)
      case Get(n) => allocGet(n, inMem)
      case f: Filter => allocFilter(f, numGlb, numLcl, inMem)
      case ua: UnsafeArrayAccess => allocUnsafeArrayAccess(ua, call, numGlb, numLcl, numPvt, inMem)
      case ca: CheckedArrayAccess => allocCheckedArrayAccess(ca, call, numGlb, numLcl, numPvt, inMem)

      case debug.PrintView(_, f) => allocLambda(f, numGlb, numLcl, numPvt, inMem)

      case RewritingGuidePost(_) => inMem

      case Split(_) | Join() | asVector(_) | asScalar() |
           Transpose() | Unzip() | TransposeW() | Slide(_, _) | Pad(_, _, _) | PadConstant(_, _, _) |
           Head() | Tail() | Gather(_) | Scatter(_) | ArrayAccess(_) |
           debug.PrintType(_) | debug.PrintTypeInConsole(_) | debug.PrintComment(_) | debug.AssertType(_, _) | Id() =>
        inMem
    }
  }

  private def getInMFromArgs(call: FunCall,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator): OpenCLMemory = {
    call.args.length match {
      case 0 =>
        throw new IllegalArgumentException(s"Function call without arguments $call")
      case 1 =>
        alloc(call.args.head, numGlb, numLcl, numPvt)
      case _ =>
        OpenCLMemoryCollection(call.args.map(alloc(_, numGlb, numLcl, numPvt)))
    }
  }

  private def allocUserFun(outT: Type,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator,
    call: FunCall): OpenCLMemory = {

    if (call.addressSpace == UndefAddressSpace)
      throw new RuntimeException("No address space at " + call)

    val maxSizeInBytes = Type.getAllocatedSize(outT)
    val baseSize = Type.getAllocatedSize(Type.getBaseType(outT))
    // size in bytes necessary to hold the result of f in the different
    // memory spaces
    val maxGlbOutSize = numGlb(baseSize, maxSizeInBytes)
    val maxLclOutSize = numLcl(baseSize, maxSizeInBytes)
    val maxPvtOutSize = numPvt(baseSize, maxSizeInBytes)

    OpenCLMemory.allocMemory(maxGlbOutSize, maxLclOutSize,
      maxPvtOutSize, call.addressSpace)
  }

  private def allocLambda(l: Lambda,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator,
    inMem: OpenCLMemory): OpenCLMemory = {
    setMemInParams(l.params, inMem)
    alloc(l.body, numGlb, numLcl, numPvt)
  }

  private def allocMapGlb(am: AbstractMap,
    outT: Type,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator,
    inMem: OpenCLMemory): OpenCLMemory = {
    am.f.params(0).mem = inMem

    alloc(am.f.body, sizeOfArray(numGlb, outT), numLcl, numPvt)
  }

  private def allocMapAtomWrg(am: AbstractMap,
    outT: Type,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator,
    inMem: OpenCLMemory): OpenCLMemory = {
    am.f.params(0).mem = inMem

    am.asInstanceOf[MapAtomWrg].globalTaskIndex =
      OpenCLMemory.allocGlobalMemory(Type.getMaxAllocatedSize(Int))

    alloc(am.f.body, sizeOfArray(numGlb, outT), numLcl, numPvt)
  }

  private def allocMapSeqLcl(am: AbstractMap,
    outT: Type,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator,
    inMem: OpenCLMemory): OpenCLMemory = {
    am.f.params(0).mem = inMem

    val privateMultiplier: ArithExpr =
      if (am.f.body.addressSpace.containsAddressSpace(PrivateMemory) ||
          inMem.addressSpace.containsAddressSpace(PrivateMemory))
        am.iterationCount
      else
        1

    alloc(am.f.body,
          sizeOfArray(numGlb, outT),
          sizeOfArray(numLcl, outT),
      (bs, inner) => numPvt(bs, privateMultiplier * inner))
  }

  private def allocInsertionSort(iss: InsertionSortSeq,
                                 call: FunCall,
                                 numGlb: Allocator,
                                 numLcl: Allocator,
                                 numPvt: Allocator,
                                 inMem: OpenCLMemory): OpenCLMemory = {

    val sizeInBytes = Type.getAllocatedSize(call.t)
    val outMem = OpenCLMemory.allocMemory(sizeInBytes, sizeInBytes, sizeInBytes, call.addressSpace)

    // Comparison function
    iss.f.params(1).mem = outMem
    iss.f.params(0).mem = inMem
    alloc(iss.f.body, numGlb, numLcl, numPvt)

    outMem
  }

  private def allocFilterSeq(fs: FilterSeq, call: FunCall,
                             numGlb: Allocator,
                             numLcl: Allocator,
                             numPvt: Allocator,
                             inMem: OpenCLMemory): OpenCLMemory = {
    fs.f.params.head.mem = inMem
    alloc(fs.f.body, numGlb, numLcl, numPvt)
    val sizeInBytes = Type.getAllocatedSize(call.t)
    OpenCLMemory.allocMemory(sizeInBytes, sizeInBytes, sizeInBytes, call.addressSpace)
  }

  private def allocReduce(r: AbstractPartRed,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator,
    inMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        val initM = coll.subMemories(0)
        r.f.params(0).mem = initM
        r.f.params(1).mem = coll.subMemories(1)
        val bodyM = alloc(r.f.body, numGlb, numLcl, numPvt)

        // if we have an instance of a reduce while, allocate the predicate
        r match {
          case rps: ReduceWhileSeq =>
            rps.p.params(0).mem = initM
            rps.p.params(1).mem = coll.subMemories(1)
            // set the predicate return memory to the returned memory
            rps.pmem = alloc(rps.p.body, numGlb, numLcl, numPvt)
          case _ =>
        }

        // replace `bodyM` by `initM` in `r.f.body`
        Expr.visit(r.f.body, e => if (e.mem == bodyM) e.mem = initM, _ => {})

        initM // return initM as the memory of the reduction pattern
      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def allocMapSeqSlide(sp: MapSeqSlide,
                                outT: Type,
                                numGlb: Allocator,
                                numLcl: Allocator,
                                numPvt: Allocator,
                                inMem: OpenCLMemory): OpenCLMemory = {

    sp.f.params(0).mem = OpenCLMemory(sp.windowVar, Type.getAllocatedSize(sp.f.params(0).t) * sp.size , PrivateMemory)

    val privateMultiplier: ArithExpr =
      if (sp.f.body.addressSpace.containsAddressSpace(PrivateMemory) ||
        inMem.addressSpace.containsAddressSpace(PrivateMemory))
        sp.iterationCount
      else
        1

    alloc(sp.f.body,
          sizeOfArray(numGlb, outT),
          sizeOfArray(numLcl, outT),
      (bs, inner) => numPvt(bs, privateMultiplier * inner))
  }


  private def allocSearch(s: AbstractSearch, call: FunCall,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator,
    inMem: OpenCLMemory): OpenCLMemory = {

    inMem match {
      case coll: OpenCLMemoryCollection =>
        // Set the comparison function input to be the elements
        // of the array we're searching
        s.f.params(0).mem = coll.subMemories(1)
        // Allocate memory for the comparison function
        alloc(s.f.body, numGlb, numLcl, numPvt)

        val size = Type.getAllocatedSize(call.t)
        // TODO: Do this the way the reduce does it - it makes a lot more sense!?
        // HOW IT'S ACTUALLY DONE: manually allocate that memory,
        // storing it in the correct address space
        OpenCLMemory.allocMemory(size, call.addressSpace)

      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def allocUnsafeArrayAccess(ua: UnsafeArrayAccess,
                                     call: FunCall,
                                     numGlb: Allocator,
                                     numLcl: Allocator,
                                     numPvt: Allocator,
                                     inMem: OpenCLMemory): OpenCLMemory = {
    // let the index allocate memory for itself (most likely a param, so it will know its memory)
    alloc(ua.index, numGlb, numLcl, numPvt)

    // allocate memory itself
    val outputSize = Type.getAllocatedSize(call.t)
    val baseSize = Type.getAllocatedSize(Type.getBaseType(call.t))
    // manually allocate that much memory, storing it in the correct address space
    if (call.addressSpace != UndefAddressSpace) {
     // use given address space
      OpenCLMemory.allocMemory(
        numGlb(baseSize, outputSize),
        numLcl(baseSize, outputSize),
        numPvt(baseSize, outputSize),
        call.addressSpace
      )
    } else {
      // address space is not predetermined
      //  => figure out the address space based on the input address space(s)
      val addrSpace =
        inMem match {
          case m: OpenCLMemory => m.addressSpace
          case _ => throw new IllegalArgumentException("PANIC")
        }
      OpenCLMemory.allocMemory(outputSize, outputSize, outputSize, addrSpace)
    }
  }

  private def allocCheckedArrayAccess(ca: CheckedArrayAccess,
                                      call: FunCall,
                                      numGlb: Allocator,
                                      numLcl: Allocator,
                                      numPvt: Allocator,
                                      inMem: OpenCLMemory): OpenCLMemory = {
    // let the index allocate memory for itself (most likely a param, so it will know its memory)
    alloc(ca.index, numGlb, numLcl, numPvt)

    // allocate memory itself
    val outputSize = Type.getAllocatedSize(call.t)
    val baseSize = Type.getAllocatedSize(Type.getBaseType(call.t))
    // manually allocate that much memory, storing it in the correct address space
    if (call.addressSpace != UndefAddressSpace) {
      // use given address space
      OpenCLMemory.allocMemory(
        numGlb(baseSize, outputSize),
        numLcl(baseSize, outputSize),
        numPvt(baseSize, outputSize),
        call.addressSpace
      )
    } else {
      // address space is not predetermined
      //  => figure out the address space based on the input address space(s)
      val addrSpace =
      inMem match {
        case m: OpenCLMemory => m.addressSpace
        case _ => throw new IllegalArgumentException("PANIC")
      }
      OpenCLMemory.allocMemory(outputSize, outputSize, outputSize, addrSpace)
    }
  }

  private def allocIterate(it: Iterate, call: FunCall,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator,
    inMem: OpenCLMemory): OpenCLMemory = {

    it.n match {
      case _ =>
        // Get sizes in bytes necessary to hold the input and output of the
        // function inside the iterate
        val inSize = Type.getAllocatedSize(call.argsType)
        val outSize = Type.getAllocatedSize(call.t)

        // Get the max from those two
        val largestSize = ArithExpr.max(inSize, outSize)
        val baseSize = Type.getAllocatedSize(Type.getBaseType(call.t))

        // Create a swap buffer
        it.swapBuffer = OpenCLMemory.allocMemory(
          numGlb(baseSize, largestSize),
          numLcl(baseSize, largestSize),
          numPvt(baseSize, largestSize),
          inMem.addressSpace
        )
    }

    // Recurse to allocate memory for the function(s) inside
    it.f.params(0).mem = inMem
    alloc(it.f.body, numGlb, numLcl, numPvt)
  }

  private def allocConcat(cc: Concat, call: FunCall,
                          numGlb: Allocator,
                          numLcl: Allocator,
                          numPvt: Allocator,
                          memOfArgs: OpenCLMemory): OpenCLMemory = {

    // dig into collection and match on arguments and replace already allocated memory
    memOfArgs match {
      case ocml: OpenCLMemoryCollection =>
        // check all address spaces are the same (private, local, global) -- skip for now
        val addressSpace = ocml.subMemories.head.addressSpace
        // get the size of each allocation and sum up to get overall allocation size
        val totalSize = ocml.subMemories.foldLeft[ArithExpr](Cst(0))((s,oo) => oo.size + s )
        // allocate new memory object with new size
        val outputMemory = OpenCLMemory.allocMemory(totalSize,addressSpace)
        // then remember to replace in arguments old allocated objects with new ones
        // this is done in RemoveRedundantMemory
        cc.replacementMap =
          ocml.subMemories.map(toBeReplaced => toBeReplaced -> outputMemory).toMap
        outputMemory
      case _ => throw new NotImplementedError()
    }
  }

  private def allocScanSeq(scan: ScanSeq,
                           call: FunCall,
    numGlb: Allocator,
    numLcl: Allocator,
    numPvt: Allocator,
    inMem: OpenCLMemory): OpenCLMemory = {
      inMem match {
        case coll: OpenCLMemoryCollection =>
          //"Connect" the input memories to the parameters of F
          val init_mem = coll.subMemories(0)
          val input_mem = coll.subMemories(1)
          scan.f.params(0).mem = init_mem
          scan.f.params(1).mem = input_mem
          val bodyM = alloc(scan.f.body, numGlb, numLcl, numPvt)
          // replace `bodyM` by `init_mem` in the lambda's body
          Expr.visit(scan.f.body, e => if (e.mem == bodyM) e.mem = init_mem, _ => {})

          val maxSizeInBytes = Type.getAllocatedSize(call.t)
          val baseSize = Type.getAllocatedSize(Type.getBaseType(call.t))

          val maxGlbOutSize = numGlb(baseSize, maxSizeInBytes)
          val maxLclOutSize = numLcl(baseSize, maxSizeInBytes)
          val maxPvtOutSize = numPvt(baseSize, maxSizeInBytes)

          val resultMem = OpenCLMemory.allocMemory(maxGlbOutSize, maxLclOutSize,
            maxPvtOutSize, call.addressSpace)
          resultMem

        case _ =>
          throw new IllegalArgumentException("Cannot allocate memory for scanSeq: an OpenCL memory collection input is needed")
      }
  }

  private def allocZipTuple(inMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        if (coll.subMemories.length < 2) throw new NumberOfArgumentsException
        coll
      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def allocGet(n: Int, inMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        assert(n < coll.subMemories.length)
        coll.subMemories(n)
      case _ => inMem
      // case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocFilter(f: Filter,
    numGlb: Allocator,
    numLcl: Allocator,
    inMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        if (coll.subMemories.length != 2) throw new NumberOfArgumentsException
        coll.subMemories(0)
      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def setMemInParams(params: Array[Param], mem: OpenCLMemory ): Unit = {
    params.length match {
      case 1 => params.head.mem = mem
      case _ =>
        val coll = mem.asInstanceOf[OpenCLMemoryCollection]
        (params zip coll.subMemories).foreach({case (p, m) => p.mem = m})
    }
  }

  /**
   * Helper function for computing the size to allocate for an array given its
   * type and the size of its elements
   *
   * @param getSizeFromInner a scala function allocating the whole array given the inner size
   * @param ty the type of the array
   * @return a scala function that allocates this array. Might be `?`
   */
  private def sizeOfArray(getSizeFromInner: Allocator, ty: Type): Allocator = {
    def align(n: ArithExpr): ArithExpr = ((n + size_t.size - 1) / size_t.size) * size_t.size

    ty match {
      case at: ArrayType =>
        at match {
          case c: Capacity =>
            val map = TypeVar.getTypeVars(c.capacity).map(tv => (tv, tv.range.max)).toMap
            val capacity = ArithExpr.substitute(c.capacity, map.toMap)
            def alloc(baseSize: ArithExpr, innerSize: ArithExpr): ArithExpr = {
              if (baseSize.eval < size_t.size.eval)
                getSizeFromInner(baseSize, at.headerLength * size_t.size + align(capacity * innerSize))
              else
                getSizeFromInner(baseSize, at.headerLength * baseSize + capacity * innerSize)
            }
            alloc
          case _ =>
            // Unknown capacity. We can't know how much memory to allocate…
            // See the comments at the top of this file for further information.
            (_, _) => ?
        }
      case _ => throw new IllegalArgumentException("sizeOfArray expects an array type")
    }
  }
}
