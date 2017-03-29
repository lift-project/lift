package opencl.ir

import lift.arithmetic.{ArithExpr, Cst}
import ir._
import ir.ast._
import opencl.ir.OpenCLMemory._
import opencl.ir.pattern._

object OpenCLMemoryAllocator {

  def apply(f: Lambda) = {
    f.params.foreach((p) =>
      p.mem = OpenCLMemory.allocMemory(OpenCLMemory.getSizeInBytes(p.t), p.addressSpace)
    )

    alloc(f.body)
  }

  /** Allocate OpenCLMemory objects for a given Fun f
    *
    * @param expr   The expression for which memory should be allocated
    * @param numGlb Number of ...
    * @param numLcl Number of ..
    * @return The OpenCLMemory used as output by f
    */
  def alloc(expr: Expr,
    numGlb: ArithExpr = 1,
    numLcl: ArithExpr = 1,
    numPvt: ArithExpr = 1): OpenCLMemory = {

    val result = expr match {
      case v: Value => allocValue(v)
      case vp: VectorParam => allocParam(vp.p)
      case p: Param => allocParam(p)
      case call: FunCall =>
        allocFunCall(call, numGlb, numLcl, numPvt, UndefAddressSpace)
    }
    // set the output
    assert(result != OpenCLNullMemory)
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
      OpenCLMemory.allocPrivateMemory(getSizeInBytes(v.t))
    }
  }

  private def allocParam(p: Param): OpenCLMemory = {
    if (p.mem == UnallocatedMemory)
      throw new IllegalArgumentException(s"Param $p has UnallocatedMemory")

    OpenCLMemory.asOpenCLMemory(p.mem)
  }

  private def allocFunCall(call: FunCall,
    numGlb: ArithExpr,
    numLcl: ArithExpr,
    numPvt: ArithExpr,
    addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    // Get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call, numGlb, numLcl, numPvt)

    // Determine the output memory based on the type of f ...
    call.f match {
      // Here is where the actual allocation happens
      case _: UserFun | _: VectorizeUserFun =>
        allocUserFun(call.t, numGlb, numLcl, numPvt, call)

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

      case r: AbstractPartRed => allocReduce(r, numGlb, numLcl, numPvt, inMem)

      case s: AbstractSearch => allocSearch(s, call, numGlb, numLcl, numPvt, inMem)

      case it: Iterate => allocIterate(it, call, numGlb, numLcl, numPvt, inMem)

      case l: Lambda => allocLambda(l, numGlb, numLcl, numPvt, inMem)
      case toGlobal(f) => allocLambda(f, numGlb, numLcl, numPvt, inMem)
      case toLocal(f) => allocLambda(f, numGlb, numLcl, numPvt, inMem)
      case toPrivate(f) => allocLambda(f, numGlb, numLcl, numPvt, inMem)

      case Zip(_) | Tuple(_) => allocZipTuple(inMem)
      case Get(n) => allocGet(n, inMem)
      case f: Filter => allocFilter(f, numGlb, numLcl, inMem)
      case ua: UnsafeArrayAccess => allocUnsafeArrayAccess(ua, call, numGlb, numLcl, numPvt, inMem)

      case Split(_) | Join() | asVector(_) | asScalar() |
           Transpose() | Unzip() | TransposeW() | Slide(_, _) | Pad(_, _, _) |
           Head() | Tail() | Gather(_) | Scatter(_) | ArrayAccess(_) | PrintType() =>
        inMem
    }
  }

  private def getInMFromArgs(call: FunCall,
    numGlb: ArithExpr,
    numLcl: ArithExpr,
    numPvt: ArithExpr): OpenCLMemory = {
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
    numGlb: ArithExpr,
    numLcl: ArithExpr,
    numPvt: ArithExpr,
    call: FunCall): OpenCLMemory = {

    if (call.addressSpace == UndefAddressSpace)
      throw new RuntimeException("No address space at " + call)

    val maxSizeInBytes = getSizeInBytes(outT)
    // size in bytes necessary to hold the result of f in the different
    // memory spaces
    val maxGlbOutSize = maxSizeInBytes * numGlb
    val maxLclOutSize = maxSizeInBytes * numLcl
    val maxPvtOutSize = maxSizeInBytes * numPvt

    OpenCLMemory.allocMemory(maxGlbOutSize, maxLclOutSize,
      maxPvtOutSize, call.addressSpace)
  }

  private def allocLambda(l: Lambda,
    numGlb: ArithExpr,
    numLcl: ArithExpr,
    numPvt: ArithExpr,
    inMem: OpenCLMemory): OpenCLMemory = {
    setMemInParams(l.params, inMem)
    alloc(l.body, numGlb, numLcl, numPvt)
  }

  private def allocMapGlb(am: AbstractMap,
    outT: Type,
    numGlb: ArithExpr,
    numLcl: ArithExpr,
    numPvt: ArithExpr,
    inMem: OpenCLMemory): OpenCLMemory = {
    am.f.params(0).mem = inMem

    val len = Type.getMaxLength(outT)
    alloc(am.f.body, numGlb * len, numLcl, numPvt)
  }

  private def allocMapAtomWrg(am: AbstractMap,
    outT: Type,
    numGlb: ArithExpr,
    numLcl: ArithExpr,
    numPvt: ArithExpr,
    inMem: OpenCLMemory): OpenCLMemory = {
    am.f.params(0).mem = inMem

    am.asInstanceOf[MapAtomWrg].globalTaskIndex =
      OpenCLMemory.allocGlobalMemory(Type.getMaxSize(Int))

    val len = Type.getMaxLength(outT)
    alloc(am.f.body, numGlb * len, numLcl, numPvt)
  }

  private def allocMapSeqLcl(am: AbstractMap,
    outT: Type,
    numGlb: ArithExpr,
    numLcl: ArithExpr,
    numPvt: ArithExpr,
    inMem: OpenCLMemory): OpenCLMemory = {
    am.f.params(0).mem = inMem

    val len = Type.getMaxLength(outT)

    val privateMultiplier: ArithExpr =
      if (am.f.body.addressSpace.containsAddressSpace(PrivateMemory) ||
          inMem.addressSpace.containsAddressSpace(PrivateMemory))
        am.iterationCount
      else
        1

    alloc(am.f.body, numGlb * len, numLcl * len, numPvt * privateMultiplier)
  }

  private def allocReduce(r: AbstractPartRed,
    numGlb: ArithExpr,
    numLcl: ArithExpr,
    numPvt: ArithExpr,
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

  private def allocSearch(s: AbstractSearch, call: FunCall,
    numGlb: ArithExpr,
    numLcl: ArithExpr,
    numPvt: ArithExpr,
    inMem: OpenCLMemory): OpenCLMemory = {

    inMem match {
      case coll: OpenCLMemoryCollection =>
        // Set the comparison function input to be the elements
        // of the array we're searching
        s.f.params(0).mem = coll.subMemories(1)
        // Allocate memory for the comparison function
        alloc(s.f.body, numGlb, numLcl, numPvt)

        val size = getSizeInBytes(call.t)
        // TODO: Do this the way the reduce does it - it makes a lot more sense!?
        // HOW IT'S ACTUALLY DONE: manually allocate that memory,
        // storing it in the correct address space
        OpenCLMemory.allocMemory(size, call.addressSpace)

      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def allocUnsafeArrayAccess(ua: UnsafeArrayAccess,
                                     call: FunCall,
                                     numGlb: ArithExpr,
                                     numLcl: ArithExpr,
                                     numPvt: ArithExpr,
                                     inMem: OpenCLMemory): OpenCLMemory = {
    // let the index allocate memory for itself (most likely a param, so it will know its memory)
    alloc(ua.index, numGlb, numLcl, numPvt)

    // allocate memory itself
    val outputSize = getSizeInBytes(call.t)
    // manually allocate that much memory, storing it in the correct address space
    if (call.addressSpace != UndefAddressSpace) {
     // use given address space
      OpenCLMemory.allocMemory(outputSize, outputSize, outputSize,
                           call.addressSpace)
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
    numGlb: ArithExpr,
    numLcl: ArithExpr,
    numPvt: ArithExpr,
    inMem: OpenCLMemory): OpenCLMemory = {

    it.n match {
      case Cst(1) => // do not allocate a swap buffer when we only iterate once
      case _ =>
        // Get sizes in bytes necessary to hold the input and output of the
        // function inside the iterate
        val inSize = getSizeInBytes(call.argsType)
        val outSize = getSizeInBytes(call.t)

        // Get the max from those two
        val largestSize = ArithExpr.max(inSize, outSize)

        // Create a swap buffer
        it.swapBuffer = OpenCLMemory.allocMemory(
          largestSize * numGlb, largestSize * numLcl, largestSize * numPvt, inMem.addressSpace)
    }

    // Recurse to allocate memory for the function(s) inside
    it.f.params(0).mem = inMem
    alloc(it.f.body, numGlb, numLcl, numPvt)
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
    numGlb: ArithExpr,
    numLcl: ArithExpr,
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

}
