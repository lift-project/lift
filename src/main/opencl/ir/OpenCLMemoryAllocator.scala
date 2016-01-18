package opencl.ir

import apart.arithmetic.{?, ArithExpr}
import ir._
import ir.ast._
import opencl.ir.pattern._

import OpenCLMemory._

object OpenCLMemoryAllocator {

  def apply(f: Lambda) = {
    f.params.foreach((p) =>
      p.t match {
        case _: ScalarType =>
          p.mem = OpenCLMemory.allocPrivateMemory(
            OpenCLMemory.getMaxSizeInBytes(p.t))
        case _ =>
          p.mem = OpenCLMemory.allocGlobalMemory(
            OpenCLMemory.getMaxSizeInBytes(p.t))
      })

      alloc(f.body)
  }


  /** Allocate OpenCLMemory objects for a given Fun f
    *
    * @param expr The expression for which memory should be allocated
    * @param numGlb Number of ...
    * @param numLcl Number of ..
    * @param addressSpace The OpenCL address space where memory should be allocated
    * @return The OpenCLMemory used as output by f
    */
  def alloc(expr: Expr,
            numGlb: ArithExpr = 1,
            numLcl: ArithExpr = 1,
            numPvt: ArithExpr = 1,
            addressSpace: OpenCLAddressSpace = UndefAddressSpace): OpenCLMemory = {

    val result = expr match {
      case v: Value => allocValue(v)
      case p: Param => allocParam(p)
      case call: FunCall =>
        allocFunCall(call, numGlb, numLcl, numPvt, addressSpace)
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

  private def allocParam(param: Param): OpenCLMemory = {
    val res = param match {
      case vp: VectorParam =>
        if (vp.p.mem == UnallocatedMemory)
          throw new IllegalArgumentException("PANIC!")
        vp.p.mem
      case p: Param =>
        if (p.mem == UnallocatedMemory)
          throw new IllegalArgumentException("PANIC!")
        p.mem
    }

    OpenCLMemory.asOpenCLMemory(res)
  }

  private def allocFunCall(call: FunCall,
                           numGlb: ArithExpr,
                           numLcl: ArithExpr,
                           numPvt: ArithExpr,
                           addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    // get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call, numGlb, numLcl, numPvt, addressSpace)

    // determine the output memory based on the type of f ...
    call.f match {
      // here is where the actual allocation happens
      case uf: UserFun        => allocUserFun(call.t, numGlb, numLcl, numPvt,
                                              inMem, addressSpace)
      case vec: VectorizeUserFun
                              => allocUserFun(call.t, numGlb, numLcl, numPvt,
                                              inMem, addressSpace)
      case l: Lambda          => allocLambda(l, numGlb, numLcl, numPvt,
                                             inMem, addressSpace)
      case MapGlb(_, _) |
           MapWrg(_, _) |
           Map(_)             => allocMapGlb(call.f.asInstanceOf[AbstractMap],
                                             call.t, numGlb, numLcl, numPvt,
                                             inMem, addressSpace)
      case MapLcl(_, _) |
           MapWarp(_)   |
           MapLane(_)   |
           MapSeq(_)          => allocMapLcl(call.f.asInstanceOf[AbstractMap],
                                             call.t, numGlb, numLcl, numPvt,
                                             inMem, addressSpace)
      case r: AbstractPartRed => allocReduce(r, numGlb, numLcl, numPvt, inMem)
      case s: AbstractSearch  => allocSearch(s, call, numGlb, numLcl, numPvt, inMem, addressSpace)
      case it: Iterate        => allocIterate(it, call, numGlb, numLcl, numPvt,
                                              inMem)
      case tg: toGlobal       => allocToGlobal(tg, numGlb, numLcl, numPvt,
                                               inMem)
      case tl: toLocal        => allocToLocal(tl, numGlb, numLcl, numPvt,
                                              inMem)
      case tp: toPrivate      => allocToPrivate(tp, numGlb, numLcl, numPvt,
                                                inMem)
      case Zip(_) | Tuple(_)  => allocZipTuple(inMem)
      case Get(n)             => allocGet(n, inMem)
      case f: Filter          => allocFilter(f, numGlb, numLcl, inMem)
      case Split(_)    | Join()  | asVector(_)  | asScalar() |
           Transpose() | Unzip() | TransposeW() | Group(_)   | Pad(_,_) |
           Head()      | Tail()  | Gather(_)    | Scatter(_) =>
        inMem
    }
  }

  private def getInMFromArgs(call: FunCall,
                             numGlb: ArithExpr,
                             numLcl: ArithExpr,
                             numPvt: ArithExpr,
                             addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    call.args.length match {
      case 0 => OpenCLNullMemory
      case 1 => alloc(call.args.head, numGlb, numLcl, numPvt, addressSpace)
      case _ => OpenCLMemoryCollection(
                  call.args.map(alloc(_, numGlb, numLcl, numPvt, addressSpace)))
    }
  }

  private def allocUserFun(outT: Type,
                           numGlb: ArithExpr,
                           numLcl: ArithExpr,
                           numPvt: ArithExpr,
                           inMem: OpenCLMemory,
                           addressSpace: OpenCLAddressSpace): OpenCLMemory = {

    val maxSizeInBytes = getMaxSizeInBytes(outT)
    // size in bytes necessary to hold the result of f in the different
    // memory spaces
    val maxGlbOutSize = maxSizeInBytes * numGlb
    val maxLclOutSize = maxSizeInBytes * numLcl
    val maxPvtOutSize = maxSizeInBytes * numPvt

    if (addressSpace != UndefAddressSpace) {
      // use given address space
      OpenCLMemory.allocMemory(maxGlbOutSize, maxLclOutSize, maxPvtOutSize,
                               addressSpace)
    } else {
      // address space is not predetermined
      //  => figure out the address space based on the input address space(s)
      val addressSpace =
        inMem match {
          case coll: OpenCLMemoryCollection =>
            coll.addressSpace.findCommonAddressSpace()
          case m: OpenCLMemory => m.addressSpace
        }
      OpenCLMemory.allocMemory(maxGlbOutSize, maxLclOutSize, maxPvtOutSize,
                               addressSpace)
    }
  }

  private def allocLambda(l: Lambda,
                          numGlb: ArithExpr,
                          numLcl: ArithExpr,
                          numPvt: ArithExpr,
                          inMem: OpenCLMemory,
                          addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    setMemInParams(l.params, inMem)
    alloc(l.body, numGlb, numLcl, numPvt, addressSpace)
  }

  private def allocMapGlb(am: AbstractMap,
                          outT: Type,
                          numGlb: ArithExpr,
                          numLcl: ArithExpr,
                          numPvt: ArithExpr,
                          inMem: OpenCLMemory,
                          addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    am.f.params(0).mem = inMem

    val maxLen = ArithExpr.max(Type.getLength(outT))
    alloc(am.f.body, numGlb * maxLen, numLcl, numPvt, addressSpace)
  }

  private def allocMapLcl(am: AbstractMap,
                          outT: Type,
                          numGlb: ArithExpr,
                          numLcl: ArithExpr,
                          numPvt: ArithExpr,
                          inMem: OpenCLMemory,
                          addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    am.f.params(0).mem = inMem

    val maxLen = ArithExpr.max(Type.getLength(outT))

    var privateMultiplier = am.iterationCount
    privateMultiplier = if (privateMultiplier == ?) 1 else privateMultiplier

    alloc(am.f.body, numGlb * maxLen, numLcl * maxLen,
          numPvt * privateMultiplier, addressSpace)
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
        val bodyM = alloc(r.f.body, numGlb, numLcl, numPvt, initM.addressSpace)

        // replace `bodyM` by `initM` in `r.f.body`
        Expr.visit(r.f.body, e => if (e.mem == bodyM) e.mem = initM , _ => {} )

        initM // return initM as the memory of the reduction pattern
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocSearch(s: AbstractSearch, call: FunCall,
                          numGlb: ArithExpr,
                          numLcl: ArithExpr,
                          numPvt: ArithExpr, 
                          inMem: OpenCLMemory,
                          addressSpace: OpenCLAddressSpace): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        val defaultM = coll.subMemories(0)
        // set the comparison function input to be the elements of the array we're searching
        s.f.params(0).mem = coll.subMemories(1)
        // allocate memory for the comparison function - otherwise the allocator will complain!
        s.searchFMem = alloc(s.f.body, numGlb, numLcl, numPvt, PrivateMemory)

        // TODO: This is the way the reduce does it - it makes a lot more sense!
        // Fix it so that we do it too?
        // use the ``default value'' memory to return our value
        // return defaultM

        // HOW IT'S ACTUALLY DONE:
        // get the size of memory we return from the search, based off the type we return
        val outputSize = getSizeInBytes(call.t) 
        // manually allocate that much memory, storing it in the correct address space
        if (addressSpace != UndefAddressSpace) {
         // use given address space
          OpenCLMemory.allocMemory(outputSize, outputSize, outputSize,
                               addressSpace)
        } else {
          // address space is not predetermined
          //  => figure out the address space based on the input address space(s)
          val addressSpace =
            inMem match {
              case coll: OpenCLMemoryCollection =>
                coll.addressSpace.findCommonAddressSpace()
              case m: OpenCLMemory => m.addressSpace
              case _ => throw new IllegalArgumentException("PANIC")
            }
          OpenCLMemory.allocMemory(outputSize, outputSize, outputSize,
                                   addressSpace)
        }
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocIterate(it: Iterate, call: FunCall,
                           numGlb: ArithExpr,
                           numLcl: ArithExpr,
                           numPvt: ArithExpr,
                           inMem: OpenCLMemory): OpenCLMemory = {
    // get sizes in bytes necessary to hold the input and output of the
    // function inside the iterate
    val inSize = getMaxSizeInBytes(call.argsType)
    val outSize = getMaxSizeInBytes(call.t)
    // get the max from those two
    val largestSize = ArithExpr.max(inSize, outSize)

    // create a swap buffer
    it.swapBuffer =
      OpenCLMemory.allocMemory(largestSize*numGlb, largestSize*numLcl, largestSize*numPvt,
                               inMem.addressSpace)

    // recurs to allocate memory for the function(s) inside
    it.f.params(0).mem = inMem
    alloc(it.f.body, numGlb, numLcl, numPvt)
  }

  private def allocToGlobal(tg: toGlobal,
                            numGlb: ArithExpr,
                            numLcl: ArithExpr,
                            numPvt: ArithExpr,
                            inMem: OpenCLMemory): OpenCLMemory = {
    setMemInParams(tg.f.params, inMem)

    alloc(tg.f.body, numGlb, numLcl, numPvt, GlobalMemory)
  }

  private def allocToLocal(tl: toLocal,
                           numGlb: ArithExpr,
                           numLcl: ArithExpr,
                           numPvt: ArithExpr,
                           inMem: OpenCLMemory): OpenCLMemory = {
    setMemInParams(tl.f.params, inMem)

    alloc(tl.f.body, numGlb, numLcl, numPvt, LocalMemory)
  }

  private def allocToPrivate(tp: toPrivate,
                             numGlb: ArithExpr,
                             numLcl: ArithExpr,
                             numPvt: ArithExpr,
                             inMem: OpenCLMemory): OpenCLMemory = {
    setMemInParams(tp.f.params, inMem)

    alloc(tp.f.body, numGlb, numLcl, numPvt, PrivateMemory)
  }

  private def allocZipTuple(inMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        if (coll.subMemories.length < 2) throw new NumberOfArgumentsException
        coll
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def allocGet(n: Int, inMem: OpenCLMemory): OpenCLMemory = {
    inMem match {
      case coll: OpenCLMemoryCollection =>
        assert(n < coll.subMemories.length)
        coll.subMemories(n)
      case _ => inMem
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
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def setMemInParams(params: Array[Param], mem: OpenCLMemory ): Unit = {
    params.length match {
      case 1 => params.head.mem = mem
      case _ =>
        val coll = mem match { case coll: OpenCLMemoryCollection => coll}
        (params zip coll.subMemories).foreach({case (p, m) => p.mem = m})
    }
  }

}
