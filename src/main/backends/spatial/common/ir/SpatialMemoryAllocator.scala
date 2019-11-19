package backends.spatial.common.ir

import backends.spatial.accel.ir.pattern._
import _root_.ir.ast.{AbstractMap, AbstractSearch, ArrayAccess, ArrayConstructors, ArrayFromExpr, CheckedArrayAccess, Concat, Expr, Filter, FunCall, Gather, Get, Head, Id, Iterate, Join, Lambda, Map, Pad, PadConstant, Param, RewritingGuidePost, Scatter, Slide, Split, Tail, Transpose, TransposeW, Tuple, UnsafeArrayAccess, Unzip, UserFun, Value, VectorParam, VectorizeUserFun, Zip, asScalar, asVector, debug}
import _root_.ir.{ArrayType, NumberOfArgumentsException, Size, Type, UnallocatedMemory}

object SpatialMemoryAllocator {
  /** innerType => fullType */
  type Allocator = Type => Type

  /**
    * Allocate memory for both the body and the parameters of the outermost lambda expression
    *
    * @param f the lambda expression
    * @return the SpatialMemory used as output by f
    */
  def apply(f: Lambda): SpatialMemory = {
    f.params.foreach((p) =>
      p.mem = SpatialMemory.allocMemory(p.t, p.addressSpace)
    )

    alloc(f.body, t => t, DRAMMemory)
  }

  /**
    * Allocate SpatialMemory objects for a given expression
    *
    * @param expr             The expression for which memory should be allocated
    * @param outMemT          The type of the output memory which the expression is writing to
    * @param outAddressSpace  The address space of the output memory which the expression is writing to.
   *                          In Lift-OpenCL, it would always be the same of the that of the writer userfun call.
   *                          In Lift-Spatial, it might be different when address space caster is used.
    * @return                 The SpatialMemory used by expr
    */
  def alloc(expr: Expr, outMemT: Allocator, outAddressSpace: SpatialAddressSpace): SpatialMemory = {

    val result = expr match {
      case ArrayFromExpr(e)       => throw new NotImplementedError()
      case _: ArrayConstructors   => throw new NotImplementedError()

      case v: Value               => allocValue(v)
      case vp: VectorParam        => throw new NotImplementedError()
      case p: Param               => allocParam(p)
      case call: FunCall          => allocFunCall(call, outMemT, outAddressSpace)
      case _                      => throw new NotImplementedError()
    }
    // set the output
    expr.mem = result

    // finally, return the output
    result
  }

  private def allocValue(v: Value): SpatialMemory = {
    val spatialMem = SpatialMemory.asSpatialMemory(v.mem)

    spatialMem.addressSpace match {
      case LiteralMemory => spatialMem
      case UndefAddressSpace => SpatialMemory.allocMemory(v.t, v.addressSpace)
      case _ => throw new IllegalArgumentException(s"Unexpected address space ${spatialMem.addressSpace} for Value $v")
    }
  }

  private def allocParam(p: Param): SpatialMemory = {
    if (p.mem == UnallocatedMemory)
      throw new IllegalArgumentException(s"Param $p has UnallocatedMemory")

    SpatialMemory.asSpatialMemory(p.mem)
  }

  private def allocFunCall(call: FunCall,
                           outMemT: Allocator,
                           outAddressSpace: SpatialAddressSpace): SpatialMemory = {
    // Determine argument memory type:
    // If this node creates new memory, than the argument memory type is the argument type.
    // If this node only propagates or transforms the view the data of the argument, the argument memory type
    // is the outMemT of this expression
    val argMemT = if (call.isConcrete(visitArgs = false)) (t: Type) => t else outMemT

    // Get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call, argMemT, outAddressSpace)

    // Determine the output memory based on the type of f ...
    call.f match {
      // Here is where the actual allocation happens:
      case _: UserFun               => allocUserFun(call, outMemT, outAddressSpace)
      case  _: VectorizeUserFun     => throw new NotImplementedError()

      case Map(_)                   => allocMap(call.f.asInstanceOf[AbstractMap], call, outMemT, outAddressSpace, inMem)

      case sf: SpForeach            => allocSpForeach(sf, call, outMemT, outAddressSpace, inMem)
      case m: MapSeq                => allocMapSeq(m, call, outMemT, outAddressSpace, inMem)
      case asf: AbstractSpFold      => allocAbstrSpFold(asf, call, outMemT, outAddressSpace, inMem)
      case r: ReduceSeq             => allocReduceSeq(r, call, outMemT, outAddressSpace, inMem)

      case s: AbstractSearch        => throw new NotImplementedError()

      case it: Iterate              => throw new NotImplementedError()

      case cc: Concat               => throw new NotImplementedError()

      case l: Lambda                => allocLambda(l, outMemT, outAddressSpace, inMem)

      case toDRAM(f)                => allocLambda(f, outMemT, DRAMMemory, inMem)
      case toSRAM(f)                => allocLambda(f, outMemT, SRAMMemory, inMem)
      case toArgOut(f)              => allocLambda(f, outMemT, ArgOutMemory, inMem)
      case toReg(f)                 => allocLambda(f, outMemT, RegMemory, inMem)

      case Zip(_) | Tuple(_)        => allocZipTuple(inMem)
      case Get(n)                   => allocGet(n, inMem)
      case f: Filter                => throw new NotImplementedError()
      case ua: UnsafeArrayAccess    => throw new NotImplementedError()
      case ca: CheckedArrayAccess   => throw new NotImplementedError()

      case sp: SchedulingPattern    => allocLambda(sp.f, outMemT, outAddressSpace, inMem)

      case debug.PrintView(_, f)    => allocLambda(f, outMemT, outAddressSpace, inMem)

      case RewritingGuidePost(_)    => inMem

      case Map(_) |
           Split(_) | Join() | asVector(_) | asScalar() |
           Transpose() | Unzip() | TransposeW() | Slide(_, _) | Pad(_, _, _) | PadConstant(_, _, _) |
           Head() | Tail() | Gather(_) | Scatter(_) | ArrayAccess(_) |
           debug.PrintType(_) | debug.PrintTypeInConsole(_) | debug.PrintComment(_) | debug.AssertType(_, _) |
           Id()                     => inMem

      case _                        => throw new NotImplementedError()
    }
  }

  private def getInMFromArgs(call: FunCall,
                             outMemT: Allocator,
                             outAddressSpace: SpatialAddressSpace): SpatialMemory = {
    call.args.length match {
      case 0 => throw new IllegalArgumentException(s"Function call without arguments $call")
      case 1 => alloc(call.args.head, outMemT, outAddressSpace)
      case _ => SpatialMemoryCollection(call.args.map(arg => alloc(arg, outMemT, outAddressSpace)))
    }
  }

  private def allocUserFun(call: FunCall,
                           outMemT: Allocator,
                           outAddressSpace: SpatialAddressSpace): SpatialMemory = {
    if (call.addressSpace == UndefAddressSpace)
      throw new RuntimeException("No address space at " + call)

    SpatialMemory.allocMemory(outMemT(call.t), outAddressSpace)
  }

  private def allocLambda(l: Lambda,
                          outMemT: Allocator,
                          outAddressSpace: SpatialAddressSpace,
                          inMem: SpatialMemory): SpatialMemory = {
    setMemInParams(l.params, inMem)
    alloc(l.body, outMemT, outAddressSpace)
  }

  private def allocMap(am: AbstractMap,
                       call: FunCall,
                       outMemT: Allocator,
                       outAddressSpace: SpatialAddressSpace,
                       inMem: SpatialMemory): SpatialMemory = {
    val outerSize = call.t.asInstanceOf[ArrayType with Size].size
    am.f.params(0).mem = inMem

    alloc(am.f.body, innerType => outMemT(ArrayType(innerType, outerSize)), outAddressSpace)
  }

  private def allocSpForeach(sf: SpForeach,
                             call: FunCall,
                             outMemT: Allocator,
                             outAddressSpace: SpatialAddressSpace,
                             inMem: SpatialMemory): SpatialMemory = {
    val outerSize = call.t.asInstanceOf[ArrayType with Size].size
    sf.f.params(0).mem = inMem

    alloc(sf.f.body, innerType => outMemT(ArrayType(innerType, outerSize)), outAddressSpace)
  }

  private def allocMapSeq(m: MapSeq,
                          call: FunCall,
                          outMemT: Allocator,
                          outAddressSpace: SpatialAddressSpace,
                          inMem: SpatialMemory): SpatialMemory = {
    val outerSize = call.t.asInstanceOf[ArrayType with Size].size
    m.f.params(0).mem = inMem

    alloc(m.f.body, innerType => outMemT(ArrayType(innerType, outerSize)), outAddressSpace)
  }

  private def allocAbstrSpFold(asf: AbstractSpFold,
                               call: FunCall,
                               outMemT: Allocator,
                               outAddressSpace: SpatialAddressSpace,
                               inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case coll: SpatialMemoryCollection =>
        val initM = coll.subMemories(0)

        initM.bufferHazard = true

        asf.fMap.params(0).mem = coll.subMemories(1)
        // fMap body memory has the size of chunkSize. Although fReduce reads data produced by fMap,
        // it expects memory of size call.args(1).t.size. Here, we will allocate output memory of fMap
        // and input memory of fReduce separately. This will not be a problem during code generation as those
        // memories are not explicitly written to / read from. Spatial takes care of the disparity.
        // Note, the empty allocator (t => t) is there because fMap.body will have a loop iterating chunkSize times,
        // which will increase the allocator
        alloc(asf.fMap.body, t => t, asf.fMap.body.addressSpace)

        // Here, we are doing something potentially dangerous: associate one variable with two memories,
        // the first one referring to the map body memory (containing single tile) and the second one
        // referring to the bigger map memory (containing all tiles)
        asf.fMapMem = SpatialMemory(asf.fMap.body.mem.variable, asf.flatMapT,
          asf.fMap.body.mem.asInstanceOf[SpatialMemory].addressSpace)

        asf.fReduce.params(0).mem = initM
        asf.fReduce.params(1).mem = asf.fMapMem

        val reduceBodyM = alloc(asf.fReduce.body, t => t, initM.addressSpace)

        // replace `bodyM` by `initM` in `asf.fReduce.body`
        Expr.visit(asf.fReduce.body, e => if (e.mem == reduceBodyM) e.mem = initM, _ => {})

        initM
      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def allocReduceSeq(r: ReduceSeq,
                             call: FunCall,
                             outMemT: Allocator,
                             outAddressSpace: SpatialAddressSpace,
                             inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case coll: SpatialMemoryCollection =>
        val initM = coll.subMemories(0)

        initM.bufferHazard = true

        r.f.params(0).mem = initM
        r.f.params(1).mem = coll.subMemories(1)

        val reduceBodyM = alloc(r.f.body, t => t, initM.addressSpace)

        // replace `bodyM` by `initM` in `r.f.body`
        Expr.visit(r.f.body, e => if (e.mem == reduceBodyM) e.mem = initM, _ => {})

        initM
      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def allocZipTuple(inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case coll: SpatialMemoryCollection =>
        if (coll.subMemories.length < 2) throw new NumberOfArgumentsException
        coll
      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def allocGet(n: Int, inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case coll: SpatialMemoryCollection =>
        assert(n < coll.subMemories.length)
        coll.subMemories(n)
      case _ => inMem
    }
  }

  private def setMemInParams(params: Array[Param], inMem: SpatialMemory): Unit = {
    params.length match {
      case 1 => params.head.mem = inMem
      case _ =>
        val coll = inMem.asInstanceOf[SpatialMemoryCollection]
        (params zip coll.subMemories).foreach({case (p, m) => p.mem = m})
    }
  }
}
