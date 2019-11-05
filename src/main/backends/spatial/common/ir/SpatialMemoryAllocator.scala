package backends.spatial.common.ir

import backends.spatial.accel.ir.pattern._
import _root_.ir.ast.{AbstractMap, AbstractSearch, ArrayAccess, ArrayConstructors, ArrayFromExpr, CheckedArrayAccess, Concat, Expr, Filter, FunCall, Gather, Get, Head, Id, Iterate, Join, Lambda, Map, Pad, PadConstant, Param, RewritingGuidePost, Scatter, Slide, Split, Tail, Transpose, TransposeW, Tuple, UnsafeArrayAccess, Unzip, UserFun, Value, VectorParam, VectorizeUserFun, Zip, asScalar, asVector, debug}
import _root_.ir.{NumberOfArgumentsException, Type, UnallocatedMemory}

object SpatialMemoryAllocator {
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

    alloc(f.body, f.body.t, DRAMMemory)
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
  def alloc(expr: Expr, outMemT: Type, outAddressSpace: SpatialAddressSpace): SpatialMemory = {

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
                           outMemT: Type,
                           outAddressSpace: SpatialAddressSpace): SpatialMemory = {
    // Get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call, outMemT, outAddressSpace)

    // Determine the output memory based on the type of f ...
    call.f match {
      // Here is where the actual allocation happens:
      case _: UserFun               => allocUserFun(outMemT, outAddressSpace, call)
      case  _: VectorizeUserFun     => throw new NotImplementedError()

      case Map(_)                   => allocMap(call.f.asInstanceOf[AbstractMap], outMemT, outAddressSpace, inMem)

      case sf: SpForeach            => allocSpForeach(sf, outMemT, outAddressSpace, inMem)
      case m: MapSeq                => allocMapSeq(m, outMemT, outAddressSpace, inMem)
      case asf: AbstractSpFold      => allocAbstrSpFold(asf, outMemT, outAddressSpace, inMem)

      case s: AbstractSearch        => throw new NotImplementedError()

      case it: Iterate              => throw new NotImplementedError()

      case cc: Concat               => throw new NotImplementedError()

      case l: Lambda                => allocLambda(l, outMemT, outAddressSpace, inMem)

      case toDRAM(f)                => allocLambda(f, call.t, DRAMMemory, inMem)
      case toSRAM(f)                => allocLambda(f, call.t, SRAMMemory, inMem)
      case toReg(f)                 => allocLambda(f, call.t, RegMemory, inMem)

      case Zip(_) | Tuple(_)        => allocZipTuple(inMem)
      case Get(n)                   => allocGet(n, inMem)
      case f: Filter                => throw new NotImplementedError()
      case ua: UnsafeArrayAccess    => throw new NotImplementedError()
      case ca: CheckedArrayAccess   => throw new NotImplementedError()

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
                             outMemT: Type,
                             outAddressSpace: SpatialAddressSpace): SpatialMemory = {
    call.args.length match {
      case 0 => throw new IllegalArgumentException(s"Function call without arguments $call")
      case 1 => alloc(call.args.head, outMemT, outAddressSpace)
      case _ => SpatialMemoryCollection(call.args.map(arg => alloc(arg, outMemT, outAddressSpace)))
    }
  }

  private def allocUserFun(outMemT: Type,
                           outAddressSpace: SpatialAddressSpace,
                           call: FunCall): SpatialMemory = {
    if (call.addressSpace == UndefAddressSpace)
      throw new RuntimeException("No address space at " + call)

    SpatialMemory.allocMemory(outMemT, outAddressSpace)
  }

  private def allocLambda(l: Lambda,
                          outMemT: Type,
                          outAddressSpace: SpatialAddressSpace,
                          inMem: SpatialMemory): SpatialMemory = {
    setMemInParams(l.params, inMem)
    alloc(l.body, outMemT, outAddressSpace)
  }

  private def allocMap(am: AbstractMap,
                       outMemT: Type,
                       outAddressSpace: SpatialAddressSpace,
                       inMem: SpatialMemory): SpatialMemory = {
    am.f.params(0).mem = inMem

    alloc(am.f.body, outMemT, outAddressSpace)
  }

  private def allocSpForeach(sf: SpForeach,
                             outMemT: Type,
                             outAddressSpace: SpatialAddressSpace,
                             inMem: SpatialMemory): SpatialMemory = {
    sf.f.params(0).mem = inMem
    alloc(sf.f.body, outMemT, outAddressSpace)
  }

  private def allocMapSeq(m: MapSeq,
                          outMemT: Type,
                          outAddressSpace: SpatialAddressSpace,
                          inMem: SpatialMemory): SpatialMemory = {
    m.f.params(0).mem = inMem
    alloc(m.f.body, outMemT, outAddressSpace)
  }

  private def allocAbstrSpFold(asf: AbstractSpFold,
                               outMemT: Type,
                               outAddressSpace: SpatialAddressSpace,
                               inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case coll: SpatialMemoryCollection =>
        val initM = coll.subMemories(0)

        asf.fMap.params(0).mem = coll.subMemories(1)
        // fMap body memory has the size of chunkSize. Although fReduce reads data produced by fMap,
        // it expects memory of size call.args(1).t.size. Here, we will allocate output memory of fMap
        // and input memory of fReduce separately. This will not be a problem during code generation as those
        // memories are not explicitly written to / read from. Spatial takes care of the disparity.
        alloc(asf.fMap.body, asf.fMap.body.t, asf.fMap.body.addressSpace)
        // Here, we are doing something potentially dangerous: associate one variable with two memories,
        // the first one referring to the map body memory (containing single tile) and the second one
        // referring to the bigger map memory (containing all tiles)
        asf.fMapMem = SpatialMemory(asf.fMap.body.mem.variable, asf.fFlatMapT,
          asf.fMap.body.mem.asInstanceOf[SpatialMemory].addressSpace)

        asf.fReduce.params(0).mem = initM
        asf.fReduce.params(1).mem = asf.fMapMem

        val reduceBodyM = alloc(asf.fReduce.body, outMemT, initM.addressSpace)

        // replace `bodyM` by `initM` in `asf.fReduce.body`
        Expr.visit(asf.fReduce.body, e => if (e.mem == reduceBodyM) e.mem = initM, _ => {})

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
