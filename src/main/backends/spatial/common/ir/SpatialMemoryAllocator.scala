package backends.spatial.common.ir

import backends.spatial.accel.ir.pattern._
import _root_.ir.ast.{AbstractMap, AbstractSearch, ArrayAccess, ArrayConstructors, ArrayFromExpr, CheckedArrayAccess, Concat, Expr, Filter, FunCall, Gather, Get, Head, Id, Iterate, Join, Lambda, Map, Pad, PadConstant, Param, RewritingGuidePost, Scatter, SkipW, Slide, Split, Tail, Transpose, TransposeW, Tuple, UnsafeArrayAccess, Unzip, UserFun, Value, VectorParam, VectorizeUserFun, Zip, asScalar, asVector, debug}
import _root_.ir.{ArrayType, ArrayTypeWS, ArrayTypeWSWC, Memory, NumberOfArgumentsException, ScalarType, Size, TupleType, Type, TypeException, UnallocatedMemory}

import scala.collection.{mutable, immutable}


object SpatialMemoryAllocator {
  /** innerType => fullType */
  type Allocator = Type => Type

  // Redundant memory collection
  var redundantMemories: mutable.Map[Memory, Memory] = mutable.Map[Memory, Memory]()

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

    val result = alloc(f.body, t => t, DRAMMemory)

    // Verify that the parameter memory types were not changed by primitives such as JoinW
    f.params.foreach(p =>
      if (!p.mem.asInstanceOf[SpatialMemory].t.equals(p.t))
        throw TypeException(f"The type of the memory cannot be changed using primitives such as JoinW -- " +
          f"changes to memory type are only permitted for memories produced by UserFuns.\n" +
        f"The memory with changed type is ${p.mem}; the original type is ${p.t}; " +
          f"the changed type is ${p.mem.asInstanceOf[SpatialMemory].t}"))

    removeRedundantMemory(f, result)
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
    val argAddressSpaces = call.args.zipWithIndex.map {
      case (_, argIdx) =>
        (call.f, outAddressSpace) match {
          case (toDRAM(_), _) => DRAMMemory
          case (toSRAM(_), _) => SRAMMemory
          case (toReg(_), _) => RegMemory
          case (toArgOut(_), _) => ArgOutMemory
          case (Tuple(_), outAScoll: AddressSpaceCollection) => outAScoll.spaces(argIdx)
          case _ => outAddressSpace
        }
    }

    // Get the input memory of f from the input arguments
    val inMem = getInMFromArgs(call, argMemT, argAddressSpaces)

    // Determine the output memory based on the type of f ...
    call.f match {
      // Here is where the actual allocation happens:
      case _: UserFun               => allocUserFun(call, outMemT, outAddressSpace)
      case  _: VectorizeUserFun     => throw new NotImplementedError()

      case Map(_)                   => allocMap(call.f.asInstanceOf[AbstractMap], call, outMemT, outAddressSpace, inMem)

      case sf: AbstractSpForeach    => allocAbstrSpForeach(sf, call, outMemT, outAddressSpace, inMem)
      case m: MapSeq                => allocMapSeq(m, call, outMemT, outAddressSpace, inMem)
      case asf: AbstractSpFold      => allocAbstrSpFold(asf, call, outMemT, outAddressSpace, inMem)
      case r: ReduceSeq             => allocReduceSeq(r, call, outMemT, outAddressSpace, inMem)

      case mapAccum: MapAccumSeq    => allocMapAccumSeq(mapAccum, call, outMemT, outAddressSpace, inMem)

      case s: AbstractSearch        => throw new NotImplementedError()

      case it: Iterate              => throw new NotImplementedError()

      case cc: Concat               => throw new NotImplementedError()

      case sw: SkipW                => allocSkipW(sw, call, outMemT, inMem)

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

      case jw: JoinW                => allocJoinW(jw, call, inMem)

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
                             outAddressSpaces: Seq[SpatialAddressSpace]): SpatialMemory = {
    call.args.length match {
      case 0 => throw new IllegalArgumentException(s"Function call without arguments $call")
      case 1 => alloc(call.args.head, outMemT, outAddressSpaces.head)
      case _ => SpatialMemoryCollection(call.args.zip(outAddressSpaces).map {
        case (arg, outAddressSpace) => alloc(arg, outMemT, outAddressSpace)
      })
    }
  }

  private def allocUserFun(call: FunCall,
                           outMemT: Allocator,
                           outAddressSpace: SpatialAddressSpace): SpatialMemory = {
    if (call.addressSpace == UndefAddressSpace)
      throw new RuntimeException("No address space at " + call)

    SpatialMemory.allocMemory(outMemT(call.t), outAddressSpace)
  }

  private def allocSkipW(sw: SkipW,
                         call: FunCall,
                         outMemT: Allocator,
                         inMem: SpatialMemory): SpatialMemory = {
    // Need to increase the size by the skip distance
    val skipDistance = sw.left + sw.right

    // Find the dimension where the SkipW is applied
    val baseT = Type.getBaseType(inMem.t)
    val outerArrayT = outMemT(baseT)

    /**
     * Unwraps an array by example.
     * The use case is when the SkipW is applied within a Map, i.e. on a subarray of the input array.
     * This function extracts the type of the array whose elements are skipped from a nested array.
     * For example:
     * typeToUnwrap = ArrayType(ArrayType(ArrayType(ArrayType(Float, A), B), C), D)
     * exampleT = ArrayType(ArrayType(Float, E), F)
     * return = ArrayType(ArrayType(Float, A), B)
     * The example type is built using the Allocator, which keeps track of the maps that Skip is nested in.
     */
    def unwrapNDArrayByExample(typeToUnwrap: Type, exampleT: Type): Type = {
      (typeToUnwrap, exampleT) match {
        case (t: ScalarType, _: ScalarType) => t
        case (_: ScalarType, _: ArrayType) =>
          throw new IllegalArgumentException(s"Cannot unwrap $typeToUnwrap by the example of $exampleT")
        case (elemT: ArrayType, _: ScalarType) => elemT
        case (ArrayType(elemToUnwrapT), ArrayType(exampleElemT)) => unwrapNDArrayByExample(elemToUnwrapT, exampleElemT)
        case _ => throw new IllegalArgumentException(f"Unknown combination of ($typeToUnwrap, $exampleT)")
      }
    }

    val nestedArrayT = unwrapNDArrayByExample(inMem.t, outerArrayT)

    val updatedNestedArrayT = nestedArrayT match {
      case ArrayTypeWSWC(elemT, s, c) if s == c =>
        ArrayType(elemT, s + skipDistance)
      case _ =>
        throw TypeException(f"Expected ArrayType(_, s, c) with s == c for input memory $inMem of $sw. Got ${inMem.t}")
    }

    val updatedInMemT = outMemT(updatedNestedArrayT)

    val updatedInMem = SpatialMemory.allocMemory(updatedInMemT, inMem.addressSpace)
    redundantMemories += (inMem -> updatedInMem)

    updatedInMem
  }

  private def allocLambda(l: Lambda,
                          outMemT: Allocator,
                          outAddressSpace: SpatialAddressSpace,
                          inMem: SpatialMemory): SpatialMemory = {
    setMemInParams(l.params, inMem)
    alloc(l.body, outMemT, outAddressSpace)
  }

  private def allocJoinW(jw: JoinW,
                             call: FunCall,
                             inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case SpatialMemory(
          ArrayTypeWSWC(ArrayTypeWSWC(elemT, sInner, cInner), sOuter, cOuter),
          addressSpace, bufferHazard) =>
        val updatedInMem = SpatialMemory.allocMemory(ArrayTypeWSWC(elemT, sOuter * sInner, cOuter * cInner), addressSpace)
        updatedInMem.bufferHazard = bufferHazard

        redundantMemories += (inMem -> updatedInMem)

        updatedInMem

      case mem => throw new IllegalArgumentException(f"Expected a SpatialMemory with type ArrayType(ArrayType(_)). " +
        f"Got $mem with type ${mem.t}")
    }
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

  private def allocAbstrSpForeach(sf: AbstractSpForeach,
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
        // Note, the empty allocator (t => t) is there because any memory fMap allocates is in the local scope of
        // the implicit map, so we reset the memory size multiplier here
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

        if (!reduceBodyM.equalsInShape(initM))
          throw new IllegalStateException(f"Expected initM and reduceBodyM shapes to be equals. " +
            f"Got:\ninitM: $initM\nreduceBodyM: $reduceBodyM")

        // replace `bodyM` by `initM` in `r.f.body`
        // (bodyM will be replaced by initM in removeRedundantMemories())
        redundantMemories ++=
          SpatialMemory.getAllMemories(reduceBodyM).zip(
            SpatialMemory.getAllMemories(initM)).toMap

        initM
      case _ => throw new IllegalArgumentException(inMem.toString)
    }
  }

  private def allocMapAccumSeq(mapAccum: MapAccumSeq,
                               call: FunCall,
                               outMemT: Allocator,
                               outAddressSpace: SpatialAddressSpace,
                               inMem: SpatialMemory): SpatialMemory = {
    inMem match {
      case coll: SpatialMemoryCollection =>
        val initM = coll.subMemories(0)

        initM.bufferHazard = true

        mapAccum.f.params(0).mem = initM
        mapAccum.f.params(1).mem = coll.subMemories(1)

        val outValuesType = call.t.asInstanceOf[TupleType].elemsT(1)
        val outValuesAS = outAddressSpace match {
          case coll: AddressSpaceCollection => coll.spaces(1)
          case _ => outAddressSpace
        }
        val outMem = SpatialMemory.allocMemory(outMemT(outValuesType), outValuesAS)

        val mapAccumOuterMemory = SpatialMemoryCollection(Vector(initM, outMem))

        val mapAccumBodyM: SpatialMemoryCollection = alloc(mapAccum.f.body, t => t, initM.addressSpace) match {
          case aColl: SpatialMemoryCollection => aColl
          case mem => throw new IllegalStateException(f"Expected a collection of memories to be allocated for " +
            f"mapAccumSeq body. Got $mem")
        }

        if (!mapAccumBodyM.subMemories.head.equalsInShape(initM))
          throw new IllegalStateException(f"Expected initM and mapAccumBodyM.head shapes to be equals. " +
            f"Got:\ninitM: $initM\nmapAccumBodyM.head: ${mapAccumBodyM.subMemories.head}")

        // replace `bodyM` by `initM` in `r.f.body`
        // (bodyM will be replaced by initM in removeRedundantMemories())
        redundantMemories ++=
          SpatialMemory.getAllMemories(mapAccumBodyM).zip(
            SpatialMemory.getAllMemories(mapAccumOuterMemory)).toMap

        mapAccumOuterMemory

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

   /**
   * Removes redundant memory.
   * The memory becomes redundant in either of two cases:
   * 1. When the allocated memory needs to be reduced with that of an accumulator of Reduce or Scan;
   * 2. When the memory producer (e.g. a UserFun) is composed into a primitive that changes something
   *    fundamental about memory -- concatenates it with another memory, performs a skip on write or
   *    changes the spatial memory type.
   * This is a more advanced version of opencl.ir.RemoveRedundantMemory, adding the following functionality:
   * - Handling chain replacements of arbitrary order:
   *   (memA -> memB -> memC -> memD) is performed as (memA -> memD, memB -> memD, memC -> memD)
   * - Handling Spatial-specific pattern JoinW
   */
  def removeRedundantMemory(f: Lambda, originalReturnMem: SpatialMemory): SpatialMemory = {
    // 1. Resolve memory replacement chains
    val redundantMemoriesWithoutChains = redundantMemories.foldLeft(mutable.Map[SpatialMemory, SpatialMemory]()) {
      case (replacementMap, (mem1: Memory, mem2: Memory)) =>
        val origMem = mem1.asInstanceOf[SpatialMemory]
        val replMem = mem2.asInstanceOf[SpatialMemory]
        (replacementMap.keys.exists(_ == replMem), replacementMap.values.exists(_ == origMem))  match {
          case (false, false) =>
            replacementMap += (origMem -> replMem)
          case (false, true) =>
            // When adding (a -> b) to [.., c -> a, ..], change the map to [.., c -> b, a -> b, ..]
            replacementMap.filter(_._2 == origMem).foreach(oldRepl =>
              replacementMap.update(key = oldRepl._1, value = replMem))
            replacementMap += (origMem -> replMem)
          case (true, false) =>
            // When adding (a -> b) to [.., b -> c, ..], change the map to [.., b -> c, a -> c, ..]
            replacementMap += (origMem -> replacementMap.find(_._1 == replMem).get._2)
          case (true, true) =>
            // When adding (a -> b) to [.., c -> a, b -> d, ..], change the map to [.., c -> d, b -> d, a -> d, ..]
            replacementMap.filter(_._2 == origMem).foreach(oldRepl =>
              replacementMap.update(key = oldRepl._1, value = replacementMap(replMem)))
            replacementMap += (origMem -> replacementMap(replMem))
        }
    }.toMap

    // 2. apply all replacements
    Expr.visit(f.body, e =>
      e.mem = SpatialMemory.substitute(e.mem.asInstanceOf[SpatialMemory], redundantMemoriesWithoutChains), _ => {})

    SpatialMemory.substitute(originalReturnMem, redundantMemoriesWithoutChains)
  }
}
