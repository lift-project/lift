package opencl.ir

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir.pattern._

object CollectTypedOpenCLMemory {
  /**
    * Function to collect typed OpenCL memory objects of the given lambda.
    *
    * @param includePrivate Should private memories be included in the intermediates.
    * @return Memory objects of the (inputs, outputs, globalIntermediates, localIntermediates).
    */
  def apply(lambda: Lambda, includePrivate: Boolean = false):
  (Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory]) = {
    new CollectTypedOpenCLMemory(lambda, includePrivate)()
  }

  /**
    * Function to collect typed OpenCL memory objects of the given lambda.
    *
    * @param includePrivate Should private memories be included in the intermediates.
    * @return Memory objects in the order: [inputs ++ outputs ++ globalIntermediates ++ localIntermediates]
    */
  def asFlatSequence(lambda: Lambda, includePrivate: Boolean = false): Seq[TypedOpenCLMemory] = {
    val (inputs, outputs, globalIntermediates, localIntermediates) = apply(lambda, includePrivate)
    inputs ++ outputs ++ globalIntermediates ++ localIntermediates
  }
}

private class CollectTypedOpenCLMemory(val lambda: Lambda, val includePrivate: Boolean) {

  private def apply():
  (Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory]) = {
    val inputs = lambda.params.map(TypedOpenCLMemory(_))
    val output = TypedOpenCLMemory(lambda.body)

    val intermediates =
      distinct(collectIntermediateMemories(lambda.body, insideMapLcl = false)).filter(_.mem != output.mem)

    val (localIntermediates, globalIntermediates) =
      intermediates.partition(_.mem.addressSpace == LocalMemory)

    (inputs.sortBy(_.mem.variable.name),
      Seq(output),
      globalIntermediates.sortBy(_.mem.variable.name),
      localIntermediates.sortBy(_.mem.variable.name))
  }

  // this prevents that multiple memory objects (possibly with different types)
  // are collected multiple times without changing the order
  private def distinct(memories: Seq[TypedOpenCLMemory]) = {
    val builder = Seq.newBuilder[TypedOpenCLMemory]
    val seen = scala.collection.mutable.HashSet[OpenCLMemory]()
    for (m <- memories) {
      if (!seen(m.mem)) {
        builder += m
        seen += m.mem
      }
    }
    builder.result()
  }

  private def collectIntermediateMemories(expr: Expr, insideMapLcl: Boolean): Seq[TypedOpenCLMemory] = {
    expr match {
      case v: Value => collectValueOrUserFunMemory(v)
      case _: Param => Seq()
      case a: ArrayFromExpr => collectIntermediateMemories(a.e, insideMapLcl)
      case _: ArrayConstructors => Seq()
      case call: FunCall => collectFunCall(call, insideMapLcl)
    }
  }

  private def collectValueOrUserFunMemory(expr: Expr) = expr.mem match {
    case memory: OpenCLMemory =>
      if (!includePrivate && memory.addressSpace == PrivateMemory)
        Seq()
      else
        Seq(TypedOpenCLMemory(expr))
  }

  private def collectFunCall(call: FunCall, insideMapLcl: Boolean) = {
    val argumentMemories = call.args.length match {
      case 0 => Seq()
      case 1 => collectIntermediateMemories(call.args.head, insideMapLcl)
      case _ => call.args.map(collectIntermediateMemories(_, insideMapLcl)).reduce(_ ++ _)
    }

    val bodyMemories = call.f match {
      case _: UserFun | _: VectorizeUserFun => collectValueOrUserFunMemory(call)
      case l: Lambda              => collectIntermediateMemories(l.body, insideMapLcl)
      case m: AbstractMap         => collectMap(call.t, m, insideMapLcl)
      case f: FilterSeq           => collectIntermediateMemories(f.f.body, insideMapLcl) :+ TypedOpenCLMemory(call)
      case r: AbstractPartRed     => collectReduce(r, argumentMemories, insideMapLcl)
      case sp: MapSeqSlide        => collectMapSeqSlide(sp, argumentMemories, insideMapLcl)
      case mv: MapSeqVector       => collectMapSeqVector(mv, call, argumentMemories, insideMapLcl)
      case s: AbstractSearch      => collectSearch(s, call, argumentMemories, insideMapLcl)
      case s: ScanSeq             => collectScanSeq(s, call, argumentMemories, insideMapLcl)
      case _: UnsafeArrayAccess   => Seq(TypedOpenCLMemory(call))
      case _: CheckedArrayAccess  => Seq(TypedOpenCLMemory(call))
      case i: Iterate             => collectIterate(call, i, insideMapLcl)
      case fp: FPattern           => collectIntermediateMemories(fp.f.body, insideMapLcl)
      case _                      => Seq()
    }

    argumentMemories ++ bodyMemories
  }

  private def collectMap(t: Type, map: AbstractMap, insideMapLcl: Boolean) = {
    val memories = collectIntermediateMemories(map.f.body, insideMapLcl = insideMapLcl || map.isInstanceOf[MapLcl])

    @scala.annotation.tailrec
    def changeType(addressSpace: OpenCLAddressSpace, tm: TypedOpenCLMemory): TypedOpenCLMemory = {
      // TODO: This might return one of two types in case of reduce (T or Array(T, 1))
      addressSpace match {
        case PrivateMemory =>
          map match {
            case _: MapGlb | _: MapWrg  | _: Map =>
              tm
            case _: MapLcl | _: MapWarp | _: MapLane | _: MapSeq =>

              val privateMultiplier =
                if (map.iterationCount == ?) Cst(1) else map.iterationCount

              TypedOpenCLMemory(tm.mem, ArrayTypeWSWC(tm.t, privateMultiplier))
          }
        case LocalMemory =>
          map match {
            case _: MapGlb | _: MapWrg  | _: Map =>
              tm
            case _: MapSeq if !insideMapLcl =>
              tm
            case _: MapLcl | _: MapWarp | _: MapLane | _: MapSeq =>
              val newType = t.asInstanceOf[ArrayType].replacedElemT(tm.t)
              TypedOpenCLMemory(tm.mem, newType)
          }
        case GlobalMemory =>
          val newType = t.asInstanceOf[ArrayType].replacedElemT(tm.t)
          TypedOpenCLMemory(tm.mem, newType)

        case coll: AddressSpaceCollection =>
          changeType(coll.findCommonAddressSpace(), tm)

        case UndefAddressSpace =>
          throw new MemoryAllocationException("Address space must be known at this point")
      }
    }

    val cts = memories.map(tm => changeType(tm.mem.addressSpace, tm) )

    // TODO: Think about other ways of refactoring this out
    map match {
      case aw : MapAtomWrg =>
        cts :+ TypedOpenCLMemory(aw.globalTaskIndex, ArrayTypeWSWC(Int, Cst(1)))
      case _ => cts
    }

  }

  private def collectReduce(
    reduce: AbstractPartRed,
    argumentMemories: Seq[TypedOpenCLMemory],
    insideMapLcl: Boolean) = {

    val memories = collectIntermediateMemories(reduce.f.body, insideMapLcl) ++ (reduce match {
      case rws: ReduceWhileSeq => collectIntermediateMemories(rws.p.body, insideMapLcl)
      case _ => Seq[TypedOpenCLMemory]()
    })

    removeParameterAndArgumentDuplicates(memories, argumentMemories)
  }

  private def collectMapSeqSlide(
    slide: MapSeqSlide,
    argumentMemories: Seq[TypedOpenCLMemory],
    insideMapLcl: Boolean) = {

    val memories = collectIntermediateMemories(slide.f.body, insideMapLcl)

    removeParameterAndArgumentDuplicates(memories, argumentMemories)
  }

  private def collectMapSeqVector(mv: MapSeqVector,
                                  call: FunCall,
                                  argumentMemories: Seq[TypedOpenCLMemory],
                                  insideMapLcl: Boolean) = {
    assert(mv.outTScalarPart.isDefined)
    val memories = {
      // Vector loop intermediate memories
      if (mv.vectorPartNonEmpty)
        collectIntermediateMemories(mv.fVectorized.body, insideMapLcl)
      else Seq()
    } ++ {
      // Final memory, which is also used in the scalar loop
      val result =
        if (!includePrivate && call.addressSpace == PrivateMemory)
          Seq()
        else
          Seq(TypedOpenCLMemory(call.mem, call.t))
      result
    }

    removeParameterAndArgumentDuplicates(memories, argumentMemories)
  }

  private def collectSearch(
    search: AbstractSearch,
    call:FunCall,
    argumentMemories: Seq[TypedOpenCLMemory],
    insideMapLcl: Boolean) = {

    val memories = collectIntermediateMemories(search.f.body, insideMapLcl)

    // TODO: Optimise so we use the default value instead of more allocated memory!
    TypedOpenCLMemory(call) +:
      removeParameterAndArgumentDuplicates(memories, argumentMemories)
  }

  private def collectScanSeq(
                             scan: ScanSeq,
                             call:FunCall,
                             argumentMemories: Seq[TypedOpenCLMemory],
                             insideMapLcl: Boolean) = {

    val memories = collectIntermediateMemories(scan.f.body, insideMapLcl)

    // TODO: Optimise so we use the default value instead of more allocated memory!
    TypedOpenCLMemory(call) +:
      removeParameterAndArgumentDuplicates(memories, argumentMemories)
  }

  private def removeParameterAndArgumentDuplicates(
    memories: Seq[TypedOpenCLMemory],
    argumentMemories: Seq[TypedOpenCLMemory]) = {

    memories.filter(m => {
      val isAlreadyInArgs = argumentMemories.exists(_.mem.variable == m.mem.variable)
      val isAlreadyInParams = lambda.params.exists(_.mem.variable == m.mem.variable)

      !isAlreadyInArgs && !isAlreadyInParams
    })
  }

  private def collectIterate(call: FunCall, iterate: Iterate, insideMapLcl: Boolean) = {

    val intermediateMemories = collectIntermediateMemories(iterate.f.body, insideMapLcl)
    iterate.swapBuffer match {
      case UnallocatedMemory => intermediateMemories
      case _ =>
        val swapBufferMemory =
          TypedOpenCLMemory(iterate.swapBuffer, ArrayTypeWSWC(call.args.head.t, ?))

        swapBufferMemory +: intermediateMemories
    }
  }

}
