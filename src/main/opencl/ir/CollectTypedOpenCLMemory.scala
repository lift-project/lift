package opencl.ir

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir.pattern._

object CollectTypedOpenCLMemory {
  /**
    * Function to collect typed OpenCL memory objects of the given lambda.
    * Returns a tuple with three components:
    * the first component are the memory objects for the given parameters -- in the same order;
    * the second component are output memory objects;
    * the third component are temporary memory objects.
    *
    * @param f The lambda from which memory objects are collected.
    * @param includePrivate indicate if private memories should be included in the intermediates.
    * @return A tuple with the memory objects of the (inputs, intermediates, outputs).
    */
  def apply(f: Lambda,
    includePrivate: Boolean = false):
  (Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory]) = {
    new CollectTypedOpenCLMemory(f, includePrivate)()
  }

  /**
    * Function to collect typed OpenCL memory objects of the given lambda.
    * Returns a flat sequence with the memory objects in the order: [inputs ++ outputs ++ intermediates]
    *
    * @param f The lambda from which memory objects are collected.
    * @param includePrivate indicate if private memories should be included in the intermediates.
    * @return A flat sequence with the memory objects in the order: [inputs ++ outputs ++ intermediates]
    */
  def asFlatSequence(f: Lambda, includePrivate: Boolean = false): Seq[TypedOpenCLMemory] = {
    val (inputs, outputs, globalIntermediates, localIntermediates) = apply(f, includePrivate)
    inputs ++ outputs ++ globalIntermediates ++ localIntermediates
  }
}

private class CollectTypedOpenCLMemory(val f: Lambda, val includePrivate: Boolean) {

  private def apply():
  (Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory]) = {
    val inputs = f.params.map(TypedOpenCLMemory(_))
    val output = TypedOpenCLMemory(f.body)

    val intermediates =
      distinct(collectIntermediateMemories(f.body)).filter(_.mem != output.mem)

    val (localIntermediates, globalIntermediates) =
      intermediates.partition(_.mem.addressSpace == LocalMemory)

    (inputs, Seq(output), globalIntermediates, localIntermediates)
  }

  // this prevents that multiple memory objects (possibly with different types)
  // are collected multiple times without changing the order
  private def distinct(seq: Seq[TypedOpenCLMemory]) = {
    val builder = Seq.newBuilder[TypedOpenCLMemory]
    val seen = scala.collection.mutable.HashSet[OpenCLMemory]()
    for (x <- seq) {
      if (!seen(x.mem)) {
        builder += x
        seen += x.mem
      }
    }
    builder.result()
  }

  private def collectIntermediateMemories(expr: Expr): Seq[TypedOpenCLMemory] = {
    expr match {
      case v: Value =>
        if (includePrivate) {
          Seq(TypedOpenCLMemory(v))
        } else {
          Seq()
        }
      case _: Param => Seq()
      case _: ArrayConstructors => Seq()
      case call: FunCall => collectFunCall(call)
    }
  }

  private def collectFunCall(call: FunCall): Seq[TypedOpenCLMemory] = {
    val argMemories: Seq[TypedOpenCLMemory] = call.args.length match {
      case 0 => Seq()
      case 1 => collectIntermediateMemories(call.args.head)
      case _ => call.args.map(collectIntermediateMemories).reduce(_ ++ _)
    }

    val bodyMemories = call.f match {
      case _: UserFun | _: VectorizeUserFun => collectUserFun(call)
      case l: Lambda              => collectIntermediateMemories(l.body)
      case m: AbstractMap         => collectMap(call.t, m)
      case f: FilterSeq           => collectIntermediateMemories(f.f.body) :+ TypedOpenCLMemory(call)
      case r: AbstractPartRed     => collectReduce(r, argMemories)
      case sp: MapSeqSlide        => collectMapSeqSlide(sp, argMemories)
      case s: AbstractSearch      => collectSearch(s, call, argMemories)
      case _: UnsafeArrayAccess   => Seq(TypedOpenCLMemory(call))
      case _: CheckedArrayAccess  => Seq(TypedOpenCLMemory(call))
      case i: Iterate             => collectIterate(call, i)
      case fp: FPattern           => collectIntermediateMemories(fp.f.body)
      case _                      => Seq()
    }

    argMemories ++ bodyMemories
  }

  private def collectUserFun(call: FunCall): Seq[TypedOpenCLMemory] = call.mem match {
    case m: OpenCLMemory =>
      if (!includePrivate && m.addressSpace == PrivateMemory) {
        Seq()
      } else {
        Seq(TypedOpenCLMemory(call))
      }
  }

  private def collectMap(t: Type,
    m: AbstractMap): Seq[TypedOpenCLMemory] = {
    val mems = collectIntermediateMemories(m.f.body)

    @scala.annotation.tailrec
    def changeType(addressSpace: OpenCLAddressSpace,
      tm: TypedOpenCLMemory): TypedOpenCLMemory = {
      // TODO: This might return one of two types in case of reduce (T or Array(T, 1))
      addressSpace match {
        case PrivateMemory =>
          m match {
            case _: MapGlb | _: MapWrg  | _: Map =>
              tm
            case _: MapLcl | _: MapWarp | _: MapLane | _: MapSeq =>

              val privateMultiplier = if (m.iterationCount == ?) Cst(1) else m.iterationCount

              TypedOpenCLMemory(tm.mem, ArrayTypeWSWC(tm.t, privateMultiplier))
          }
        case LocalMemory =>
          m match {
            case _: MapGlb | _: MapWrg  | _: Map =>
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

    // change types for all of them
    val cts = mems.map( (tm: TypedOpenCLMemory) => changeType(tm.mem.addressSpace, tm) )

    // TODO: Think about other ways of refactoring this out
    m match {
      case aw : MapAtomWrg =>
        cts :+ TypedOpenCLMemory(aw.globalTaskIndex, ArrayTypeWSWC(Int, Cst(1)))
      case _ => cts
    }

  }

  private def collectReduce(r: AbstractPartRed,
    argMems: Seq[TypedOpenCLMemory]): Seq[TypedOpenCLMemory] = {
    val mems: Seq[TypedOpenCLMemory] = collectIntermediateMemories(r.f.body) ++ (r match {
      case rws: ReduceWhileSeq => collectIntermediateMemories(rws.p.body)
      case _ => Seq[TypedOpenCLMemory]()
    })

    mems.filter(m => {
      val isAlreadyInArgs   = argMems.exists(_.mem.variable == m.mem.variable)
      val isAlreadyInParams =  f.params.exists(_.mem.variable == m.mem.variable)

      !isAlreadyInArgs && !isAlreadyInParams
    })
  }

  private def collectMapSeqSlide(sp: MapSeqSlide,
    argMems: Seq[TypedOpenCLMemory]): Seq[TypedOpenCLMemory] = {
    val mems: Seq[TypedOpenCLMemory] = collectIntermediateMemories(sp.f.body) ++ Seq[TypedOpenCLMemory]()

    mems.filter(m => {
      val isAlreadyInArgs   = argMems.exists(_.mem.variable == m.mem.variable)
      val isAlreadyInParams =  f.params.exists(_.mem.variable == m.mem.variable)

      !isAlreadyInArgs && !isAlreadyInParams
    })
  }

  private def collectSearch(s: AbstractSearch, call:FunCall, argMems: Seq[TypedOpenCLMemory]): Seq[TypedOpenCLMemory] = {
    val mems = collectIntermediateMemories(s.f.body)

    // TODO: Optimise so we use the default value instead of more allocated memory!
    TypedOpenCLMemory(call) +: mems.filter(m => {
      val isAlreadyInArgs   = argMems.exists(_.mem.variable == m.mem.variable)
      val isAlreadyInParams =  f.params.exists(_.mem.variable == m.mem.variable)

      !isAlreadyInArgs && !isAlreadyInParams
    })
  }

  private def collectIterate(call: FunCall, i: Iterate): Seq[TypedOpenCLMemory] = {
    i.swapBuffer match {
      case UnallocatedMemory => collectIntermediateMemories(i.f.body)
      case _ =>
        TypedOpenCLMemory(i.swapBuffer, ArrayTypeWSWC(call.args.head.t, ?)) +: collectIntermediateMemories(i.f.body)
    }
  }

}
