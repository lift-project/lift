package backends.spatial.common.ir

import backends.spatial.accel.ir.pattern.{AbstractSpFold, SpFold, SpForeach, SpMemFold}
import ir.{Memory, Type}
import ir.ast.{AbstractMap, AbstractPartRed, ArrayConstructors, ArrayFromExpr, CheckedArrayAccess, Expr, FPattern, FunCall, Iterate, Lambda, Param, UnsafeArrayAccess, UserFun, Value, VectorizeUserFun}

import scala.collection.mutable

final case class ContextualMemoryCollection(inputs: Seq[ContextualSpatialMemory],
                                            outputs: Seq[ContextualSpatialMemory],
                                            intermediates: collection.immutable.Map[
                                              SpatialAddressSpace, Seq[ContextualSpatialMemory]]) {
  lazy val asFlatSequence: Seq[ContextualSpatialMemory] = inputs ++ outputs ++ intermediates.values.flatten

  private lazy val memIndexed: collection.immutable.Map[SpatialMemory, ContextualSpatialMemory] =
    asFlatSequence.map(typedMem => typedMem.mem -> typedMem).toMap

  def apply(mem: Memory): ContextualSpatialMemory = mem match {
    case sMem: SpatialMemory => memIndexed(sMem)
    case m => throw new IllegalArgumentException(s"Expected SpatialMemory. Got $m")
  }

  def contains(mem: SpatialMemory): Boolean = memIndexed.contains(mem)
}

object ContextualMemoryCollection {
  def apply(): ContextualMemoryCollection = ContextualMemoryCollection(Seq(), Seq(), Map())
}

object CollectTypedSpatialMemory {
  /**
   * Function to collect typed Spatial memory objects of the given lambda.
   *
   * @return Memory objects of the (inputs, outputs, intermediates).
   */
  def apply(lambda: Lambda): ContextualMemoryCollection = {
    new CollectTypedSpatialMemory(lambda)()
  }
}

private class CollectTypedSpatialMemory(val lambda: Lambda) {

  private var implicitReadScopes: mutable.Map[Memory, FunCall] = mutable.Map()
  private var implicitWriteScopes: mutable.Map[Memory, FunCall] = mutable.Map()

  private def apply(): ContextualMemoryCollection = {
    val inputs = lambda.params.map(p => ContextualSpatialMemory(p))

    val (intermediates, output) = {
      val memories = distinct(collectIntermediateMemories(lambda.body))

      // Infer the output write type based on intermediate memory
      val output = {
        val outputAmongIntermediates = memories.filter(_.mem == lambda.body.mem)
        if (outputAmongIntermediates.nonEmpty) outputAmongIntermediates.head else ContextualSpatialMemory(lambda.body)
      }

      (List(DRAMMemory, SRAMMemory, RegMemory, LiteralMemory).map(addressSpace =>
        addressSpace -> memories.filter(_.mem.addressSpace == addressSpace)).toMap,
        output)
    }

    val collection = ContextualMemoryCollection(inputs.sortBy(_.mem.variable.name), Seq(output), intermediates)

    // Mark memories that are implicitly read from as such
    collection.asFlatSequence.foreach(tm =>
      if (implicitReadScopes.contains(tm.mem)) tm.implicitReadScope = Some(implicitReadScopes(tm.mem)))
    // Mark memories that are implicitly written to as such
    collection.asFlatSequence.foreach(tm =>
      if (implicitWriteScopes.contains(tm.mem)) tm.implicitWriteScope = Some(implicitWriteScopes(tm.mem)))

    collection
  }

  /**
   *  This prevents collecting multiple memory objects (possibly with different types)
   *  multiple times without changing the order
   */
  private def distinct(memories: Seq[ContextualSpatialMemory]) = {
    val builder = Seq.newBuilder[ContextualSpatialMemory]
    val seen = scala.collection.mutable.HashSet[SpatialMemory]()
    for (m <- memories) {
      if (!seen(m.mem)) {
        builder += m
        seen += m.mem
      }
    }
    builder.result()
  }

  @scala.annotation.tailrec
  private def collectIntermediateMemories(expr: Expr): Seq[ContextualSpatialMemory] = {
    expr match {
      case v: Value               => collectValueOrUserFunMemory(v)
      case _: Param               => Seq()
      case a: ArrayFromExpr       => collectIntermediateMemories(a.e)
      case _: ArrayConstructors   => Seq()
      case call: FunCall          => collectFunCall(call)
    }
  }

  private def collectValueOrUserFunMemory(expr: Expr) = expr.mem match {
    case _: SpatialMemory => Seq(ContextualSpatialMemory(expr))
  }

  private def collectFunCall(call: FunCall) = {
    val argumentMemories = call.args.length match {
      case 0 => Seq()
      case 1 => collectIntermediateMemories(call.args.head)
      case _ => call.args.map(collectIntermediateMemories).reduce(_ ++ _)
    }

    val bodyMemories = call.f match {
      case _: UserFun             => collectValueOrUserFunMemory(call)
      case _: VectorizeUserFun    => throw new NotImplementedError()
      case l: Lambda              => collectIntermediateMemories(l.body)
      case sF: SpForeach          => collectSpForeach(call.t, sF, call)
      case m: AbstractMap         => collectMap(call.t, m)
      case sf: SpFold             => collectSpFold(sf, argumentMemories, call)
      case smf: SpMemFold         => collectSpMemFold(smf, argumentMemories, call)
      case r: AbstractPartRed     => throw new NotImplementedError()
      case _: UnsafeArrayAccess   => Seq(ContextualSpatialMemory(call))
      case _: CheckedArrayAccess  => Seq(ContextualSpatialMemory(call))
      case i: Iterate             => throw new NotImplementedError()
      case fp: FPattern           => collectIntermediateMemories(fp.f.body)
      case _                      => Seq()
    }

    argumentMemories ++ bodyMemories
  }


  private def collectSpForeach(t: Type, sf: SpForeach, call: FunCall) = {
    collectIntermediateMemories(sf.f.body)
  }

  private def collectMap(t: Type, map: AbstractMap) = {
    collectIntermediateMemories(map.f.body)
  }

  private def collectAbstractSpFold(asf: AbstractSpFold,
                                    argumentMemories: Seq[ContextualSpatialMemory],
                                    call: FunCall) = {
    val mapTMem = ContextualSpatialMemory(asf.fMapMem, asf.fFlatMapT,
      implicitReadScope = None, implicitWriteScope = None)
    val foldTMem = ContextualSpatialMemory(call)

    // The input memory of the implicit reduce is a different representation of asf.fMapMem and is not materialised as well

    asf.fReduce.body match {
      case reduceCall: FunCall =>
        // These memories are written to and read from by the reduce implicitly -- we don't need to generate stores/loads
        implicitReadScopes += (asf.fMapMem -> reduceCall)
        implicitReadScopes += (asf.fReduce.body.mem -> reduceCall)
        implicitWriteScopes += (asf.fReduce.body.mem -> reduceCall)
      case _ =>
    }
    implicitWriteScopes += (foldTMem.mem -> call)


    val memories = collectIntermediateMemories(asf.fMap.body) ++ Seq(mapTMem, foldTMem) ++
      collectIntermediateMemories(asf.fReduce.body)

    removeParameterAndArgumentDuplicates(memories, argumentMemories)
  }

  private def collectSpFold(sf: SpFold,
                            argumentMemories: Seq[ContextualSpatialMemory],
                            call: FunCall) = {
    // The memory of the implicit map body is not materialised if a literal is passed as an initial value

    collectAbstractSpFold(sf, argumentMemories, call)
  }

  private def collectSpMemFold(smf: SpMemFold,
                               argumentMemories: Seq[ContextualSpatialMemory],
                               call: FunCall) = {
    collectAbstractSpFold(smf, argumentMemories, call)
  }

  private def removeParameterAndArgumentDuplicates(memories: Seq[ContextualSpatialMemory],
                                                   argumentMemories: Seq[ContextualSpatialMemory]) = {

    memories.filter(m => {
      val isAlreadyInArgs = argumentMemories.exists(_.mem.variable == m.mem.variable)
      val isAlreadyInParams = lambda.params.exists(_.mem.variable == m.mem.variable)

      !isAlreadyInArgs && !isAlreadyInParams
    })
  }
}