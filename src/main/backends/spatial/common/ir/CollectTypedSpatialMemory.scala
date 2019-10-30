package backends.spatial.common.ir

import backends.spatial.accel.ir.pattern.{AbstractSpFold, SpForeach}
import ir.{Memory, Type}
import ir.ast.{AbstractMap, AbstractPartRed, ArrayConstructors, ArrayFromExpr, CheckedArrayAccess, Expr, FPattern, FunCall, Iterate, Lambda, Param, UnsafeArrayAccess, UserFun, Value, VectorizeUserFun}
import lift.arithmetic.Var

import scala.collection.mutable

final case class TypedMemoryCollection(inputs: Seq[TypedSpatialMemory],
                                       outputs: Seq[TypedSpatialMemory],
                                       intermediates: collection.immutable.Map[
                                         SpatialAddressSpace, Seq[TypedSpatialMemory]]) {
  lazy val asFlatSequence: Seq[TypedSpatialMemory] = inputs ++ outputs ++ intermediates.values.flatten

  private lazy val varIndexed: collection.immutable.Map[Var, TypedSpatialMemory] =
    asFlatSequence.map(typedMem => typedMem.mem.variable -> typedMem).toMap

  private lazy val memIndexed: collection.immutable.Map[SpatialMemory, TypedSpatialMemory] =
    asFlatSequence.map(typedMem => typedMem.mem -> typedMem).toMap

  def apply(variable: Var): TypedSpatialMemory = varIndexed(variable)

  def apply(mem: Memory): TypedSpatialMemory = mem match {
    case sMem: SpatialMemory => memIndexed(sMem)
    case m => throw new IllegalArgumentException(s"Expected SpatialMemory. Got $m")
  }

  def contains(mem: SpatialMemory): Boolean = memIndexed.contains(mem)
}

object TypedMemoryCollection {
  def apply(): TypedMemoryCollection = TypedMemoryCollection(Seq(), Seq(), Map())
}

object CollectTypedSpatialMemory {
  /**
   * Function to collect typed Spatial memory objects of the given lambda.
   *
   * @return Memory objects of the (inputs, outputs, intermediates).
   */
  def apply(lambda: Lambda): TypedMemoryCollection = {
    new CollectTypedSpatialMemory(lambda)()
  }
}

private class CollectTypedSpatialMemory(val lambda: Lambda) {

  private var nonMaterialMems: mutable.Set[Memory] = mutable.Set.empty
  private var implicitlyReadFromMems: mutable.Set[Memory] = mutable.Set.empty
  private var implicitWriteScopes: mutable.Map[Memory, FunCall] = mutable.Map()

  private def apply(): TypedMemoryCollection = {
    val inputs = lambda.params.map(p => TypedSpatialMemory(p))

    val (intermediates, output) = {
      val memories = distinct(collectIntermediateMemories(lambda.body))

      // Infer the output write type based on intermediate memory
      val output = {
        val outputAmongIntermediates = memories.filter(_.mem == lambda.body.mem)
        if (outputAmongIntermediates.nonEmpty) outputAmongIntermediates.head else TypedSpatialMemory(lambda.body)
      }

      (List(DRAMMemory, SRAMMemory, RegMemory, LiteralMemory).map(addressSpace =>
        addressSpace -> memories.filter(_.mem.addressSpace == addressSpace)).toMap,
        output)
    }

    val collection = TypedMemoryCollection(inputs.sortBy(_.mem.variable.name), Seq(output), intermediates)

    // Mark memories that don't need materialisation as such
    collection.asFlatSequence.foreach(tm => if (nonMaterialMems.contains(tm.mem)) tm.materialised = false)
    // Mark memories that are implicitly read from as such
    collection.asFlatSequence.foreach(tm => if (implicitlyReadFromMems.contains(tm.mem)) tm.implicitlyReadFrom = true)
    // Mark memories that are implicitly written to as such
    collection.asFlatSequence.foreach(tm =>
      if (implicitWriteScopes.contains(tm.mem))
        tm.implicitWriteScope = Some(implicitWriteScopes(tm.mem)))

    collection
  }

  /**
   *  This prevents collecting multiple memory objects (possibly with different types)
   *  multiple times without changing the order
   */
  private def distinct(memories: Seq[TypedSpatialMemory]) = {
    val builder = Seq.newBuilder[TypedSpatialMemory]
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
  private def collectIntermediateMemories(expr: Expr): Seq[TypedSpatialMemory] = {
    expr match {
      case v: Value               => collectValueOrUserFunMemory(v)
      case _: Param               => Seq()
      case a: ArrayFromExpr       => collectIntermediateMemories(a.e)
      case _: ArrayConstructors   => Seq()
      case call: FunCall          => collectFunCall(call)
    }
  }

  private def collectValueOrUserFunMemory(expr: Expr) = expr.mem match {
    case _: SpatialMemory => Seq(TypedSpatialMemory(expr))
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
      case aSF: AbstractSpFold    => collectSpFold(aSF, argumentMemories, call)
      case r: AbstractPartRed     => throw new NotImplementedError()
      case _: UnsafeArrayAccess   => Seq(TypedSpatialMemory(call))
      case _: CheckedArrayAccess  => Seq(TypedSpatialMemory(call))
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
  
  private def collectSpFold(asf: AbstractSpFold,
                            argumentMemories: Seq[TypedSpatialMemory],
                            call: FunCall) = {
    // The memory of the implicit map is not materialised if a literal is passed as an initial value
    if (call.args.head.addressSpace == LiteralMemory)
      nonMaterialMems += asf.fMapMem
    // The input memory of the implicit reduce is a different representation of asf.fMapMem and is not materialised as well
    val fReduceInputTypedFakeMem = TypedSpatialMemory(asf.fReduce.params(1))
    nonMaterialMems += fReduceInputTypedFakeMem.mem
//     The memory written to by the reduce is to be materialised even if it marked as non-material before
    nonMaterialMems -= call.mem
    // The memory is written to and read from by the reduce implicitly -- we don't need to generate stores/loads
    implicitlyReadFromMems += call.mem
    implicitWriteScopes += (call.mem -> call)


    val memories = collectIntermediateMemories(asf.fMap.body) ++ Seq(fReduceInputTypedFakeMem) ++
      collectIntermediateMemories(asf.fReduce.body)

    removeParameterAndArgumentDuplicates(memories, argumentMemories)
  }

  private def removeParameterAndArgumentDuplicates(memories: Seq[TypedSpatialMemory],
                                                   argumentMemories: Seq[TypedSpatialMemory]) = {

    memories.filter(m => {
      val isAlreadyInArgs = argumentMemories.exists(_.mem.variable == m.mem.variable)
      val isAlreadyInParams = lambda.params.exists(_.mem.variable == m.mem.variable)

      !isAlreadyInArgs && !isAlreadyInParams
    })
  }
}