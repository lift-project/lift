package backends.spatial.common.ir

import backends.spatial.accel.ir.pattern.{AbstractSpFold, SpForeach}
import ir.Type
import ir.ast.{AbstractMap, AbstractPartRed, ArrayConstructors, ArrayFromExpr, CheckedArrayAccess, Expr, FPattern, FunCall, Iterate, Lambda, Param, UnsafeArrayAccess, UserFun, Value, VectorizeUserFun}
import lift.arithmetic.Var

final case class TypedMemoryCollection(inputs: Seq[TypedSpatialMemory],
                                       outputs: Seq[TypedSpatialMemory],
                                       intermediates: collection.immutable.Map[
                                         SpatialAddressSpace, Seq[TypedSpatialMemory]]) {
  lazy val varMems: collection.immutable.Map[Var, Type] =
    (inputs ++ outputs ++ intermediates.values.flatten).map(typedMem => typedMem.mem.variable -> typedMem.t).toMap
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

  private def apply(): TypedMemoryCollection= {
    val inputs = lambda.params.map(TypedSpatialMemory(_))
    val output = TypedSpatialMemory(lambda.body)

    val intermediates = {
      val memories = distinct(collectIntermediateMemories(lambda.body)).filter(_.mem != output.mem)

      List(DRAMMemory, SRAMMemory, RegMemory).map(addressSpace =>
        addressSpace -> memories.filter(_.mem.addressSpace == addressSpace)).toMap
    }

    TypedMemoryCollection(inputs.sortBy(_.mem.variable.name), Seq(output), intermediates)
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
      case v: Value => collectValueOrUserFunMemory(v)
      case _: Param => Seq()
      case a: ArrayFromExpr => collectIntermediateMemories(a.e)
      case _: ArrayConstructors => Seq()
      case call: FunCall => collectFunCall(call)
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
      case sF: SpForeach          => collectSpForeach(call.t, sF)
      case m: AbstractMap         => throw new NotImplementedError()
      case aSF: AbstractSpFold    => collectSpFold(aSF, argumentMemories)
      case r: AbstractPartRed     => throw new NotImplementedError()
      case _: UnsafeArrayAccess   => Seq(TypedSpatialMemory(call))
      case _: CheckedArrayAccess  => Seq(TypedSpatialMemory(call))
      case i: Iterate             => throw new NotImplementedError()
      case fp: FPattern           => collectIntermediateMemories(fp.f.body)
      case _                      => Seq()
    }

    argumentMemories ++ bodyMemories
  }


  private def collectSpForeach(t: Type, sF: SpForeach) = {
    collectIntermediateMemories(sF.f.body)
  }
  
  private def collectSpFold(aSF: AbstractSpFold,
                            argumentMemories: Seq[TypedSpatialMemory]) = {

    val memories = collectIntermediateMemories(aSF.fMap.body) ++ collectIntermediateMemories(aSF.fReduce.body)

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