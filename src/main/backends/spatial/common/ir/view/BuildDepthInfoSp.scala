package backends.spatial.common.ir.view

import backends.common.view._
import backends.spatial.accel.ir.pattern.{AbstractSpFold, SpForeach}
import backends.spatial.common.ir._
import ir.ast.{AbstractMap, Expr, FPattern, FunCall, Get, Lambda, Param, UserFun}
import ir.{ArrayType, Memory, Type}
import lift.arithmetic.{ArithExpr, Cst, Var}

object MemoryAccessInfoSp {
  def apply(): collection.mutable.ListMap[Memory, List[SingleAccess]] = collection.mutable.ListMap()
}

class AccessInfoSp(var accessInf: scala.collection.mutable.ListMap[Memory, List[SingleAccess]],
                   var collection: Seq[AccessInfoSp]) extends AccessInfo {

  override def toString = s"AccessInfoSp($accessInf, $collection)"

  def apply(newAccess: SingleAccess, usedMemories: Seq[Memory]): AccessInfoSp = {
    if (collection.isEmpty) {
      AccessInfoSp(accessInf.map {
        case (mem: Memory, accessInfo: List[SingleAccess]) =>
          if (usedMemories.contains(mem))
          // TODO: verify ordering of accessInfo
            mem -> (newAccess :: accessInfo)
          else
            mem -> accessInfo
      })
    } else {
      AccessInfoSp(collection.map(aInf => AccessInfoSp(aInf.accessInf.map {
        case (mem: Memory, accessInfo: List[SingleAccess]) =>
          if (usedMemories.contains(mem))
          // TODO: verify ordering of accessInfo
            mem -> (newAccess :: accessInfo)
          else
            mem -> accessInfo
      })))
    }
  }
}

object AccessInfoSp {

  def apply() = new AccessInfoSp(accessInf = MemoryAccessInfoSp(), collection = Seq())
  def apply(accessInf: collection.mutable.ListMap[Memory, List[SingleAccess]]) = new AccessInfoSp(accessInf, Seq())
  def apply(collection: Seq[AccessInfoSp]) = new AccessInfoSp(MemoryAccessInfoSp(), collection)
}

/**
 * Helper object for building views.
 *
 * Determines dimensionality and length of each dimension of arrays in all memories.
 *
 * The essential difference to the OpenCL-specific BuildDepthInfoCL is in scoping. The OpenCL version
 * maintains a list of accesses per address space globally, growing them while traversing the AST top-down.
 * Once a memory is found, accesses from all outer loops are taken into account. TODO: confirm
 * For Spatial, we maintain a list of memories and accumulate accesses for each memory separately.
 * When collecting accesses to a memory, we take into account only the accesses made using iteration
 * variables of loops that are inside the scope of the memory. The loops outside the scope do not produce accesses.
 * Consider the following example:
 * Foreach(..) { i =>
 *    val a = SRAM(N)
 *    Foreach(N) { j =>
 *     f(a(j)) } }
 * In Spatial, we would collect only the access made using variable "j". In OpenCL, "a" would be global
 * to the whole kernel and both "i" and "j" would be recorded as accesses.
 */
object BuildDepthInfoSp {

  /**
   * Determine the dimensions, variables and lengths.
   *
   * @param expr Starting expression.
   */
  def apply(expr: Expr): Unit = {
    (new BuildDepthInfoSp)visitAndBuildDepthInfo(expr)
  }
}

private class BuildDepthInfoSp() {

  var memoryAccessInfo: collection.mutable.ListMap[Memory, List[SingleAccess]] = MemoryAccessInfoSp()

  private def visitAndBuildDepthInfo(expr: Expr): AccessInfoSp = {
    val result = expr match {
      case call: FunCall => buildDepthInfoFunCall(call)
      case e: Expr =>
        e.inputDepth = getAccessInfo(Seq(expr.mem))
        e.accessInf.asInstanceOf[AccessInfoSp]
    }

    expr.accessInf = result
    result
  }

  private def buildDepthInfoFunCall(call: FunCall): AccessInfoSp = {

    val argInf = buildDepthForArgs(call)

    val result = call.f match {
      case sF: SpForeach        => buildDepthInfoSpForeach(sF, call, argInf)
      case m: AbstractMap       => buildDepthInfoMapCall(m, call, argInf)
      case aSF: AbstractSpFold  => buildDepthInfoSpFold(aSF, call, argInf)
      case _                    =>

        val readMemories = getMemoryAccesses(call.args.head)
        val writeMemories = getMemoryAccesses(call)

        setDepths(call, readMemories, writeMemories)

        call.f match {
          case l: Lambda            =>  buildDepthInfoLambda(l, call, argInf)
          case fp: FPattern         =>  buildDepthInfoLambda(fp.f, call, argInf)
          case Get(n)               =>  if (argInf.collection.nonEmpty) argInf.collection(n) else argInf
          case _: UserFun           =>  AccessInfoSp(memoryAccessInfo)
          case _                    =>  argInf
        }
    }

    result
  }

  private def buildDepthInfoSpForeach(sF: SpForeach, call: FunCall,
                                      l: AccessInfoSp): AccessInfoSp = {
    buildDepthInfoMapLikeCall(sF.f, sF.loopVar, call, l)
  }

  private def buildDepthInfoMapCall(m: AbstractMap, call: FunCall,
                                    l: AccessInfoSp): AccessInfoSp = {
    buildDepthInfoMapLikeCall(m.f, m.loopVar, call, l)
  }

  private def buildDepthInfoMapLikeCall(f: Lambda, loopVar: Var, call: FunCall,
                                        l: AccessInfoSp): AccessInfoSp = {
    val readMemories = getMemoryAccesses(call.args.head)

    val inf = getArrayAccessInf(call.args.head.t, loopVar)
    f.params.head.accessInf = l(inf, readMemories)
    buildDepthInfoPatternCall(f.body, call, loopVar, readMemories)

    if (f.body.isConcrete) // create fresh input view for following function
      AccessInfoSp(memoryAccessInfo)
    else // call.isAbstract, return input
      l
  }

  /**
   * To be checked for correctness
   */
  private def buildDepthInfoSpFold(aSF: AbstractSpFold, call: FunCall,
                                   l: AccessInfoSp): AccessInfoSp = {

    val mapReadMemories = getMemoryAccesses(call.args(1))

    // Map depth info
    val mapArgAccessInfo = getArrayAccessInf(call.args(1).t, aSF.mapLoopVar)
    aSF.fMap.params.head.accessInf = l.collection(1)(mapArgAccessInfo, mapReadMemories)
    buildDepthInfoPatternCall(aSF.fMap.body, call, aSF.mapLoopVar, mapReadMemories)

    val fMapAccessInfo = if (aSF.fMap.body.isConcrete) // create fresh input view for following function
      AccessInfoSp(memoryAccessInfo)
    else // call.isAbstract, return input
      l

    val mapType = call.args(1).t
    val mapWriteMemories = getMemoryAccesses(aSF.fMap.body)

    // Reduce depth info
    val reduceReadMemories = getMemoryAccesses(call.args(1))
    val reduceArgAccessInfo = getArrayAccessInf(mapType, aSF.reduceLoopVar)
    aSF.fReduce.params(0).accessInf = l.collection.head
    aSF.fReduce.params(1).accessInf = fMapAccessInfo(reduceArgAccessInfo, mapWriteMemories)

    val accumAccessInf = getArrayAccessInf(call.t, Cst(0))
    val reduceWriteMemories = getMemoryAccesses(call)

    updateAccessInf(reduceReadMemories ++ reduceWriteMemories, accumAccessInf)
    // traverse into call.f
    visitAndBuildDepthInfo(aSF.fReduce.body)

    setDepths(call, reduceReadMemories, reduceWriteMemories)

    restoreAccessInf(reduceReadMemories ++ reduceWriteMemories)

    AccessInfoSp(memoryAccessInfo)
  }

  private def getMemoryAccesses(expr: Expr) =
    SpatialMemory.getAllMemories(expr.mem.asInstanceOf[SpatialMemory])

  private def buildDepthInfoPatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                        readMemories: Seq[Memory]): Unit = {
    val inf = getArrayAccessInf(call.t, index)
    val writeMemories = getMemoryAccesses(call)

    updateAccessInf(readMemories ++ writeMemories, inf)

    // traverse into call.f
    visitAndBuildDepthInfo(expr)

    setDepths(call, readMemories, writeMemories)

    restoreAccessInf(readMemories ++ writeMemories)

    AccessInfoSp(memoryAccessInfo)
  }

  private def buildDepthForArgs(call: FunCall): AccessInfoSp = {

    if (call.args.length == 1)
      visitAndBuildDepthInfo(call.args.head)
    else {
      AccessInfoSp({
        val a = call.args.map((expr: Expr) => visitAndBuildDepthInfo(expr))
        a
      })
    }
  }

  private def buildDepthInfoLambda(l: Lambda, call: FunCall,
                                   list: AccessInfoSp): AccessInfoSp = {

    if (call.args.length == 1)
      setAccessInfo(list, l.params.head)
    else
      (list.collection, l.params).zipped.foreach((accessInfo, param) =>
        setAccessInfo(accessInfo, param))

    visitAndBuildDepthInfo(l.body)
  }

  /**
   * For new memories, adds them to the memoryAccessInfo collection with one access.
   * For memories already in memoryAccessInfo, adds the new access to the list of existing ones
   * @param accessedMemories
   * @param newAccess
   */
  protected def updateAccessInf(accessedMemories: Seq[Memory],
                                newAccess: SingleAccess): Unit = {
    // New memories
    accessedMemories.foreach(mem =>
      if (memoryAccessInfo.contains(mem)) memoryAccessInfo(mem) ::= newAccess
      else memoryAccessInfo += (mem -> List(newAccess)))
    // Existing memories
    memoryAccessInfo.filterNot{ case (m, _) => accessedMemories.contains(m) }.
      foreach { case (mem, _) => memoryAccessInfo(mem) ::= newAccess }
  }

  protected def restoreAccessInf(accessedMemories: Seq[Memory]): Unit = {
    accessedMemories.foreach(mem => memoryAccessInfo(mem) = memoryAccessInfo(mem).tail)
  }

  protected def setDepths(call: FunCall, readMemories: Seq[Memory], writeMemories: Seq[Memory]): Unit = {
    call.inputDepth = getAccessInfo(writeMemories)
    call.outputDepth = getAccessInfo(readMemories)
  }

  protected def setAccessInfo(list: AccessInfoSp, param: Param): Unit = {
    val readMemory = getMemoryAccesses(param)

    param.outputDepth = getAccessInfo(readMemory)
    param.accessInf = list
  }

  /**
   * Choose memories out of the list of accessed memories based on the address space
   * priority rules (TODO: explain priority) and return the access chain of the memory
   * with the deepest access chain out of the filtered memories
   */
  protected def getAccessInfo(accessedMemories: Seq[Memory]): List[SingleAccess] = {
    val spAccessedMemories = accessedMemories.map(_.asInstanceOf[SpatialMemory])

    val accessedRegisterMem = spAccessedMemories.filter(SpatialMemory.containsRegMemory)
    val accessedSRAMMem = spAccessedMemories.filter(SpatialMemory.containsSRAMMemory)
    val accessedDRAMMem = spAccessedMemories.filter(SpatialMemory.containsDRAMMemory)

    // Find the memory with deepest access level. Return its accesses
    val accessedMemoriesOfInterest =
      if (accessedRegisterMem.nonEmpty) accessedRegisterMem
      else if (accessedSRAMMem.nonEmpty) accessedSRAMMem
      else if (accessedDRAMMem.nonEmpty) accessedDRAMMem
      else throw new IllegalArgumentException("Unknown or no memory")

    val accessesOfInterest = accessedMemoriesOfInterest.flatMap(mem =>
      if (memoryAccessInfo.contains(mem)) Some(memoryAccessInfo(mem)) else None)

    // Return the deepest access
    accessesOfInterest.toList.sortWith((l, r) => l.length > r.length) match {
      case Nil => List()
      case accesses => accesses.head
    }
  }

  /**
   * Utility function for building a bit of access information about an
   * ArrayType instance
   *
   * @param ty the array type passed as a generic type. We check that it is
   *           indeed an array type below.
   * @param v the variable used to perform the access
   */
  protected def getArrayAccessInf(ty: Type, v: ArithExpr): SingleAccess = {
    ty match {
      case at: ArrayType => (at.replacedElemT, v)
      case _ => throw new IllegalArgumentException(
        s"Cannot compute access information for $ty. ArrayType required."
      )
    }
  }
}
