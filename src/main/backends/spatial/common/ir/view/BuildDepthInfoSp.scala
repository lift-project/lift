package backends.spatial.common.ir.view

import backends.common.view._
import backends.spatial.accel.ir.pattern.{AbstractSpFold, SpForeach}
import backends.spatial.common.ir._
import ir.ast.{AbstractMap, Expr, FPattern, FunCall, Get, Lambda, Param, UserFun}
import ir.{AddressSpace, Memory}
import lift.arithmetic.{ArithExpr, Cst, Var}

/**
 * TODO
 */
object BuildDepthInfoSp {

  /**
   * Determine the dimensions, variables and lengths.
   *
   * @param expr Starting expression.
   */
  def apply(expr: Expr): Unit = (new BuildDepthInfoSp).visitAndBuildDepthInfo(expr)
}

object MemoryAccessInfoSp {
  def apply(): MemoryAccessInfo = scala.collection.mutable.ListMap[AddressSpace, List[SingleAccess]](
    RegMemory -> List(), SRAMMemory -> List(), DRAMMemory -> List())
}

class AccessInfoSp(var accessInf: MemoryAccessInfo,
                   var collection: Seq[AccessInfoSp])
  extends AccessInfo() {

  override def toString = s"AccessInfoSp($accessInf, $collection)"
}

object AccessInfoSp extends AccessInfoSingleton[AccessInfoSp] {
  val alwaysUsedMemories: Set[AddressSpace] = Set()

  def apply() = new AccessInfoSp(accessInf = MemoryAccessInfoSp(), collection = Seq())
  def apply(accessInf: MemoryAccessInfo) = new AccessInfoSp(accessInf, Seq())
  def apply(collection: Seq[AccessInfoSp]) = new AccessInfoSp(MemoryAccessInfoSp(), collection)
}

private class BuildDepthInfoSp() extends BuildDepthInfo[AccessInfoSp] {
  implicit val accessInfoSingleton: AccessInfoSp.type = AccessInfoSp

  var memoryAccessInfo: MemoryAccessInfo = MemoryAccessInfoSp()

  private def visitAndBuildDepthInfo(expr: Expr): AccessInfoSp = {
    val result = expr match {
      case call: FunCall => buildDepthInfoFunCall(call)
      case e: Expr =>
        val as = e.addressSpace.asInstanceOf[SpatialAddressSpace]
        val accessedMemory = ((if (as.containsAddressSpace(RegMemory)) Set(RegMemory) else Nil) ++
          (if (as.containsAddressSpace(SRAMMemory)) Set(SRAMMemory) else Nil) ++
          (if (as.containsAddressSpace(DRAMMemory)) Set(DRAMMemory) else Nil)).toSet[AddressSpace]

        e.inputDepth = getAccessInfo(accessedMemory)
        e.accessInf.asInstanceOf[AccessInfoSp]
    }

    expr.accessInf = result
    result
  }

  private def buildDepthInfoFunCall(call: FunCall): AccessInfoSp = {

    val argInf = buildDepthForArgs(call)

    val result = call.f match {
      case sF: SpForeach => buildDepthInfoSpForeach(sF, call, argInf)
      case m: AbstractMap => buildDepthInfoMapCall(m, call, argInf)
      case aSF: AbstractSpFold => buildDepthInfoSpFold(aSF, call, argInf)
      case _ =>

        val readMemories = getMemoryReads(call)
        val writeMemories = getMemoryWrites(call)

        setDepths(call, readMemories, writeMemories)

        call.f match {
          case l: Lambda =>     buildDepthInfoLambda(l, call, argInf)
          case fp: FPattern =>  buildDepthInfoLambda(fp.f, call, argInf)
          case Get(n) =>        if (argInf.collection.nonEmpty) argInf.collection(n) else argInf
          case _: UserFun =>    AccessInfoSp(memoryAccessInfo)
          case _ =>             argInf
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

    val readMemories = getMemoryReads(call)

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

    val readMemories = getMemoryAccesses(call.args(1).mem)

    // Map depth info
    val mapArgAccessInfo = getArrayAccessInf(call.args.head.t, aSF.mapLoopVar)
    aSF.fMap.params.head.accessInf = l.collection(1)(mapArgAccessInfo, readMemories)
    buildDepthInfoPatternCall(aSF.fMap.body, call, aSF.mapLoopVar, readMemories)

    val fMapAccessInfo = if (aSF.fMap.body.isConcrete) // create fresh input view for following function
      AccessInfoSp(memoryAccessInfo)
    else // call.isAbstract, return input
      l

    // Reduce depth info
    val reduceArgAccessInfo = getArrayAccessInf(call.args(1).t, aSF.reduceLoopVar)
    aSF.fReduce.params(0).accessInf = l.collection.head
    aSF.fReduce.params(1).accessInf = fMapAccessInfo(reduceArgAccessInfo, getMemoryAccesses(aSF.fMap.body.mem))

    val accumAccessInf = getArrayAccessInf(call.t, Cst(0))
    val writeMemories = getMemoryWrites(call)

    updateAccessInf(readMemories ++ writeMemories, accumAccessInf)
    // traverse into call.f
    visitAndBuildDepthInfo(aSF.fReduce.body)

    restoreAccessInf(readMemories ++ writeMemories)

    setDepths(call, readMemories, writeMemories)

    AccessInfoSp(memoryAccessInfo)
  }

  private def getMemoryReads(call: FunCall) = getMemoryAccesses(call.args.head.mem)

  private def getMemoryWrites(call: FunCall) = getMemoryAccesses(call.mem)

  private def getMemoryAccesses(mem: Memory) = {
    var accessedMemories = scala.collection.mutable.Set[AddressSpace]()
    if (SpatialMemory.containsRegMemory(mem))
      accessedMemories += RegMemory
    if (SpatialMemory.containsSRAMMemory(mem))
      accessedMemories += SRAMMemory
    if (SpatialMemory.containsDRAMMemory(mem))
      accessedMemories += DRAMMemory
    accessedMemories.toSet
  }

  private def buildDepthInfoPatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                        readMemories: Set[AddressSpace]): Unit = {
    val inf = getArrayAccessInf(call.t, index)
    val writeMemories = getMemoryWrites(call)

    updateAccessInf(readMemories ++ writeMemories, inf)

    // traverse into call.f
    visitAndBuildDepthInfo(expr)

    restoreAccessInf(readMemories ++ writeMemories)

    setDepths(call, readMemories, writeMemories)

    AccessInfoSp(memoryAccessInfo)
  }

  private def buildDepthForArgs(call: FunCall): AccessInfoSp = {

    if (call.args.length == 1)
      visitAndBuildDepthInfo(call.args.head)
    else {
      AccessInfoSp(call.args.map((expr: Expr) => visitAndBuildDepthInfo(expr)))
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

  protected def setAccessInfo(list: AccessInfoSp, param: Param): Unit = {
    val readMemory = getMemoryAccesses(param.mem)

    param.outputDepth = getAccessInfo(readMemory)
    param.accessInf = list
  }

  protected def getAccessInfo(accessedMemories: Set[AddressSpace]): List[SingleAccess] = {
    if (accessedMemories.contains(RegMemory))
      memoryAccessInfo(RegMemory)
    else if (accessedMemories.contains(SRAMMemory))
      memoryAccessInfo(SRAMMemory)
    else if (accessedMemories.contains(DRAMMemory))
      memoryAccessInfo(DRAMMemory)
    else throw new IllegalArgumentException("No memory to access")
  }
}
