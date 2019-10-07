package ir.view

import backends.common.view.{AccessInfo, AccessInfoSingleton, BuildDepthInfo, MemoryAccessInfo, SingleAccess}
import lift.arithmetic.{ArithExpr, Cst}
import ir._
import ir.ast._
import opencl.ir.{GlobalMemory, LocalMemory, OpenCLAddressSpace, OpenCLMemory, PrivateMemory}
import opencl.ir.pattern._

// FIXME: rewrite me
/**
 * Helper object for building views.
 *
 * Determine the dimensionality and length of each dimension of all global,
 * local and private arrays, as well as the iteration/access variables in all
 * dimensions.
 *
 * Needs to be done separately for input and output, as an e.g. expression can read
 * from global memory and write to private.
 *
 * The tuples contain the variable in the first position and the length in
 * the second.
 *
 */
object BuildDepthInfoCL {

  /**
   * Determine the dimensions, variables and lengths.
   *
   * @param expr Starting expression.
   */
  def apply(expr: Expr): Unit = (new BuildDepthInfoCL).visitAndBuildDepthInfo(expr)
}

object MemoryAccessInfoCL {
  def apply(): MemoryAccessInfo = scala.collection.mutable.ListMap[AddressSpace, List[SingleAccess]](
    PrivateMemory -> List(), LocalMemory -> List(), GlobalMemory -> List())
}

class AccessInfoCL(override var accessInf: MemoryAccessInfo,
                   override var collection: Seq[AccessInfoCL])
  extends AccessInfo(accessInf, collection) {

  override def toString = s"AccessInfoCL($accessInf, $collection)"
}

object AccessInfoCL extends AccessInfoSingleton[AccessInfoCL] {
  val alwaysUsedMemories: Set[AddressSpace] = Set(GlobalMemory)

  def apply() = new AccessInfoCL(accessInf = MemoryAccessInfoCL(), collection = Seq())
  def apply(accessInf: MemoryAccessInfo) = new AccessInfoCL(accessInf, Seq())
  def apply(collection: Seq[AccessInfoCL]) = new AccessInfoCL(MemoryAccessInfoCL(), collection)
}

private class BuildDepthInfoCL() extends BuildDepthInfo[AccessInfoCL] {
  implicit val accessInfoSingleton: AccessInfoCL.type = AccessInfoCL

  var memoryAccessInfo: MemoryAccessInfo = MemoryAccessInfoCL()

  var seenMapLcl = false

  private def visitAndBuildDepthInfo(expr: Expr): AccessInfoCL = {
    val result = expr match {
      case call: FunCall => buildDepthInfoFunCall(call)
      case e: Expr =>
        e.inputDepth = getAccessInf(
          e.addressSpace.asInstanceOf[OpenCLAddressSpace].containsAddressSpace(PrivateMemory),
          e.addressSpace.asInstanceOf[OpenCLAddressSpace].containsAddressSpace(LocalMemory))
        e.accessInf.asInstanceOf[AccessInfoCL]
    }

    expr.accessInf = result
    result
  }

  private def buildDepthInfoFunCall(call: FunCall): AccessInfoCL = {

    val argInf = buildDepthForArgs(call)

    val result = call.f match {
      case m: AbstractMap => buildDepthInfoMapCall(m, call, argInf)
      case f: FilterSeq => buildDepthInfoFilterCall(f, call, argInf)
      case r: AbstractPartRed => buildDepthInfoReduceCall(r, call, argInf)
      case s: ScanSeq => buildDepthInfoScanCall(s, call, argInf)
      case sp: MapSeqSlide => buildDepthInfoMapSeqSlideCall(sp, call, argInf)
      case _ =>

        val readMemories = readsLocalPrivate(call)
        val writeMemories = writesLocalPrivate(call)

        setDepths(call, readMemories, writeMemories)

        call.f match {
          case l: Lambda => buildDepthInfoLambda(l, call, argInf)
          case fp: FPattern => buildDepthInfoLambda(fp.f, call, argInf)
          case Get(n) => if (argInf.collection.nonEmpty) argInf.collection(n) else argInf
          case _: UserFun =>
            AccessInfoCL(memoryAccessInfo)
          case _ => argInf
        }
    }

    result
  }

  private def buildDepthInfoMapCall(m: AbstractMap, call: FunCall,
                                    l: AccessInfoCL): AccessInfoCL = {

    val readMemories = readsLocalPrivate(call)

    val orig = seenMapLcl

    if (m.isInstanceOf[MapLcl])
    seenMapLcl = true

    val inf = getArrayAccessInf(call.args.head.t, m.loopVar)
    m.f.params.head.accessInf = l(inf, readMemories)
    buildDepthInfoPatternCall(m.f.body, call, m.loopVar, readMemories)

    if (m.isInstanceOf[MapLcl])
      seenMapLcl = orig

    if (m.f.body.isConcrete) // create fresh input view for following function
      AccessInfoCL(memoryAccessInfo)
    else // call.isAbstract, return input
      l
  }
  
  private def buildDepthInfoFilterCall(f: FilterSeq, call: FunCall,
                                       l: AccessInfoCL): AccessInfoCL = {
    val readMemories = readsLocalPrivate(call)
    
    val inf = getArrayAccessInf(call.args.head.t, f.loopRead)
    f.f.params.head.accessInf = l(inf, readMemories)
    buildDepthInfoPatternCall(f.f.body, call, f.loopRead, readMemories)
  
    if (f.f.body.isConcrete) // create fresh input view for following function
      AccessInfoCL(memoryAccessInfo)
    else // call.isAbstract, return input
      l
  }
  
  private def readsLocalPrivate(call: FunCall) = containsLocalPrivate(call.args.head.mem)

  private def writesLocalPrivate(call: FunCall) = containsLocalPrivate(call.mem)

  private def containsLocalPrivate(mem: Memory) = {
    var accessedMemories = scala.collection.mutable.Set[AddressSpace]()
    if (OpenCLMemory.containsPrivateMemory(mem))
      accessedMemories += PrivateMemory
    if (OpenCLMemory.containsLocalMemory(mem))
      accessedMemories += LocalMemory
    accessedMemories.toSet
  }

  private def buildDepthInfoReduceCall(r: AbstractPartRed, call: FunCall,
                                       l: AccessInfoCL): AccessInfoCL = {

    val readMemories = containsLocalPrivate(call.args(1).mem)
    
    val inf = getArrayAccessInf(call.args(1).t, r.loopVar)
    r.f.params(0).accessInf = l.collection.head
    r.f.params(1).accessInf = l.collection(1)(inf, readMemories ++ (if (seenMapLcl) Set(LocalMemory) else Nil))

    buildDepthInfoReducePatternCall(r.f.body, call, Cst(0), readMemories, l)

    AccessInfoCL(memoryAccessInfo)
  }

  private def buildDepthInfoScanCall(s: ScanSeq, call: FunCall,
                                     l: AccessInfoCL): AccessInfoCL = {

    val readMemories = containsLocalPrivate(call.args(1).mem)

    val inf = getArrayAccessInf(call.args(1).t, s.loopVar)
    s.f.params(0).accessInf = l.collection.head
    s.f.params(1).accessInf = l.collection(1)(inf, readMemories ++ (if (seenMapLcl) Set(LocalMemory) else Nil))

    buildDepthInfoReducePatternCall(s.f.body, call, Cst(0) /* unsure here */, readMemories, l)

    AccessInfoCL(memoryAccessInfo)
  }

  private def buildDepthInfoMapSeqSlideCall(sp: MapSeqSlide, call: FunCall,
                                             l: AccessInfoCL): AccessInfoCL = {

    val readMemories = readsLocalPrivate(call)

    val inf = getArrayAccessInf(call.args.head.t, sp.loopVar)
    sp.f.params.head.accessInf = l(inf, readMemories ++ (if (seenMapLcl) Set(LocalMemory) else Nil))
    buildDepthInfoPatternCall(sp.f.body, call, sp.loopVar, readMemories)

    if (sp.f.body.isConcrete) // create fresh input view for following function
      AccessInfoCL(memoryAccessInfo)
    else // call.isAbstract, return input
      l
  }

  private def  buildDepthInfoReducePatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                              readMemories: Set[AddressSpace],
                                              l: AccessInfoCL
                                             ): Unit = {
    val inf = getArrayAccessInf(call.t, index)
    val writeMemories = writesLocalPrivate(call)

    updateAccessInf(readMemories ++ writeMemories ++ (if (seenMapLcl) Set(LocalMemory) else Nil), inf)
    // traverse into call.f
    visitAndBuildDepthInfo(expr)

    restoreAccessInf(readMemories ++ writeMemories ++ (if (seenMapLcl) Set(LocalMemory) else Nil))

    setDepths(call, readMemories, writeMemories)

  }

  private def buildDepthInfoPatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                        readMemories: Set[AddressSpace]): Unit = {
    val inf = getArrayAccessInf(call.t, index)
    val writeMemories = writesLocalPrivate(call)

    updateAccessInf(readMemories ++ writeMemories ++ (if (seenMapLcl) Set(LocalMemory) else Nil), inf)

    // traverse into call.f
    visitAndBuildDepthInfo(expr)

    restoreAccessInf(readMemories ++ writeMemories ++ (if (seenMapLcl) Set(LocalMemory) else Nil))

    setDepths(call, readMemories, writeMemories)

    AccessInfoCL(memoryAccessInfo)
  }

  private def buildDepthForArgs(call: FunCall): AccessInfoCL = {

    if (call.args.length == 1)
      visitAndBuildDepthInfo(call.args.head)
    else {
      AccessInfoCL(call.args.map((expr: Expr) => visitAndBuildDepthInfo(expr)))
    }
  }

  private def buildDepthInfoLambda(l: Lambda, call: FunCall,
                                   list: AccessInfoCL): AccessInfoCL = {

    if (call.args.length == 1)
      setAccessInfo(list, l.params.head)
    else
      (list.collection, l.params).zipped.foreach((accessInfo, param) =>
        setAccessInfo(accessInfo, param))

    visitAndBuildDepthInfo(l.body)
  }

  protected def setAccessInfo(list: AccessInfoCL, param: Param): Unit = {
    val readMemory = containsLocalPrivate(param.mem)

    param.outputDepth = getAccessInfo(readMemory)
    param.accessInf = list
  }

  private def getAccessInf(privateMemoryRead: Boolean, localMemoryRead: Boolean): List[SingleAccess] = {
    getAccessInfo((if (privateMemoryRead) Set(PrivateMemory) else Set()) ++
      (if (localMemoryRead) Set(LocalMemory) else Set()))
  }

  protected def getAccessInfo(accessedMemories: Set[AddressSpace]): List[SingleAccess] = {
    if (accessedMemories.contains(PrivateMemory))
      memoryAccessInfo(PrivateMemory)
    else if (accessedMemories.contains(LocalMemory))
      memoryAccessInfo(LocalMemory)
    else
      memoryAccessInfo(GlobalMemory)
  }
}
