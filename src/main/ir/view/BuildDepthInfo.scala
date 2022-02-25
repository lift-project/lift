package ir.view

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
object BuildDepthInfo {

  /**
   * Determine the dimensions, variables and lengths.
   *
   * @param expr Starting expression.
   */
  def apply(expr: Expr): Unit = (new BuildDepthInfo).visitAndBuildDepthInfo(expr)
}

class AccessInfo(var privateAccessInf: List[SingleAccess],
                 var localAccessInf:   List[SingleAccess],
                 var globalAccessInf:  List[SingleAccess],
                 var l: Seq[AccessInfo]) {
  def apply(thisLevel: SingleAccess,
            usePrivate: Boolean, useLocal: Boolean): AccessInfo = {

    if (l.isEmpty) {
      val privateAccess = if (usePrivate) thisLevel :: privateAccessInf else privateAccessInf
      val localAccess = if (useLocal) thisLevel :: localAccessInf else localAccessInf
      val globalAccess = thisLevel :: globalAccessInf
      AccessInfo(privateAccess, localAccess, globalAccess)
    } else {
      AccessInfo(l.map(_(thisLevel, usePrivate, useLocal)))
    }
  }

  def getAccesses(addressSpace: OpenCLAddressSpace): List[SingleAccess]  = {
    val contGlobal = addressSpace.containsAddressSpace(GlobalMemory)
    val contLocal = addressSpace.containsAddressSpace(LocalMemory)
    val contPrivate = addressSpace.containsAddressSpace(PrivateMemory)

    if (contPrivate)
      privateAccessInf
    else if (contLocal)
      localAccessInf
    else if (contGlobal)
      globalAccessInf
    else throw new IllegalArgumentException()
  }

  override def toString = s"AccessInfo($privateAccessInf, $localAccessInf, $globalAccessInf, $l)"
}

object AccessInfo {
  def apply() = new AccessInfo(List(), List(), List(), List())

  def apply(privateInf: List[SingleAccess],
            localInf:   List[SingleAccess],
            globalInf:  List[SingleAccess]) =
    new AccessInfo(privateInf, localInf, globalInf, List())

  def apply(l: Seq[AccessInfo]) =
    new AccessInfo(List(), List(), List(), l)
}

private class BuildDepthInfo() {
  var privateAccessInf: List[SingleAccess] = List()
  var localAccessInf:   List[SingleAccess] = List()
  var globalAccessInf:  List[SingleAccess] = List()

  var seenMapLcl = false

  private def visitAndBuildDepthInfo(expr: Expr): AccessInfo = {
    val result = expr match {
      case call: FunCall => buildDepthInfoFunCall(call)
      case e: Expr =>
        e.inputDepth = getAccessInf(e.addressSpace.containsAddressSpace(PrivateMemory),
          e.addressSpace.containsAddressSpace(LocalMemory))
        e.accessInf
    }

    expr.accessInf = result
    result
  }

  private def buildDepthInfoFunCall(call: FunCall): AccessInfo = {

    val argInf = buildDepthForArgs(call)

    val result = call.f match {
      case m: AbstractMap => buildDepthInfoMapCall(m, call, argInf)
      case f: FilterSeq => buildDepthInfoFilterCall(f, call, argInf)
      case r: AbstractPartRed => buildDepthInfoReduceCall(r, call, argInf)
      case s: ScanSeq => buildDepthInfoScanCall(s, call, argInf)
      case sp: MapSeqSlide => buildDepthInfoMapSeqSlideCall(sp, call, argInf)
      case mv: MapSeqVector => buildDepthInfoMapSeqVectorCall(mv, call, argInf)
      case _ =>

        val (readsLocal, readsPrivate) = readsLocalPrivate(call)
        val (writesLocal, writesPrivate) = writesLocalPrivate(call)

        setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)

        call.f match {
          case l: Lambda => buildDepthInfoLambda(l, call, argInf)
          case fp: FPattern => buildDepthInfoLambda(fp.f, call, argInf)
          case Get(n) => if (argInf.l.nonEmpty) argInf.l(n) else argInf
          case _: UserFun =>
            AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
          case _ => argInf
        }
    }

    result
  }

  private def buildDepthInfoMapCall(m: AbstractMap, call: FunCall,
                                    l: AccessInfo): AccessInfo = {

    val (readsLocal, readsPrivate) = readsLocalPrivate(call)

    val orig = seenMapLcl

    if (m.isInstanceOf[MapLcl]) {
      seenMapLcl = true
      if (readsPrivate)
        println(f"[BuildDepthInfo] WARNING: MapLcl reads private memory.\nPrivate argument: " +
          f"${call.args.head} ${call.args.head.mem}\nFull call:\n$call")
    }

    val inf = getArrayAccessInf(call.args.head.t, m.loopVar)
    m.f.params.head.accessInf = l(inf, readsPrivate, readsLocal || seenMapLcl)
    buildDepthInfoPatternCall(m.f.body, call, m.loopVar, readsLocal, readsPrivate, isMapWrg = m.isInstanceOf[MapWrg])
    //buildDepthInfoPatternCall(m.f.body, call, m.loopVar, readsLocal, readsPrivate && !m.isInstanceOf[MapWrg] && !m.isInstanceOf[MapLcl])

    if (m.isInstanceOf[MapLcl])
      seenMapLcl = orig

    if (m.f.body.isConcrete) // create fresh input view for following function
      AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
    else // call.isAbstract, return input
      l
  }
  
  private def buildDepthInfoFilterCall(f: FilterSeq, call: FunCall,
                                       l: AccessInfo): AccessInfo = {
    val (readsLocal, readsPrivate) = readsLocalPrivate(call)
    
    val inf = getArrayAccessInf(call.args.head.t, f.loopRead)
    f.f.params.head.accessInf = l(inf, readsPrivate, readsLocal)
    buildDepthInfoPatternCall(f.f.body, call, f.loopRead, readsLocal, readsPrivate)
  
    if (f.f.body.isConcrete) // create fresh input view for following function
      AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
    else // call.isAbstract, return input
      l
  }
  
  private def readsLocalPrivate(call: FunCall) = containsLocalPrivate(call.args.head.mem)

  private def writesLocalPrivate(call: FunCall) = containsLocalPrivate(call.mem)

  private def containsLocalPrivate(mem: Memory) = {
    val containsLocal = OpenCLMemory.containsLocalMemory(mem)
    val containsPrivate = OpenCLMemory.containsPrivateMemory(mem)
    (containsLocal, containsPrivate)
  }

  private def buildDepthInfoReduceCall(r: AbstractPartRed, call: FunCall,
                                       l: AccessInfo): AccessInfo = {

    val (readsLocal, readsPrivate) = containsLocalPrivate(call.args(1).mem)
    
    val inf = getArrayAccessInf(call.args(1).t, r.loopVar)
    r.f.params(0).accessInf = l.l.head
    r.f.params(1).accessInf = l.l(1)(inf, readsPrivate, readsLocal || seenMapLcl)

    buildDepthInfoReducePatternCall(r.f.body, call, Cst(0), readsLocal, readsPrivate, l)

    AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
  }

  private def buildDepthInfoScanCall(s: ScanSeq, call: FunCall,
                                     l: AccessInfo): AccessInfo = {

    val (readsLocal, readsPrivate) = containsLocalPrivate(call.args(1).mem)

    val inf = getArrayAccessInf(call.args(1).t, s.loopVar)
    s.f.params(0).accessInf = l.l.head
    s.f.params(1).accessInf = l.l(1)(inf, readsPrivate, readsLocal || seenMapLcl)

    buildDepthInfoReducePatternCall(s.f.body, call, Cst(0) /* unsure here */, readsLocal, readsPrivate, l)

    AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
  }

  private def buildDepthInfoMapSeqSlideCall(sp: MapSeqSlide, call: FunCall,
                                             l: AccessInfo): AccessInfo = {

    val (readsLocal, readsPrivate) = readsLocalPrivate(call)

    val inf = getArrayAccessInf(call.args.head.t, sp.loopVar)
    sp.f.params.head.accessInf = l(inf, readsPrivate, readsLocal || seenMapLcl)
    buildDepthInfoPatternCall(sp.f.body, call, sp.loopVar, readsLocal, readsPrivate)

    if (sp.f.body.isConcrete) // create fresh input view for following function
      AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
    else // call.isAbstract, return input
      l
  }

  private def buildDepthInfoMapSeqVectorCall(mv: MapSeqVector, call: FunCall,
                                             l: AccessInfo): AccessInfo = {
    assert(mv.argTVectorizedPart.isDefined)
    assert(mv.argTScalarPart.isDefined)

    val (readsLocal, readsPrivate) = readsLocalPrivate(call)

    if (mv.vectorPartNonEmpty) {
      val infV = getArrayAccessInf(mv.argTVectorizedPart.get.get, mv.vectorLoopVar)
      mv.fVectorized.params.head.accessInf = l(infV, readsPrivate, readsLocal || seenMapLcl)
    }

    if (mv.scalarPartNonEmpty) {
      val infS = getArrayAccessInf(mv.argTScalarPart.get.get, mv.scalarLoopVar)
      mv.fScalar.params.head.accessInf = l(infS, readsPrivate, readsLocal || seenMapLcl)
    }

    buildDepthInfoPatternCall(mv.fVectorized.body, call, mv.vectorLoopVar * 4 + mv.vectorCopyLoopVar, readsLocal, readsPrivate)
    buildDepthInfoPatternCall(mv.fScalar.body, call, mv.scalarLoopVar, readsLocal, readsPrivate)

    if (mv.fVectorized.body.isConcrete) { // create fresh input view for following function
      assert(mv.fScalar.body.isConcrete)
      AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
    }
    else // call.isAbstract, return input
      l
  }

  private def buildDepthInfoReducePatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                              readsLocal: Boolean, readsPrivate: Boolean,
                                              l: AccessInfo
                                             ): Unit = {
    val inf = getArrayAccessInf(call.t, index)
    val (writesLocal, writesPrivate) = writesLocalPrivate(call)

    updateAccessInf(readsLocal, readsPrivate, inf, writesLocal, writesPrivate, isMapWrg = false)
    // traverse into call.f
    visitAndBuildDepthInfo(expr)

    restoreAccessInf(readsLocal, readsPrivate, writesLocal, writesPrivate, isMapWrg = false)

    setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)

  }

  private def updateAccessInf(readsLocal: Boolean, readsPrivate: Boolean,
                              tuple: SingleAccess,
                              writesLocal: Boolean, writesPrivate: Boolean,
                              isMapWrg: Boolean): Unit = {
    globalAccessInf = tuple :: globalAccessInf
    if (!isMapWrg && (seenMapLcl || readsLocal || writesLocal))
      localAccessInf = tuple :: localAccessInf
    if (!isMapWrg && (readsPrivate || writesPrivate))
      privateAccessInf = tuple :: privateAccessInf
  }

  private def buildDepthInfoPatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                        readsLocal: Boolean, readsPrivate: Boolean,
                                        isMapWrg: Boolean = false): Unit = {
    val inf = getArrayAccessInf(call.t, index)
    val (writesLocal, writesPrivate) = writesLocalPrivate(call)

    updateAccessInf(readsLocal, readsPrivate, inf, writesLocal, writesPrivate, isMapWrg)

    // traverse into call.f
    visitAndBuildDepthInfo(expr)

    restoreAccessInf(readsLocal, readsPrivate, writesLocal, writesPrivate, isMapWrg)

    setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)

    AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
  }

  private def restoreAccessInf(readsLocal: Boolean, readsPrivate: Boolean, writesLocal: Boolean, writesPrivate: Boolean,
                               isMapWrg: Boolean): Unit = {
    globalAccessInf = globalAccessInf.tail
    if (!isMapWrg && (seenMapLcl || readsLocal || writesLocal))
      localAccessInf = localAccessInf.tail
    if (!isMapWrg && (readsPrivate || writesPrivate))
      privateAccessInf = privateAccessInf.tail
  }

  private def setDepths(call: FunCall, readsLocal: Boolean, readsPrivate: Boolean,
                        writesLocal: Boolean, writesPrivate: Boolean): Unit = {
    call.inputDepth = getAccessInf(writesPrivate, writesLocal)
    call.outputDepth = getAccessInf(readsPrivate, readsLocal)
  }

  private def buildDepthForArgs(call: FunCall): AccessInfo = {

    if (call.args.length == 1)
      visitAndBuildDepthInfo(call.args.head)
    else
      AccessInfo(call.args.map((expr: Expr) => visitAndBuildDepthInfo(expr)))
  }

  private def buildDepthInfoLambda(l: Lambda, call: FunCall,
                                   list: AccessInfo): AccessInfo = {

    if (call.args.length == 1)
      setAccessInfo(list, l.params.head)
    else
      (list.l, l.params).zipped.foreach((accessInfo, param) =>
        setAccessInfo(accessInfo, param))

    visitAndBuildDepthInfo(l.body)
  }

  private def setAccessInfo(list: AccessInfo, param: Param): Unit = {
    val (readsLocal, readsPrivate) = containsLocalPrivate(param.mem)

    param.outputDepth = getAccessInf(readsPrivate, readsLocal)
    param.accessInf = list
  }

  private def getAccessInf(readsPrivate: Boolean, readsLocal: Boolean): List[SingleAccess] = {
    if (readsPrivate)
      privateAccessInf
    else if (readsLocal)
      localAccessInf
    else
      globalAccessInf
  }
  
  /**
   * Utility function for building a bit of access information about an
   * ArrayType instance
   *
   * @param ty the array type passed as a generic type. We check that it is
   *           indeed an array type below.
   * @param v the variable used to perform the access
   */
  private def getArrayAccessInf(ty: Type, v: ArithExpr): SingleAccess = {
    ty match {
      case at: ArrayType => (at.replacedElemT, v)
      case _ => throw new IllegalArgumentException(
        s"Cannot compute access information for $ty. ArrayType required."
      )
    }
  }
}
