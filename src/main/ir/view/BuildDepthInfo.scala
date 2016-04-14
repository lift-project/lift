package ir.view

import apart.arithmetic.{ArithExpr, Cst}
import benchmarks.MolecularDynamics
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.OpenCLMemory
import opencl.ir.pattern._

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

class AccessInfo(var privateAccessInf: List[(ArithExpr, ArithExpr)],
                 var localAccessInf: List[(ArithExpr, ArithExpr)],
                 var globalAccessInf: List[(ArithExpr, ArithExpr)],
                 var l: Seq[AccessInfo]) {

  def apply(thisLevel: (ArithExpr, ArithExpr),
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

  override def toString = s"AccessInfo($privateAccessInf, $localAccessInf, $globalAccessInf, $l)"
}

object AccessInfo {
  def apply() = new AccessInfo(List(), List(), List(), List())

  def apply(privateInf: List[(ArithExpr, ArithExpr)],
            localInf: List[(ArithExpr, ArithExpr)],
            globalInf: List[(ArithExpr, ArithExpr)]) =
    new AccessInfo(privateInf, localInf, globalInf, List())

  def apply(l: Seq[AccessInfo]) =
    new AccessInfo(List(), List(), List(), l)
}

private class BuildDepthInfo() {
  var privateAccessInf = List[(ArithExpr, ArithExpr)]()
  var localAccessInf = List[(ArithExpr, ArithExpr)]()
  var globalAccessInf = List[(ArithExpr, ArithExpr)]()

  var seenMapLcl = false

  private def visitAndBuildDepthInfo(expr: Expr): AccessInfo = {
    val result = expr match {
      case call: FunCall => buildDepthInfoFunCall(call)
      case p: Param => p.accessInf
    }

    expr.accessInf = result

    result
  }

  var seenMap = false

  private def buildDepthInfoFunCall(call: FunCall): AccessInfo = {
    val argInf = buildDepthForArgs(call)
//
//    if (seenMap) {
//      if (argInf.globalAccessInf.isEmpty && argInf.l.isEmpty) {
//
//
//        println()
//      }
//    }

    val result = call match {
      case call: FunCall =>
        call.f match {
          case m: AbstractMap =>
            seenMap = true
            buildDepthInfoMapCall(m, call, argInf)
          case r: AbstractPartRed => buildDepthInfoReduceCall(r, call, argInf)
          case _ =>
            val (readsLocal, readsPrivate) = readsLocalPrivate(call)
            val (writesLocal, writesPrivate) = writesLocalPrivate(call)

            setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)

            call.f match {
              case l: Lambda => buildDepthInfoLambda(l, call, argInf)
              case fp: FPattern =>  buildDepthInfoLambda(fp.f, call, argInf)
              case _: toLocal =>
                println("")
                return argInf
              case Get(n) => argInf.l(n)
              case _: UserFun =>
                AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
              case _ => argInf
            }
        }
      case _ => argInf
    }

    result
  }

  private def buildDepthInfoMapCall(m: AbstractMap, call: FunCall,
                                    l: AccessInfo): AccessInfo = {

    val (readsLocal, readsPrivate) = readsLocalPrivate(call)

    val orig = seenMapLcl

    if (m.isInstanceOf[MapLcl])
    seenMapLcl = true


    m.f.params.head.accessInf = l((Type.getLength(call.args.head.t), m.loopVar), readsPrivate, readsLocal || seenMapLcl)
    println(s">>>>>>>>>> ${Type.getLength(call.args.head.t)}  ${m.loopVar} ${m.getClass.getSimpleName}")


    buildDepthInfoPatternCall(m.f.body, call, m.loopVar, readsLocal, readsPrivate)

    if (m.isInstanceOf[MapLcl])
      seenMapLcl = orig

    m.f.body match {
      case innerCall: FunCall if innerCall.f.isInstanceOf[UserFun] =>
        // create fresh input view for following function
        AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
      case _ => // call.isAbstract and return input map view
        l
    }
  }

  def readsLocalPrivate(call: FunCall): (Boolean, Boolean) = {
    val readsLocal = OpenCLMemory.containsLocalMemory(call.args.head.mem)
    val readsPrivate = OpenCLMemory.containsPrivateMemory(call.args.head.mem)
    (readsLocal, readsPrivate)
  }

  private def buildDepthInfoReduceCall(r: AbstractPartRed, call: FunCall,
                                       l: AccessInfo): AccessInfo = {


    val readsLocal = OpenCLMemory.containsLocalMemory(call.args(1).mem)
    val readsPrivate = OpenCLMemory.containsPrivateMemory(call.args(1).mem)

    val length = Type.getLength(call.args(1).t)
    r.f.params(0).accessInf = l.l.head
    r.f.params(1).accessInf =
      l.l(1)((length, r.loopVar), readsPrivate, readsLocal || seenMapLcl)

    println(s">>>>>>>>>> $length  ${r.loopVar}")

    buildDepthInfoReducePatternCall(r.f.body, call, length, r.loopVar, readsLocal, readsPrivate, l)
    AccessInfo(privateAccessInf, globalAccessInf, localAccessInf)
  }

  private def buildDepthInfoReducePatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                              index2: ArithExpr,
                                              readsLocal: Boolean, readsPrivate: Boolean,
                                              l: AccessInfo
                                             ): Unit = {
    val tuple = (Type.getLength(call.t), index)
    val (writesLocal, writesPrivate) = writesLocalPrivate(call)

    globalAccessInf = tuple :: globalAccessInf
    if (seenMapLcl || readsLocal || writesLocal)
      localAccessInf = tuple :: localAccessInf
    if (readsPrivate || writesPrivate)
      privateAccessInf = tuple :: privateAccessInf
    // traverse into call.f
    visitAndBuildDepthInfo(expr)

    globalAccessInf = globalAccessInf.tail
    if (seenMapLcl || readsLocal || writesLocal)
      localAccessInf = localAccessInf.tail
    if (readsPrivate || writesPrivate)
      privateAccessInf = privateAccessInf.tail

    setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)

  }

  private def buildDepthInfoPatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                        readsLocal: Boolean, readsPrivate: Boolean): Unit = {
    val tuple = (Type.getLength(call.t), index)
    val (writesLocal, writesPrivate) = writesLocalPrivate(call)

    globalAccessInf = tuple :: globalAccessInf
    if (seenMapLcl || readsLocal || writesLocal)
      localAccessInf = tuple :: localAccessInf
    if (readsPrivate || writesPrivate)
      privateAccessInf = tuple :: privateAccessInf
    // traverse into call.f
    val a = visitAndBuildDepthInfo(expr)

    globalAccessInf = globalAccessInf.tail
    if (seenMapLcl || readsLocal || writesLocal)
      localAccessInf = localAccessInf.tail
    if (readsPrivate || writesPrivate)
      privateAccessInf = privateAccessInf.tail

    setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)

    AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
  }

  private def setDepths(call: FunCall, readsLocal: Boolean, readsPrivate: Boolean,
                        writesLocal: Boolean, writesPrivate: Boolean): Unit = {
    call.inputDepth = if (writesPrivate) privateAccessInf
                      else if (writesLocal) localAccessInf
                      else globalAccessInf
    call.outputDepth = if (readsPrivate) privateAccessInf
                       else if (readsLocal) localAccessInf
                       else globalAccessInf
  }

  private def writesLocalPrivate(call: FunCall): (Boolean, Boolean) = {
    val writesLocal = OpenCLMemory.containsLocalMemory(call.mem)
    val writesPrivate = OpenCLMemory.containsPrivateMemory(call.mem)
    (writesLocal, writesPrivate)
  }

  private def buildDepthForArgs(call: FunCall): AccessInfo = {

    if (call.args.length == 1) {
      visitAndBuildDepthInfo(call.args.head)
    } else {

      AccessInfo(call.args.map((expr: Expr) => visitAndBuildDepthInfo(expr)))
    }
  }

  private def buildDepthInfoLambda(l: Lambda, call: FunCall,
                                   list: AccessInfo): AccessInfo = {

    l match {
      case Lambda(_, FunCall(toLocal(_), _)) =>
        println(list)
      case _ =>
    }

    // TODO: outDepth for Params
    if (call.args.length == 1) {
      l.params(0).accessInf = list

      val param = l.params.head

      val readsPrivate = OpenCLMemory.containsPrivateMemory(param.mem)
      val readsLocal = OpenCLMemory.containsLocalMemory(param.mem)

      param.outputDepth = if (readsPrivate) privateAccessInf
      else if (readsLocal) localAccessInf else globalAccessInf

    } else {

      (l.params, list.l).zipped.foreach((param, accessInfo) => {
        val readsPrivate = OpenCLMemory.containsPrivateMemory(param.mem)
        val readsLocal = OpenCLMemory.containsLocalMemory(param.mem)

        param.outputDepth = if (readsPrivate) privateAccessInf
        else if (readsLocal) localAccessInf else globalAccessInf

        param.accessInf = accessInfo
      })
    }

    visitAndBuildDepthInfo(l.body)
  }

}
