package ir.view

import apart.arithmetic.{ArithExpr, Cst}
import ir._
import ir.ast._
import opencl.ir.OpenCLMemory

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

private class BuildDepthInfo() {
  var privateAccessInf = List[(ArithExpr, ArithExpr)]()
  var localAccessInf = List[(ArithExpr, ArithExpr)]()
  var globalAccessInf = List[(ArithExpr, ArithExpr)]()

  private def visitAndBuildDepthInfo(expr: Expr): Unit = {
    expr match {
      case call: FunCall => buildDepthInfoFunCall(call)
      case _ =>
    }
  }

  private def buildDepthInfoFunCall(call: FunCall): Unit = {
    buildDepthForArgs(call)

    call match {
      case call: FunCall =>
        call.f match {
          case m: AbstractMap => buildDepthInfoMapCall(m, call)
          case r: AbstractPartRed => buildDepthInfoReduceCall(r, call)
          case _ =>
            val (readsLocal, readsPrivate) = readsLocalPrivate(call)
            val (writesLocal, writesPrivate) = writesLocalPrivate(call)

            setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)

            call.f match {
              case l: Lambda =>     buildDepthInfoLambda(l)
              case fp: FPattern =>  buildDepthInfoFPattern(fp)
              case _ =>
            }
        }
      case _ =>
    }
  }

  private def buildDepthInfoMapCall(m: AbstractMap, call: FunCall): Unit = {
    val (readsLocal, readsPrivate) = readsLocalPrivate(call)

    buildDepthInfoPatternCall(m.f.body, call, m.loopVar, readsLocal, readsPrivate)
  }

  private def readsLocalPrivate(call: FunCall): (Boolean, Boolean) = {
    val readsLocal = OpenCLMemory.containsLocalMemory(call.args(0).mem)
    val readsPrivate = OpenCLMemory.containsPrivateMemory(call.args(0).mem)
    (readsLocal, readsPrivate)
  }

  private def buildDepthInfoReduceCall(r: AbstractPartRed, call: FunCall): Unit = {
    val readsLocal = OpenCLMemory.containsLocalMemory(call.args(1).mem)
    val readsPrivate = OpenCLMemory.containsPrivateMemory(call.args(1).mem)

    buildDepthInfoPatternCall(r.f.body, call, Cst(0), readsLocal, readsPrivate)
  }

  private def buildDepthInfoPatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                        readsLocal: Boolean, readsPrivate: Boolean): Unit = {
    val tuple = (Type.getLength(call.t), index)
    val (writesLocal, writesPrivate) = writesLocalPrivate(call)

    globalAccessInf = tuple :: globalAccessInf
    if (readsLocal || writesLocal)
      localAccessInf = tuple :: localAccessInf
    if (readsPrivate || writesPrivate)
      privateAccessInf = tuple :: privateAccessInf
    // traverse into call.f
    visitAndBuildDepthInfo(expr)

    globalAccessInf = globalAccessInf.tail
    if (readsLocal || writesLocal)
      localAccessInf = localAccessInf.tail
    if (readsPrivate || writesPrivate)
      privateAccessInf = privateAccessInf.tail

    setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)
  }

  private def setDepths(call: FunCall, readsLocal: Boolean, readsPrivate: Boolean, writesLocal: Boolean, writesPrivate: Boolean): Unit = {
    call.inputDepth = if (writesPrivate) privateAccessInf else if (writesLocal) localAccessInf else globalAccessInf
    call.outputDepth = if (readsPrivate) privateAccessInf else if (readsLocal) localAccessInf else globalAccessInf
  }

  private def writesLocalPrivate(call: FunCall): (Boolean, Boolean) = {
    val writesLocal = OpenCLMemory.containsLocalMemory(call.mem)
    val writesPrivate = OpenCLMemory.containsPrivateMemory(call.mem)
    (writesLocal, writesPrivate)
  }

  private def buildDepthForArgs(call: FunCall): Unit = {
    call.args.foreach((expr: Expr) => visitAndBuildDepthInfo(expr))
  }

  private def buildDepthInfoLambda(l: Lambda): Unit = visitAndBuildDepthInfo(l.body)

  private def buildDepthInfoFPattern(fp: FPattern): Unit = visitAndBuildDepthInfo(fp.f.body)
}
