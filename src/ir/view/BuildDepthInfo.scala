package ir.view

import arithmetic.{Cst, ArithExpr}
import ir._
import opencl.ir._

object BuildDepthInfo {
  def apply(expr: Expr): Unit = (new BuildDepthInfo).visitAndBuildDepthInfo(expr)
}

class BuildDepthInfo() {
  var privateAccessInf = List[(ArithExpr, ArithExpr)]()
  var localAccessInf = List[(ArithExpr, ArithExpr)]()
  var globalAccessInf = List[(ArithExpr, ArithExpr)]()

  def visitAndBuildDepthInfo(expr: Expr): Unit = {
    expr match {
      case call: FunCall => buildDepthInfoFunCall(call)
      case _ =>
    }
  }

  private def buildDepthInfoFunCall(call: FunCall): Unit = {
    buildDepthForArgs(call)

    call match {
      case call: MapCall => buildDepthInfoMapCall(call)
      case call: ReduceCall => buildDepthInfoReduceCall(call)
      case call: FunCall =>
        call.f match {
          case l: Lambda => buildDepthInfoLambda(l)
          case cf: CompFunDef => buildDepthInfoCompFunDef(cf)
          case fp: FPattern => buildDepthInfoFPattern(fp)
          case _ =>
        }
      case _ =>
    }
  }

  private def buildDepthInfoMapCall(call: MapCall): Unit = {
    val readsLocal = call.arg.containsLocal
    val readsPrivate = call.arg.containsPrivate

    buildDepthInfoPatternCall(call.f.f.body, call, call.loopVar, readsLocal, readsPrivate)
  }

  private def buildDepthInfoReduceCall(call: ReduceCall): Unit = {
    val readsLocal = call.arg1.containsLocal
    val readsPrivate = call.arg1.containsPrivate

    buildDepthInfoPatternCall(call.f.f.body, call, Cst(0), readsLocal, readsPrivate)
  }

  private def buildDepthInfoPatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                        readsLocal: Boolean, readsPrivate: Boolean): Unit = {
    val tuple = (Type.getLength(call.t), index)
    val writesLocal = OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == LocalMemory
    val writesPrivate = OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == PrivateMemory

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

    call.inputDepth = if (writesPrivate) privateAccessInf else if (writesLocal) localAccessInf else globalAccessInf
    call.outputDepth = if (readsPrivate) privateAccessInf else if (readsLocal) localAccessInf else globalAccessInf
  }

  private def buildDepthForArgs(call: FunCall): Unit = {
    call.args.foreach((expr: Expr) => visitAndBuildDepthInfo(expr))
  }

  private def buildDepthInfoLambda(l: Lambda): Unit = visitAndBuildDepthInfo(l.body)

  private def buildDepthInfoCompFunDef(cf: CompFunDef): Unit = cf.funs.foreach(f => visitAndBuildDepthInfo(f.body))

  private def buildDepthInfoFPattern(fp: FPattern): Unit = visitAndBuildDepthInfo(fp.f.body)
}