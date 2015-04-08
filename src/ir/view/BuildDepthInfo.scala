package ir.view

import arithmetic.{Cst, ArithExpr}
import ir._
import opencl.ir._

object BuildDepthInfo {
  def apply(expr: Expr): Unit = (new BuildDepthInfo).visitAndBuildDepthInfo(expr)
}

class BuildDepthInfo() {
  val localAccessInfs = scala.collection.mutable.Stack[List[(ArithExpr, ArithExpr)]]()
  val globalAccessInfs = scala.collection.mutable.Stack[List[(ArithExpr, ArithExpr)]]()

  def visitAndBuildDepthInfo(expr: Expr, outputAccessInf:  List[(ArithExpr, ArithExpr)] = List()): List[(ArithExpr, ArithExpr)] = {
    expr match {
      case call: FunCall => expr.inputDepth = buildDepthInfoFunCall(call, outputAccessInf)
      case _ =>
    }
    outputAccessInf
  }

  private def getDepthForArgs(call: FunCall, outputAccessInf:  List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {
    call.args.map((expr: Expr) => visitAndBuildDepthInfo(expr, outputAccessInf))
    outputAccessInf
  }

  private def buildDepthInfoFunCall(call: FunCall, outputAccessInf: List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {
    getDepthForArgs(call, outputAccessInf)

    call match {
      case call: MapCall => buildDepthInfoMapCall(call, outputAccessInf)
      case call: ReduceCall => buildDepthInfoReduceCall(call, outputAccessInf)
      case call: FunCall =>
        call.f match {
          case l: Lambda => buildDepthInfoLambda(l, outputAccessInf)
          case cf: CompFunDef => buildDepthInfoCompFunDef(cf, outputAccessInf)
          case g: Gather => buildDepthInfoGather(g, outputAccessInf)
          case s: Scatter => buildDepthInfoScatter(s, outputAccessInf)
          case tL: toLocal => buildDepthInfoToLocal(tL, outputAccessInf)
          case tG: toGlobal => buildDepthInfoToGlobal(tG, outputAccessInf)
          case i: Iterate => buildDepthInfoIterate(i, outputAccessInf)
          case _ =>
        }
    }
    outputAccessInf
  }

  private def buildDepthInfoIterate(i: Iterate, outputAccessInf: List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {
    visitAndBuildDepthInfo(i.f.body, outputAccessInf)
    outputAccessInf
  }

  private def buildDepthInfoToGlobal(tG: toGlobal, outputAccessInf: List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {
    if (localAccessInfs.length > 1) localAccessInfs.pop()
    visitAndBuildDepthInfo(tG.f.body, outputAccessInf)
    outputAccessInf
  }

  private def buildDepthInfoToLocal(tL: toLocal, outputAccessInf: List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {
    localAccessInfs.push(List())
    visitAndBuildDepthInfo(tL.f.body, outputAccessInf)
    outputAccessInf
  }

  private def buildDepthInfoMapCall(call: MapCall, outputAccessInf: List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {
    // pass down input view
    // build information where call.f should write
    val newOutputAccessInf = (Type.getLength(call.t), call.loopVar) :: outputAccessInf

    // traverse into call.f
    visitAndBuildDepthInfo(call.f.f.body, newOutputAccessInf)

    outputAccessInf
  }

  private def buildDepthInfoReduceCall(call: ReduceCall, outputAccessInf: List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {
    // pass down input view
    // build information where call.f should write
    val newOutputAccessInf = (Type.getLength(call.t), Cst(0)) :: outputAccessInf
    // traverse into call.f
    visitAndBuildDepthInfo(call.f.f.body, newOutputAccessInf)

    outputAccessInf
  }

  private def buildDepthInfoLambda(l: Lambda, outputAccessInf: List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {
    visitAndBuildDepthInfo(l.body, outputAccessInf)
    outputAccessInf
  }

  private def buildDepthInfoCompFunDef(cf: CompFunDef, outputAccessInf: List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {

    localAccessInfs.push(outputAccessInf)

    cf.funs.map(f => visitAndBuildDepthInfo(f.body, localAccessInfs.top))
    localAccessInfs.pop()
    outputAccessInf
  }

  private def buildDepthInfoGather(gather: Gather, outputAccessInf: List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {
    visitAndBuildDepthInfo(gather.f.body, outputAccessInf)
    outputAccessInf
  }

  private def buildDepthInfoScatter(scatter: Scatter, outputAccessInf: List[(ArithExpr, ArithExpr)]): List[(ArithExpr, ArithExpr)] = {
    visitAndBuildDepthInfo(scatter.f.body, outputAccessInf)
    outputAccessInf
  }
}