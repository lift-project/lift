package ir.view

import ir._
import opencl.ir._

object InputView {

  def visitAndBuildViews(expr: Expr, outputAccessInf:  List[(ArithExpr, ArithExpr)] = List()): View = {
    val result = expr match {
      case pr: ParamReference => pr.p.view.get(pr.i)
      case p: Param => p.view
      case call: FunCall => buildViewFunCall(call, outputAccessInf)
    }
    expr.view = result
    result
  }

  private def getViewFromArgs(call: FunCall, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    if (call.args.isEmpty) {
      NoView
    } else if (call.args.length == 1) {
      visitAndBuildViews(call.args(0), outputAccessInf)
    } else {
      View.tuple(call.args.map((expr: Expr) => visitAndBuildViews(expr, outputAccessInf)):_*)
    }
  }

  private def buildViewFunCall(call: FunCall, outputAccessInf: List[(ArithExpr, ArithExpr)]): View = {
    val argView = getViewFromArgs(call, outputAccessInf)

    call match {
      case call: MapCall => buildViewMapCall(call, argView, outputAccessInf)
      case call: ReduceCall => buildViewReduceCall(call, argView, outputAccessInf)
      case call: FunCall =>
        call.f match {
          case l: Lambda => buildViewLambda(l, call, argView, outputAccessInf)
          case cf: CompFunDef => buildViewCompFunDef(cf, argView, outputAccessInf)
          case z: Zip => buildViewZip(z, call, argView)
          case Split(n) => buildViewSplit(n, argView)
          case _: Join => buildViewJoin(call, argView)
          case uf: UserFunDef => buildViewUserFunDef(uf, argView, outputAccessInf)
          case ReorderStride(s) => buildViewReorderStride(s, call, argView)
          case g: Gather => buildViewGather(g, call, argView, outputAccessInf)
          case s: Scatter => buildViewScatter(s, call, argView, outputAccessInf)
          case tL: toLocal => buildViewToLocal(tL, argView, outputAccessInf)
          case tG: toGlobal => buildViewToGlobal(tG, argView, outputAccessInf)
          case i: Iterate => buildViewIterate(i, call, argView, outputAccessInf)
          case t: Transpose => buildViewTranspose(t, call, argView)
          case asVector(n) => buildViewAsVector(n, argView)
          case _: asScalar => buildViewAsScalar(argView)
          case f: Filter => buildViewFilter(f, call, argView)
          /*case uz: Unzip =>
          case SplitDim2(n) =>
          case j: JoinDim2 =>

          */
          case _ => argView
        }
    }
  }

  private def buildViewIterate(i: Iterate, call:FunCall, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    i.f.params(0).view = argView
    visitAndBuildViews(i.f.body, outputAccessInf)
    View.initialiseNewView(call.t, outputAccessInf)
  }

  private def buildViewToGlobal(tG: toGlobal, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    tG.f.params(0).view = argView
    visitAndBuildViews(tG.f.body, outputAccessInf)
  }

  private def buildViewToLocal(tL: toLocal, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    tL.f.params(0).view = argView
    visitAndBuildViews(tL.f.body, outputAccessInf)
  }

  private def buildViewMapCall(call: MapCall, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    // pass down input view
    call.f.f.params(0).view = argView.access(call.loopVar)
    // build information where call.f should write
    val newOutputAccessInf = (Type.getLength(call.t), call.loopVar) :: outputAccessInf

    // traverse into call.f
    val innerView = visitAndBuildViews(call.f.f.body, newOutputAccessInf)

    if (call.isConcrete) {
      // create fresh input view for following function
      View.initialiseNewView(call.t, outputAccessInf, call.mem.variable.name)
    } else { // call.isAbstract and return input map view
      new ViewMap(innerView, call.loopVar, call.t)
    }
  }

  private def buildViewReduceCall(call: ReduceCall, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    // pass down input view
    call.f.f.params(0).view = argView.get(0)
    call.f.f.params(1).view = argView.get(1).access(call.loopVar)
    // build information where call.f should write
    val newOutputAccessInf = (Type.getLength(call.t), Cst(0)) :: outputAccessInf
    // traverse into call.f
    visitAndBuildViews(call.f.f.body, newOutputAccessInf)
    // create fresh input view for following function
    View.initialiseNewView(call.t, outputAccessInf, call.mem.variable.name)
  }

  private def buildViewLambda(l: Lambda, call: FunCall, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).view = argView
    } else {
      l.params.zipWithIndex.foreach({ case (p, i) => p.view = argView.access(i) })
    }
    visitAndBuildViews(l.body, outputAccessInf)
  }

  private def buildViewCompFunDef(cf: CompFunDef, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {

    val outputAccessInfs = scala.collection.mutable.Stack(outputAccessInf)

    cf.funs.foldRight(argView)((f, v) => {
      if (f.params.length != 1) throw new NumberOfArgumentsException
      f.params(0).view = if (v != NoView) v else View.initialiseNewView(f.params(0).t, outputAccessInf)

      f.body match {
        case call: FunCall =>
          call.f match {
            case tL: toLocal => outputAccessInfs.push(List())
            case tG: toGlobal => if (outputAccessInfs.length > 1) outputAccessInfs.pop()
            case _ =>
          }
        case _ =>
      }

      visitAndBuildViews(f.body, outputAccessInfs.top)
    })
  }

  private def buildViewZip(z: Zip, call: FunCall, argView: View): View = {
    argView.zip()
  }

  private def buildViewFilter(f: Filter, call: FunCall, argView: View): View = {
    argView.get(0).filter(argView.get(1))
  }

  private def buildViewJoin(call: FunCall, argView: View): View = {
    val chunkSize = call.argsType match {
      case ArrayType(ArrayType(_, n), _) => n
      case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + call.argsType)
    }

    argView.join(chunkSize)
  }

  private def buildViewSplit(n: ArithExpr, argView: View): View = {
    argView.split(n)
  }

  private def buildViewAsVector(n: ArithExpr, argView: View): View = {
    argView.asVector(n)
  }

  private def buildViewAsScalar(argView: View): View = {
    argView.asScalar()
  }

  private def buildViewUserFunDef(uf: UserFunDef, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    NoView
  }

  private def buildViewReorderStride(s: ArithExpr, call: FunCall, argView: View): View = {
    val n = Type.getLength(call.argsType) / s

    argView.reorder( (i:ArithExpr) => { (i div n) + s * ( i % n) } )
  }

  private def buildViewTranspose(t: Transpose, call: FunCall, argView: View): View = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        argView.
          join(n).
          reorder((i:ArithExpr) => { IndexFunction.transpose(i, call.t) }).
          split(m)
    }
  }

  private def buildViewGather(gather: Gather, call: FunCall, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    gather.f.params(0).view = argView.reorder( (i:ArithExpr) => { gather.idx.f(i, call.t) } )
    visitAndBuildViews(gather.f.body, outputAccessInf)
  }

  private def buildViewScatter(scatter: Scatter, call: FunCall, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    scatter.f.params(0).view = argView
    visitAndBuildViews(scatter.f.body, outputAccessInf)
  }

}
