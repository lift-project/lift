package ir.view

import arithmetic.ArithExpr
import ir._
import opencl.ir._

object InputView {

  def apply(expr: Expr): Unit = visitAndBuildViews(expr)

  def visitAndBuildViews(expr: Expr): View = {
    val result = expr match {
      case v: Value => if (v.view == NoView) View(v.t, v.value) else v.view
      case pr: ParamReference => pr.p.view.get(pr.i)
      case p: Param => p.view
      case call: FunCall => buildViewFunCall(call)
    }
    expr.view = result
    result
  }

  private def getViewFromArgs(call: FunCall): View = {
    if (call.args.isEmpty) {
      NoView
    } else if (call.args.length == 1) {
      visitAndBuildViews(call.args(0))
    } else {
      View.tuple(call.args.map((expr: Expr) => visitAndBuildViews(expr)):_*)
    }
  }

  private def buildViewFunCall(call: FunCall): View = {
    val argView = getViewFromArgs(call)

    call match {
      case call: MapCall => buildViewMapCall(call, argView)
      case call: ReduceCall => buildViewReduceCall(call, argView)
      case call: FunCall =>
        call.f match {
          case l: Lambda => buildViewLambda(l, call, argView)
          case cf: CompFunDef => buildViewCompFunDef(cf, call, argView)
          case z: Zip => buildViewZip(call, argView)
          case Split(n) => buildViewSplit(n, argView)
          case _: Join => buildViewJoin(call, argView)
          case uf: UserFunDef => buildViewUserFunDef()
          case g: Gather => buildViewGather(g, call, argView)
          case tP: toPrivate => buildViewToPrivate(tP, argView)
          case tL: toLocal => buildViewToLocal(tL, argView)
          case tG: toGlobal => buildViewToGlobal(tG, argView)
          case i: Iterate => buildViewIterate(i, call, argView)
          case t: Transpose => buildViewTranspose(t, call, argView)
          case tw: TransposeW => buildViewTransposeW(tw, call, argView)
          case asVector(n) => buildViewAsVector(n, argView)
          case _: asScalar => buildViewAsScalar(argView)
          case f: Filter => buildViewFilter(call, argView)
          case g: Group => buildViewGroup(g, call, argView)
          case h: Head => buildViewHead(call, argView)
          case h: Tail => buildViewTail(call, argView)
          case uz: Unzip => buildViewUnzip(call, argView)
          case _ => argView
        }
    }
  }

  private def buildViewGroup(g: Group, call: FunCall, argView: View): View = {
      argView.group(g)
  }

  private def buildViewIterate(i: Iterate, call: FunCall, argView: View): View = {
    i.f.params(0).view = argView
    visitAndBuildViews(i.f.body)
    View.initialiseNewView(call.t, call.inputDepth)
  }

  private def buildViewToGlobal(tG: toGlobal, argView: View): View = {
    tG.f.params(0).view = argView
    visitAndBuildViews(tG.f.body)
  }

  private def buildViewToLocal(tL: toLocal, argView: View): View = {
    tL.f.params(0).view = argView
    visitAndBuildViews(tL.f.body)
  }

  private def buildViewToPrivate(tP: toPrivate, argView: View): View = {
    tP.f.params(0).view = argView
    visitAndBuildViews(tP.f.body)
  }

  private def buildViewMapCall(call: MapCall, argView: View): View = {
    // pass down input view
    call.f.f.params(0).view = argView.access(call.loopVar)

    // traverse into call.f
    val innerView = visitAndBuildViews(call.f.f.body)

    if (call.isConcrete) {
      // create fresh input view for following function
      View.initialiseNewView(call.t, call.inputDepth, call.mem.variable.name)
    } else { // call.isAbstract and return input map view
      new ViewMap(innerView, call.loopVar, call.t)
    }
  }

  private def buildViewReduceCall(call: ReduceCall, argView: View): View = {
    // pass down input view
    call.f.f.params(0).view = argView.get(0)
    call.f.f.params(1).view = argView.get(1).access(call.loopVar)
    // traverse into call.f
    visitAndBuildViews(call.f.f.body)
    // create fresh input view for following function
    View.initialiseNewView(call.t, call.inputDepth, call.mem.variable.name)
  }

  private def buildViewLambda(l: Lambda, call: FunCall, argView: View): View = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).view = argView
    } else {
      l.params.zipWithIndex.foreach({ case (p, i) => p.view = argView.access(i) })
    }
    visitAndBuildViews(l.body)
  }

  private def buildViewCompFunDef(cf: CompFunDef, call: FunCall, argView: View): View = {

    cf.funs.foldRight(argView)((f, v) => {
      if (f.params.length != 1) throw new NumberOfArgumentsException
      f.params(0).view = if (v != NoView) v else View.initialiseNewView(f.params(0).t, call.inputDepth)

      visitAndBuildViews(f.body)
    })
  }

  private def buildViewZip(call: FunCall, argView: View): View = {
    argView.zip()
  }

  private def buildViewUnzip(call: FunCall, argView: View): View = {
    argView.unzip()
  }

  private def buildViewFilter(call: FunCall, argView: View): View = {
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

  private def buildViewUserFunDef(): View = {
    NoView
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

  private def buildViewTransposeW(tw: TransposeW, call: FunCall, argView: View): View = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        argView.
          join(n).
          split(m)
    }
  }

  private def buildViewGather(gather: Gather, call: FunCall, argView: View): View = {
    argView.reorder( (i:ArithExpr) => { gather.idx.f(i, call.t) } )
  }

  private def buildViewHead(head: FunCall, argView: View) : View = {
    new ViewHead(argView.access(0), head.t)
  }

  private def buildViewTail(tail: FunCall, argView: View) : View = {
    new ViewTail(argView, tail.t)
  }

}
