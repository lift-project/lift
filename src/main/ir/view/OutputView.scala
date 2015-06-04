package ir.view

import arithmetic.{Cst, ArithExpr}
import ir._
import opencl.ir._

object OutputView {

  def apply(expr: Expr): Unit = visitAndBuildViews(expr, View(expr.t, ""))

  def visitAndBuildViews(expr: Expr, writeView: View): View = {
    expr match {
      case pr: ParamReference => pr.p.view.get(pr.i)
      case p: Param => p.view
      case call: FunCall => buildViewFunCall(call, writeView)
    }
  }

  private def buildViewFunCall(call: FunCall, writeView: View): View = {
    call match {
      case call: MapCall => buildViewMapCall(call, writeView)
      case call: ReduceCall => buildViewReduceCall(call, writeView)
      case call: FunCall =>
        call.f match {
          case l: Lambda => buildViewLambda(l, call, writeView)
          case cf: CompFunDef => buildViewCompFunDef(cf, writeView)
          case Split(n) => buildViewSplit(n, writeView)
          case _: Join => buildViewJoin(call, writeView)
          case uf: UserFunDef => buildViewUserFun(writeView, call)
          case s: Scatter => buildViewScatter(s, call, writeView)
          case tP: toPrivate => buildViewToPrivate(tP, writeView)
          case tL: toLocal => buildViewToLocal(tL, writeView)
          case tG: toGlobal => buildViewToGlobal(tG, writeView)
          case i: Iterate => buildViewIterate(i, call, writeView)
          case tw: TransposeW => buildViewTransposeW(tw, call, writeView)
          case t: Transpose => buildViewTranspose(t, call, writeView)
          case asVector(n) => buildViewAsVector(n, writeView)
          case _: asScalar => buildViewAsScalar(call, writeView)
          case Zip(_) => buildViewZip(call, writeView)
          case Tuple(_) => buildViewTuple(call, writeView)
          case h: Head => buildViewHead(h, writeView)
          case t: Tail => buildViewTail(t, writeView)
          //case uz: Unzip =>
          case _ => writeView
        }
    }
  }

  private def buildViewUserFun(writeView: View, call: FunCall): View = {
    call.view = writeView
    writeView
  }

  private def buildViewZip(call: FunCall, writeView: View): View = {
    call.args.map((expr: Expr) => visitAndBuildViews(expr, writeView))
    writeView
  }

  private def buildViewTuple(call: FunCall, writeView: View): View = {
    call.args.zipWithIndex.map({ case (e, i) => visitAndBuildViews(e,
      View.initialiseNewView(e.t, e.inputDepth)) })
    writeView
  }

  private def buildViewIterate(i: Iterate, call: FunCall, writeView: View): View = {
    visitAndBuildViews(i.f.body, writeView)
    View.initialiseNewView(call.t, call.outputDepth)
  }

  private def buildViewToGlobal(tG: toGlobal, writeView: View): View = {
    visitAndBuildViews(tG.f.body, writeView)
  }

  private def buildViewToLocal(tL: toLocal, writeView: View): View = {
    visitAndBuildViews(tL.f.body, writeView)
  }

  private def buildViewToPrivate(tP: toPrivate, writeView: View): View = {
    visitAndBuildViews(tP.f.body, writeView)
  }

  private def buildViewMapCall(call: MapCall, writeView: View): View = {
    var view = writeView

    // TODO: Find a way to deal with this in one place instead of here and in buildViewCompFunDef
    // If there was a zip, then the view could be wrong
    if (writeView.t.isInstanceOf[TupleType])
      view = View.initialiseNewView(call.t, call.inputDepth)

    // traverse into call.f
    val innerView = visitAndBuildViews(call.f.f.body, view.access(call.loopVar))

    if (call.isConcrete) {
      // create fresh view for following function
      View.initialiseNewView(call.arg.t, call.outputDepth, call.mem.variable.name)
    } else { // call.isAbstract and return input map view
      new ViewMap(innerView, call.loopVar, call.arg.t)
    }
  }

  private def buildViewReduceCall(call: ReduceCall, writeView: View): View = {
    visitAndBuildViews(call.arg0,
      View.initialiseNewView(call.arg0.t, call.inputDepth, call.arg0.mem.variable.name))
    // traverse into call.f
    visitAndBuildViews(call.f.f.body, writeView.access(Cst(0)))
    // create fresh input view for following function
    View.initialiseNewView(call.arg1.t, call.outputDepth, call.mem.variable.name)
  }

  private def buildViewLambda(l: Lambda, call: FunCall, writeView: View): View = {
    visitAndBuildViews(l.body, writeView)
  }

  private def buildViewCompFunDef(cf: CompFunDef, writeView: View): View = {
    cf.funs.foldLeft(writeView)((v, f) => {
      val resultView = visitAndBuildViews(f.body, v)

      f.body match {
        case call: FunCall =>
          if (call.args.exists({
            case call: FunCall => call.f.isInstanceOf[Zip] ||
              call.f.isInstanceOf[Tuple]
            case _ => false
          }))
            View.initialiseNewView(f.params.head.t, f.body.outputDepth)
          else
            resultView
        case _ => resultView
      }
    })
  }

  private def buildViewJoin(call: FunCall, writeView: View): View = {
    val chunkSize = call.argsType match {
      case ArrayType(ArrayType(_, n), _) => n
      case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + call.argsType)
    }

    writeView.split(chunkSize)
  }

  private def buildViewSplit(n: ArithExpr, writeView: View): View = {
    writeView.join(n)
  }

  private def buildViewAsVector(n: ArithExpr, writeView: View): View = {
    writeView.asScalar()
  }

  private def buildViewAsScalar(call: FunCall, writeView: View): View = {
    call.args(0).t match {
      case ArrayType(VectorType(_, n), _) => writeView.asVector(n)
      case _ => throw new IllegalArgumentException
    }
  }

  private def buildViewTransposeW(tw: TransposeW, call: FunCall, writeView: View): View = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        writeView.
          join(m).
          reorder((i:ArithExpr) => { IndexFunction.transpose(i, ArrayType(ArrayType(typ, n), m)) }).
          split(n)
      case _ => ???
    }
  }

  private def buildViewTranspose(t: Transpose, call: FunCall, writeView: View): View = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        writeView.
          join(m).
          split(n)
      case _ => ???
    }
  }

  private def buildViewScatter(scatter: Scatter, call: FunCall, writeView: View): View = {
    writeView.reorder( (i:ArithExpr) => { scatter.idx.f(i, call.t) } )
  }

  private def buildViewHead(head: Head, writeView: View) : View = {
    // TODO: Check with Toomas!!!!!!!!
    writeView
  }

  private def buildViewTail(tail: Tail, writeView: View) : View = {
    //TODO: Check with Toomas!!!!!!!!
    writeView
  }
}
