package ir.view

import arithmetic.{Cst, ArithExpr}
import ir._
import opencl.ir._

object OutputView {

  def visitAndBuildViews(expr: Expr, writeView: View, outputAccessInf: List[(ArithExpr, ArithExpr)] = List()): View = {
    expr match {
      case pr: ParamReference => pr.p.view.get(pr.i)
      case p: Param => p.view
      case call: FunCall => buildViewFunCall(call, outputAccessInf, writeView)
    }
  }

  private def buildViewFunCall(call: FunCall, outputAccessInf: List[(ArithExpr, ArithExpr)], writeView: View): View = {
    call match {
      case call: MapCall => buildViewMapCall(call, writeView, outputAccessInf)
      case call: ReduceCall => buildViewReduceCall(call, writeView, outputAccessInf)
      case call: FunCall =>
        call.f match {
          case l: Lambda => buildViewLambda(l, call, writeView, outputAccessInf)
          case cf: CompFunDef => buildViewCompFunDef(cf, writeView, outputAccessInf)
          case Split(n) => buildViewSplit(n, writeView)
          case _: Join => buildViewJoin(call, writeView)
          case uf: UserFunDef =>
            call.view = writeView
            writeView
          case s: Scatter => buildViewScatter(s, call, writeView, outputAccessInf)
          case g: Gather => buildViewGather(g, call, writeView, outputAccessInf)
          case tL: toLocal => buildViewToLocal(tL, writeView, outputAccessInf)
          case tG: toGlobal => buildViewToGlobal(tG, writeView, outputAccessInf)
          case i: Iterate => buildViewIterate(i, call, writeView, outputAccessInf)
          case tw: TransposeW => buildViewTransposeW(tw, call, writeView, outputAccessInf)
          case asVector(n) => buildViewAsVector(n, writeView)
          case _: asScalar => buildViewAsScalar(call, writeView)
          case Zip(_) | Tuple(_) => buildViewZipTuple(call, writeView, outputAccessInf)
          //case uz: Unzip =>
          case _ => writeView
        }
    }
  }

  private def buildViewZipTuple(call: FunCall, writeView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    call.args.map(visitAndBuildViews(_, writeView, outputAccessInf))
    writeView
  }

  private def buildViewIterate(i: Iterate, call:FunCall, writeView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    visitAndBuildViews(i.f.body, writeView, outputAccessInf)
    View.initialiseNewView(call.t, outputAccessInf)
  }

  private def buildViewToGlobal(tG: toGlobal, writeView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    visitAndBuildViews(tG.f.body, writeView, outputAccessInf)
  }

  private def buildViewToLocal(tL: toLocal, writeView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    visitAndBuildViews(tL.f.body, writeView, outputAccessInf)
  }

  private def buildViewMapCall(call: MapCall, writeView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    // build information where call.f should write
    val newOutputAccessInf = (Type.getLength(call.t), call.loopVar) :: outputAccessInf

    // traverse into call.f
    val innerView = visitAndBuildViews(call.f.f.body, writeView.access(call.loopVar), newOutputAccessInf)

    if (call.isConcrete) {
      // create fresh input view for following function
      View.initialiseNewView(call.arg.t, outputAccessInf, call.mem.variable.name)
    } else { // call.isAbstract and return input map view
      new ViewMap(innerView, call.loopVar, call.arg.t)
    }
  }

  private def buildViewReduceCall(call: ReduceCall, writeView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    // build information where call.f should write
    val newOutputAccessInf = (Type.getLength(call.t), Cst(0)) :: outputAccessInf
    // traverse into call.f
    visitAndBuildViews(call.f.f.body, writeView.access(Cst(0)), newOutputAccessInf)
    // create fresh input view for following function
    View.initialiseNewView(call.arg1.t, outputAccessInf, call.mem.variable.name)
  }

  private def buildViewLambda(l: Lambda, call: FunCall, writeView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    visitAndBuildViews(l.body, writeView, outputAccessInf)
  }

  private def buildViewCompFunDef(cf: CompFunDef, writeView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {

    val outputAccessInfs = scala.collection.mutable.Stack(outputAccessInf)

    cf.funs.foldLeft(writeView)((v, f) => {

      f.body match {
        case call: FunCall =>
          call.f match {
            case _: toGlobal => outputAccessInfs.push(List())
            case _: toLocal => if (outputAccessInfs.length > 1) outputAccessInfs.pop()
            case _ =>
          }
        case _ =>
      }

      visitAndBuildViews(f.body, v, outputAccessInfs.top)
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

  private def buildViewTransposeW(tw: TransposeW, call: FunCall, writeView: View, outputAccessInf: List[(ArithExpr, ArithExpr)]): View = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        writeView.
          join(m).
          reorder((i:ArithExpr) => { IndexFunction.transpose(i, ArrayType(ArrayType(typ, n), m)) }).
          split(n)
    }
  }

  private def buildViewGather(gather: Gather, call: FunCall, writeView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    visitAndBuildViews(gather.f.body, writeView, outputAccessInf)
  }

  private def buildViewScatter(scatter: Scatter, call: FunCall, writeView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    val reordered = writeView.reorder( (i:ArithExpr) => { scatter.idx.f(i, call.t) } )
    visitAndBuildViews(scatter.f.body, reordered, outputAccessInf)
  }
}
