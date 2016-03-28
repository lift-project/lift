package ir.view

import apart.arithmetic.{ArithExpr, Cst}
import ir._
import ir.ast._
import opencl.ir.{Float4, OpenCLMemory, OpenCLMemoryCollection}

/**
 * A helper object for constructing views.
 *
 * Visits the expressions left to right and builds the views for all
 * sub-expressions that write to memory.
 */
object OutputView {

  /**
   * Build output views for the expression.
   *
   * @param expr Expression to build views for
   */
  def apply(expr: Expr): Unit = visitAndBuildViews(expr, View(expr.t, ""))

  private def visitAndBuildViews(expr: Expr, writeView: View): View = {
    expr match {
      case p: Param => writeView
      case call: FunCall => buildViewFunCall(call, writeView)
    }
  }

  private def buildViewFunCall(call: FunCall, writeView: View): View = {
    // first handle body
    val result = call.f match {
      case m: AbstractMap => buildViewMap(m, call, writeView)
      case r: AbstractPartRed => buildViewReduce(r, call, writeView)
      case s: AbstractSearch => buildViewSearch(s, call, writeView)
      case l: Lambda => buildViewLambda(l, call, writeView)
      case Split(n) => buildViewSplit(n, writeView)
      case _: Join => buildViewJoin(call, writeView)
      case uf: UserFun => buildViewUserFun(writeView,uf, call)
      case uf: VectorizeUserFun => buildViewUserFun(writeView, uf.userFun, call)
      case s: Scatter => buildViewScatter(s, call, writeView)
      case i: Iterate => buildViewIterate(i, call, writeView)
      case tw: TransposeW => buildViewTransposeW(tw, call, writeView)
      case t: Transpose => buildViewTranspose(t, call, writeView)
      case asVector(n) => buildViewAsVector(n, writeView)
      case _: asScalar => buildViewAsScalar(call, writeView)
      case h: Head => buildViewHead(call, writeView)
      case t: Tail => buildViewTail(call, writeView)
      case fp: FPattern => buildViewFPattern(fp, writeView)
      case _: Zip => buildViewZip(call, writeView)
      case _ => writeView
    }

    // then handle arguments
    val argResult = call.f match {
      case Zip(_) | Tuple(_) =>
        call.args.foreach(e =>
          visitAndBuildViews(e, View.initialiseNewView(e.t, e.inputDepth)))
        // TODO: PROBABLY WRONG!
        result
      case r: AbstractPartRed =>
        val e = call.args.head
        visitAndBuildViews(e, View.initialiseNewView(e.t, e.inputDepth))

        if (call.args(1).isInstanceOf[Param])
          call.args(1).outputView = result

        visitAndBuildViews(call.args(1), result)
      case _ =>
        if (call.args.length == 1)
          visitAndBuildViews(call.args.head, result)
        else {
          call.args.foreach(arg => visitAndBuildViews(arg, result))
          // TODO: DEFINITELY WRONG!
          result
        }
    }

     argResult
  }

  private def buildViewZip(call: FunCall, writeView: View): View = {
    val result = writeView.unzip()

    call.args.zipWithIndex.foreach((pair) => {
      val arg = pair._1
      val id = pair._2

      if (arg.isInstanceOf[Param])
        arg.outputView = result.get(id)

    })

    result
  }

  private def buildViewUserFun(writeView: View, uf:UserFun, call: FunCall): View = {

      call.args.foreach({
        case p: Param  =>
          p.outputView = View.initialiseNewView(p.t, p.outputDepth)
        case c@FunCall(Get(i), p) =>
          println("<<<<<<<<<< " + p.accessInf)
          val memCollection = p.mem.asInstanceOf[OpenCLMemoryCollection]
          if (p.outputView != NoView) {

            val contLocal = OpenCLMemory.containsLocalMemory(c.mem)
            val contPrivate = OpenCLMemory.containsPrivateMemory(c.mem)

            val outDepth = if (contPrivate) p.accessInf.l(i).privateAccessInf
            else if (contLocal) p.accessInf.l(i).localAccessInf
            else p.accessInf.l(i).globalAccessInf

            val subviews = p.outputView.asInstanceOf[ViewZip].ivs.toArray
            subviews(i) = View.initialiseNewView(c.t, outDepth)
            p.outputView = ViewZip(subviews, p.t)

          } else {

            val contLocal = OpenCLMemory.containsLocalMemory(c.mem)
            val contPrivate = OpenCLMemory.containsPrivateMemory(c.mem)

            val outDepth = if (contPrivate) p.accessInf.l(i).privateAccessInf
            else if (contLocal) p.accessInf.l(i).localAccessInf
            else p.accessInf.l(i).globalAccessInf

            val subviews = Array.fill[View](memCollection.subMemories.length)(NoView)
            subviews(i) = View.initialiseNewView(c.t, outDepth)
            p.outputView = ViewZip(subviews, p.t)
          }
      })

    if (uf.name == "id" && call.t == Float4) {
      println(call)
      println(writeView)
    }

    call.outputView = writeView
    View.initialiseNewView(call.t, call.outputDepth, "")
  }

  private def buildViewIterate(i: Iterate, call: FunCall, writeView: View): View = {
    visitAndBuildViews(i.f.body, writeView)
    View.initialiseNewView(call.t, call.outputDepth)
  }

  private def buildViewFPattern(fp: FPattern, writeView: View): View = {
    visitAndBuildViews(fp.f.body, writeView)
  }

  private def buildViewMap(m: AbstractMap, call: FunCall, writeView: View): View = {
    var view = writeView

    // TODO: Find a way to deal with this in one place instead of here and in buildViewCompFunDef
    // If there was a zip, then the view could be wrong
    if (writeView.t.isInstanceOf[TupleType])
      view = View.initialiseNewView(call.t, call.inputDepth)

    // traverse into call.f
    val innerView = visitAndBuildViews(m.f.body, view.access(m.loopVar))

    if (m.f.body.isConcrete) {
      // create fresh view for following function
      View.initialiseNewView(call.args.head.t, call.outputDepth, call.mem.variable.name)
    } else { // call.isAbstract and return input map view
      new ViewMap(innerView, m.loopVar, call.args.head.t)
    }
  }

  private def buildViewReduce(r: AbstractPartRed,
                              call: FunCall, writeView: View): View = {
    visitAndBuildViews(call.args.head,
      View.initialiseNewView(call.args.head.t, call.inputDepth, call.args.head.mem.variable.name))
    // traverse into call.f
    visitAndBuildViews(r.f.body, writeView.access(Cst(0)))
    // create fresh input view for following function
//    View.initialiseNewView(call.args(1).t, call.outputDepth,
//                           call.mem.variable.name)
    new ViewMap(r.f.params(1).outputView, r.loopVar, call.args(1).t)
  }

  private def buildViewSearch(s: AbstractSearch, 
                              call:FunCall, writeView:View) :View = {
    visitAndBuildViews(call.args.head,
      View.initialiseNewView(call.args.head.t, call.inputDepth, call.args.head.mem.variable.name))
    visitAndBuildViews(s.f.body, writeView.access(Cst(0)))
    View.initialiseNewView(call.args(1).t, call.outputDepth, call.mem.variable.name)
  }

  private def buildViewLambda(l: Lambda, call: FunCall, writeView: View): View = {
    if (l.isInstanceOf[Let]) {
      visitAndBuildViews(l.body, writeView)
//      View.initialiseNewView(call.args.head.t, List())
      l.params.head.outputView
    } else if (l.isInstanceOf[Lambda2]){
      visitAndBuildViews(l.body, writeView)
      l.params(1).outputView
    } else
      visitAndBuildViews(l.body, writeView)
      l.params.head.outputView
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
    call.args.head.t match {
      case ArrayType(VectorType(_, n), _) => writeView.asVector(n)
      case _ => throw new IllegalArgumentException
    }
  }

  private def buildViewTransposeW(tw: TransposeW, call: FunCall, writeView: View): View = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        writeView.
          join(m).
          reorder((i:ArithExpr) => { transpose(i, ArrayType(ArrayType(typ, n), m)) }).
          split(n)
    }
  }

  private def buildViewTranspose(t: Transpose, call: FunCall, writeView: View): View = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        writeView.
          join(m).
          split(n)
    }
  }

  private def buildViewScatter(scatter: Scatter, call: FunCall, writeView: View): View = {
    writeView.reorder( (i:ArithExpr) => { scatter.idx.f(i, call.t) } )
  }

  private def buildViewHead(funCall: FunCall, writeView: View) : View = {
    // TODO: Not sure about this
    writeView
  }

  private def buildViewTail(funCall: FunCall, writeView: View) : View = {
    // TODO: Not right. See TestTail.tailBetweenMapsScatterAfter and
    // TODO: TestTail.tailBetweenMapsScatterBeforeAndAfter. Not sure how to fix.
    View.initialiseNewView(funCall.args.head.t, funCall.outputDepth,
      funCall.mem.variable.name)
  }
}
