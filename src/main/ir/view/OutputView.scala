package ir.view

import apart.arithmetic.{ArithExpr, Cst}
import ir._
import ir.ast._
import opencl.ir.{OpenCLMemory, OpenCLMemoryCollection}

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
  def apply(expr: Expr): Unit = {
    expr.outputView = View(expr.t, "")
    visitAndBuildViews(expr, View(expr.t, ""))
  }

  private def visitAndBuildViews(expr: Expr, writeView: View): View = {
    expr match {
      case p: Param =>

        if (p.outputView.isInstanceOf[NoView])
          p.outputView=writeView

        p.outputView
      case call: FunCall => buildViewFunCall(call, writeView)
    }
  }

  private def buildViewFunCall(call: FunCall, writeView: View): View = {
    // first handle body
    val result = call.f match {
      case m: AbstractMap => buildViewMap(m, call, writeView)
      case r: AbstractPartRed => buildViewReduce(r, call, writeView)
      case s: AbstractSearch => buildViewSearch(s, call, writeView)
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
      case _: Zip => buildViewZip(call, writeView)
      case _: Unzip => writeView.zip()
      case l: Lambda => buildViewLambda(l, call, writeView)
      case fp: FPattern => buildViewLambda(fp.f, call, writeView)
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

        if (call.args(1).isInstanceOf[Param] && call.args(1).outputView == NoView)
          call.args(1).outputView = result

        visitAndBuildViews(call.args(1), result)
      case Get(i) =>
        val c = call
        call.args.head match {
          case p: Param =>
            p.mem match {
              case memCollection: OpenCLMemoryCollection =>
                val accessInfo =
                  if (p.accessInf.l.nonEmpty) p.accessInf.l (i) else p.accessInf
                val outDepth = getAccessDepth (accessInfo, c.mem)

                val subviews = if (p.outputView != NoView)
                  p.outputView.asInstanceOf[ViewTuple].ivs.toArray
                else
                  Array.fill[View] (memCollection.subMemories.length) (NoView)

                if (subviews (i) == NoView)
                  subviews (i) = View.initialiseNewView (c.t, outDepth)
                c.outputView = subviews (i)
                p.outputView = ViewTuple (subviews, p.t)

              case _ =>
                val outDepth = getAccessDepth (p.accessInf, p.mem)
                p.outputView = View.initialiseNewView (p.t, outDepth)
            }
            p.outputView
          case arg =>

            c.args.head.mem match {
              case memCollection: OpenCLMemoryCollection =>

                val p = c.args.head

                val subviews = if (p.outputView != NoView)
                  p.outputView.asInstanceOf[ViewTuple].ivs.toArray
                else
                  Array.fill[View] (memCollection.subMemories.length) (NoView)

                subviews(i) = result
                visitAndBuildViews(c.args.head, ViewTuple (subviews, p.t))

              case _ =>
                visitAndBuildViews(arg, result)
                result
            }


        }
      case _ =>

        if (call.args.length == 1) {
          call.args.head match {
            case p: Param =>

              // TODO:
              if (p.outputView == NoView)
                p.outputView = result

              result
            case _ =>
              visitAndBuildViews(call.args.head, result)
          }
        }
        else {

          call.args.foreach({
            case p: Param if p.outputView == NoView => p.outputView = result
            case _: Param =>
            case arg => visitAndBuildViews(arg, result)
          })
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

      if (arg.isInstanceOf[Param] && arg.outputView == NoView)
        arg.outputView = result.get(id)

    })

    result
  }

  private def getAccessDepth(accessInfo: AccessInfo, memory: Memory) = {
    val contLocal = OpenCLMemory.containsLocalMemory(memory)
    val contPrivate = OpenCLMemory.containsPrivateMemory(memory)

    val outDepth = if (contPrivate) accessInfo.privateAccessInf
    else if (contLocal) accessInfo.localAccessInf
    else accessInfo.globalAccessInf

    outDepth
  }

  private def buildViewUserFun(writeView: View, uf:UserFun, call: FunCall): View = {

    call.outputView = writeView

    call.args.foreach({
      case p: Param  =>
        val outDepth = getAccessDepth(p.accessInf, p.mem)
        p.outputView = View.initialiseNewView(p.t, outDepth)

      case c@FunCall(Get(i), p) =>

        p.mem match {
          case memCollection: OpenCLMemoryCollection =>
            val accessInfo = if (p.accessInf.l.nonEmpty) p.accessInf.l(i) else p.accessInf
            val outDepth = getAccessDepth(accessInfo, c.mem)

            val subviews = if (p.outputView != NoView)
              p.outputView.asInstanceOf[ViewTuple].ivs.toArray
            else
              Array.fill[View](memCollection.subMemories.length)(NoView)

            if (subviews(i) == NoView)
              subviews(i) = View.initialiseNewView(c.t, outDepth)
            c.outputView = subviews(i)
            p.outputView = ViewTuple(subviews, p.t)

          case _ =>
            val outDepth = getAccessDepth(p.accessInf, p.mem)
            p.outputView = View.initialiseNewView(p.t, outDepth)
        }
      case _ =>

    })

    View.initialiseNewView(call.t, call.outputDepth, "")
  }

  private def buildViewIterate(i: Iterate, call: FunCall, writeView: View): View = {
    val v = View.initialiseNewView(call.args.head.t, call.inputDepth)
    visitAndBuildViews(i.f.body, v)
    View.initialiseNewView(call.t, call.outputDepth)
  }

  private def buildViewMap(m: AbstractMap, call: FunCall, writeView: View): View = {
    // traverse into call.f
    visitAndBuildViews(m.f.body, writeView.access(m.loopVar))

    new ViewMap(m.f.params.head.outputView, m.loopVar, call.args.head.t)
  }

  private def buildViewReduce(r: AbstractPartRed,
                              call: FunCall, writeView: View): View = {
    // traverse into call.f
    visitAndBuildViews(r.f.body, writeView.access(Cst(0)))
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
    if (l.isInstanceOf[Lambda2]) {
      visitAndBuildViews(l.body, writeView)
      l.params(1).outputView
    } else {
      visitAndBuildViews(l.body, writeView)
      l.params.head.outputView
    }
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
