package ir.view

import lift.arithmetic.{ArithExpr, Cst}
import ir._
import ir.ast._
import opencl.ir.pattern.{ReduceWhileSeq, toGlobal, toLocal, toPrivate}

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
    expr match {
      case fc: FunCall =>
        visitAndBuildViews(expr, (v:View) => v)
      case _ => // if the expression is not a funcall, there is no output view
    }
  }


  private val paramViewFuns : scala.collection.mutable.Map[Param, (View) => View] = scala.collection.mutable.Map()


  private def visitAndBuildViews(expr: Expr, buildView : ((View) => View)): ((View) => View) = {
    expr match {
      case call: FunCall => buildViewFunCall(call, buildView)
      case p: Param =>

        /* Retrieve the buildView function associated with the Param.
        *  A new buildView function is created which first call the current buildView function and only then call the buildView function associated with the Param.
        *  The buildView function from the param has to be called last, since this corresponds to the output view created for the argument of a lambda. */
        (v: View) => {
          val pBuildFun = paramViewFuns.get(p)
          if (pBuildFun.isEmpty)
            // the param was probably implicit (e.g. Map)
            buildView(v)
          else
            pBuildFun.get(buildView(v))
        }

      case e: Expr=> buildView
    }
  }


  private def buildViewFunCall(call: FunCall, buildView : ((View) => View) ) : ((View) => View) = {
    call.f match {

      case _: UserFun | _: VectorizeUserFun =>
        //val viewMem = View.initialiseNewOutputView(call.t, call.inputDepth, call.mem.variable)
        val viewMem = View.initialiseNewOutputView(call.t, call.inputDepth, call.mem.variable)
        call.outputView = buildView(viewMem)
        buildView
        //(v: View) => v

      case Split(n) =>
        visitAndBuildViews(call.args.head, (v: View) => buildView(v).join(n))

      case _: Join =>
        call.args.head.t match {
          case ArrayType(ArrayTypeWS(_, chunkSize)) => visitAndBuildViews(call.args.head, (v: View) => buildView(v).split(chunkSize))
          case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + call.argsType)
        }

      case s: Scatter =>
        visitAndBuildViews(call.args.head, (v: View) => buildView(v).reorder((i: ArithExpr) => s.idx.f(i, call.t)))

      case m: AbstractMap =>

        val mapOutputViewFun = visitAndBuildViews(m.f.body, (v: View) => buildView(v).access(m.loopVar))
        visitAndBuildViews(call.args.head, (v: View) => ViewMap(mapOutputViewFun(v), m.loopVar, call.args.head.t))

      case z: Zip =>

        val argsViewFuns = call.args.map(arg => visitAndBuildViews(arg, (v:View) => buildView(v)))
        (v: View) => ViewTuple(argsViewFuns.map(f => f(v)), call.argsType).unzip()

      case uz: Unzip =>
        visitAndBuildViews(call.args.head, (v: View) => buildView(v).zip())

      case _:asVector =>
        visitAndBuildViews(call.args.head, (v: View) => v.asScalar())

      case _: asScalar =>

        call.args.head.t match {
          case ArrayType(VectorType(_, n)) => (v: View) => v.asVector(n)
          case _ => throw new IllegalArgumentException("PANIC, expected array of vectors, found " + call.argsType)
        }

      case l: Lambda =>
        /* We first visit each arguments and remember what the buildView function is.
           We then visit the body of the lambda.
           When a param is encountered, the corresponding buildView function will be retrieved and a new buildFunction constructed (see visitAndBuildViews).
         */

        val lParamViewFuns: scala.collection.immutable.Map[Param, (View) => View] = l.params.zip(call.args).map({ case (param,arg) => param -> visitAndBuildViews(arg, (v:View) => v)}).toMap

        assert (paramViewFuns.keySet.intersect(lParamViewFuns.keySet).isEmpty)
        paramViewFuns ++= lParamViewFuns
        val resultFun = visitAndBuildViews(l.body, (v:View) => buildView(v))
        paramViewFuns --= lParamViewFuns.keys

        resultFun


      case r: AbstractPartRed =>

        // if the reduction is a while reduction, visit and build views for the predicate
        r match {
          case rws: ReduceWhileSeq =>
            visitAndBuildViews(rws.p.body, (v: View) => buildView(v).access(Cst(0)))
          case _ =>
        }

        // deal with the accumulator
        val acc = call.args(0)
        visitAndBuildViews(acc, (v: View) => buildView(v))

        // deal with f
        val redOutputViewFun = visitAndBuildViews(r.f.body, (v: View) => buildView(v).access(Cst(0)))

        // deal with the input argument
        val input = call.args(1)
        visitAndBuildViews(input, (v: View) => ViewMap(redOutputViewFun(v), r.loopVar, call.args.head.t))


      case _: toGlobal | _: toLocal | _:toPrivate =>
        val fp = call.f.asInstanceOf[FPattern]

        val fpOutputViewFun = visitAndBuildViews(fp.f.body, (v: View) => buildView(v))
        visitAndBuildViews(call.args.head, (v: View) => fpOutputViewFun(v))



       /*   case fp: FPattern =>
            visitAndBuildViews(l.body, (v: View) => v.asScalar())

            visitAndBuildViews(l.body, writeView)

            buildViewLambda(fp.f, call, writeView)*/



      /*
      case f: FilterSeq => buildViewFilter(f,  call, writeView)
      case sp: MapSeqSlide => buildViewMapSeqSlide(sp, call, writeView)
      case s: AbstractSearch => buildViewSearch(s, call, writeView)
      case i: Iterate => buildViewIterate(i, call, writeView)
      case tw: TransposeW => buildViewTransposeW(tw, call, writeView)
      case t: Transpose => buildViewTranspose(t, call, writeView)
      case _: Head => buildViewHead(call, writeView)
      case _: Tail => buildViewTail(call, writeView)
      case fp: FPattern => buildViewLambda(fp.f, call, writeView)
      case _: Slide =>
        View.initialiseNewView(call.args.head.t, call.args.head.inputDepth, call.mem.variable)
      case _: ArrayAccess | _: UnsafeArrayAccess | _ : CheckedArrayAccess =>
        View.initialiseNewView(call.args.head.t, call.args.head.inputDepth, call.mem.variable)
      case PrintType() | Get(_) | _: Tuple | Gather(_) | Filter() |
           Pad(_, _, _) =>
        writeView*/
      case dunno => throw new NotImplementedError(s"OutputView.scala: $dunno")


    }

  }

  /*private def buildViewFunCall(call: FunCall, writeView: View): View = {

    call.outputView = writeView

    // first handle body
    val result = call.f match {
      case m: AbstractMap => buildViewMap(m, call, writeView)
      case f: FilterSeq => buildViewFilter(f,  call, writeView)
      case r: AbstractPartRed => buildViewReduce(r, call, writeView)
      case sp: MapSeqSlide => buildViewMapSeqSlide(sp, call, writeView)
      case s: AbstractSearch => buildViewSearch(s, call, writeView)
      case Split(n) => buildViewSplit(n, writeView)

      case _: Join => buildViewJoin(call, writeView)

      case _: UserFun | _: VectorizeUserFun =>

        // creates a new output view for each argument
        call.args.foreach(arg => {
          val depth = getAccessDepth(arg.accessInf, arg.mem)
          View.initialiseNewView(arg.t, depth, arg.mem.variable)
        })

      case s: Scatter => buildViewScatter(s, call, writeView)
      case i: Iterate => buildViewIterate(i, call, writeView)
      case tw: TransposeW => buildViewTransposeW(tw, call, writeView)
      case t: Transpose => buildViewTranspose(t, call, writeView)
      case asVector(n) => buildViewAsVector(n, writeView)
      case _: asScalar => buildViewAsScalar(call, writeView)
      case _: Head => buildViewHead(call, writeView)
      case _: Tail => buildViewTail(call, writeView)
      case _: Zip => buildViewZip(call, writeView)
      case _: Unzip => writeView.zip()
      case l: Lambda => buildViewLambda(l, call, writeView)
      case _: Slide =>
        View.initialiseNewView(call.args.head.t, call.args.head.inputDepth, call.mem.variable)
      case _: ArrayAccess | _: UnsafeArrayAccess | _ : CheckedArrayAccess =>
        View.initialiseNewView(call.args.head.t, call.args.head.inputDepth, call.mem.variable)
      case PrintType() | Get(_) | _: Tuple | Gather(_) | Filter() |
           Pad(_, _, _) =>
        writeView
      case dunno => throw new NotImplementedError(s"OutputView.scala: $dunno")
    }

    call.outputView = result

    // then handle arguments
    call.f match {
      case Zip(_) | Tuple(_)  =>
        val res = call.args.zipWithIndex.map({ case (arg:Expr, id:Int) =>
          visitAndBuildViews(arg, result.get(id))})
        ViewTuple(res, call.argsType)

     /* case Zip(_) | Tuple(_) =>
        val res = call.args.map(arg =>
          visitAndBuildViews(arg, View.initialiseNewView(arg.t, arg.inputDepth, arg.mem.variable)))

        ViewTuple(res, call.argsType)*/
      case _: AbstractPartRed =>
        val acc = call.args.head
        visitAndBuildViews(acc, View.initialiseNewView(acc.t, acc.inputDepth, acc.mem.variable))
        visitAndBuildViews(call.args(1), result)

      case Get(i) =>
        call.args.head match {
          case param: Param =>
            buildViewGet(i, param, call)
            param.outputView
          case arg =>

            val view = arg.mem match {
              case memCollection: OpenCLMemoryCollection =>
                val subviews = getSubviews(arg, memCollection)
                subviews(i) = result
                ViewTuple(subviews, arg.t)
              case _ => result
            }

            visitAndBuildViews(arg, view)
        }

      case _: UserFun | _: VectorizeUserFun  =>



        result

      case _ =>
        assert (call.args.length == 1)
        val res = call.args.map(visitAndBuildViews(_, result))
        ViewTuple(res, call.argsType)
    }
  }

  private def buildViewZip(call: FunCall, writeView: View): View = {
    val result = writeView.unzip()

    /*call.args.zipWithIndex.foreach({
      case (arg: Param, id) if arg.outputView == NoView => arg.outputView = result.get(id)
      case _ =>
    })*/

    result
  }

  private def getAccessDepth(accessInfo: AccessInfo, memory: Memory) = {
    val contLocal = OpenCLMemory.containsLocalMemory(memory)
    val contPrivate = OpenCLMemory.containsPrivateMemory(memory)

    if (contPrivate)
      accessInfo.privateAccessInf
    else if (contLocal)
      accessInfo.localAccessInf
    else
      accessInfo.globalAccessInf
  }

 /* private def getSubviews(expr: Expr, memCollection: OpenCLMemoryCollection) = {
    if (expr.outputView != NoView)
      expr.outputView.asInstanceOf[ViewTuple].ivs.toArray
    else
      Array.fill[View](memCollection.subMemories.length)(NoView)
  }*/

  /*private def buildViewGet(i: Int, param: Param, call: FunCall): Unit = {
    param.mem match {
      case memCollection: OpenCLMemoryCollection =>
        val accessInfo =
          if (param.accessInf.l.nonEmpty) param.accessInf.l(i) else param.accessInf

        val outDepth = getAccessDepth(accessInfo, call.mem)
        val subviews = getSubviews(param, memCollection)

        if (subviews(i) == NoView)
          subviews(i) = View.initialiseNewView(call.t, outDepth, call.mem.variable)

        call.outputView = subviews(i)
        param.outputView = ViewTuple(subviews, param.t)

      case _ =>
        val outDepth = getAccessDepth (param.accessInf, param.mem)
        param.outputView = View.initialiseNewView(param.t, outDepth, param.mem.variable)
    }
  }*/

  private def buildViewUserFun(writeView: View, uf:UserFun, call: FunCall): View = {

    writeView

    /*call.args.foreach({

      case getCall@FunCall(Get(i), param: Param) =>
        buildViewGet(i, param, getCall)
      case _ =>

    })

    val newViews = call.args.map(a => {
      val depth = getAccessDepth(a.accessInf, a.mem)
      View.initialiseNewView(a.t, depth, a.mem.variable)
    })

    if (newViews.length <= 1)
      newViews.head
    else
      ViewTuple(newViews, call.argsType)*/
  }

  private def buildViewIterate(i: Iterate, call: FunCall, writeView: View): View = {
    val v = View.initialiseNewView(call.t, call.inputDepth, i.vPtrOut)
    visitAndBuildViews(i.f.body, v)

    // TODO: CD: not sure if should use the inputview instead of the mem
    View.initialiseNewView(call.t, call.outputDepth, call.args.head.mem.variable)
  }

  private def buildViewMap(m: AbstractMap, call: FunCall, writeView: View): View = {
    // traverse into call.f
    val v = visitAndBuildViews(m.f.body, writeView.access(m.loopVar))
    ViewMap(v, m.loopVar, call.args.head.t)
  }
  
  private def buildViewFilter(f: FilterSeq, call: FunCall,
                              writeView: View): View = {
    writeView
    // Output of the predicate is never stored in a variable
    visitAndBuildViews(f.f.body, writeView.access(Cst(0)))
    val outDepth = getAccessDepth(f.f.body.accessInf, f.f.body.mem)
    f.f.body.outputView = View.initialiseNewView(f.f.body.t, outDepth, f.f.body.mem.variable)
    
    // Write at the "top" of the output array
    visitAndBuildViews(f.copyFun.body, writeView.access(f.loopWrite))
    ViewMap(f.copyFun.body.outputView, f.loopWrite, call.args.head.t)
  }
  
  private def buildViewReduce(r: AbstractPartRed,
                              call: FunCall, writeView: View): View = {
    // traverse into call.f
    val v = visitAndBuildViews(r.f.body, writeView.access(Cst(0)))

    // if the reduction is a while reduction, visit and build views for the predicate
    r match {
      case rws: ReduceWhileSeq =>
        visitAndBuildViews(rws.p.body, writeView.access(Cst(0)))
      case _ =>
    }

    ViewMap(v, r.loopVar, call.args(1).t)
  }

  private def buildViewMapSeqSlide(sp: MapSeqSlide,
                                    call: FunCall, writeView: View): View = {
    visitAndBuildViews(sp.f.body, writeView.access(sp.loopVar))
    //ViewMap(sp.f.params.head.outputView, sp.loopVar, call.args.head.t)
  }


  private def buildViewSearch(s: AbstractSearch,
                              call:FunCall, writeView:View) :View = {
    visitAndBuildViews(call.args.head,
      View.initialiseNewView(call.args.head.t, call.inputDepth, call.args.head.mem.variable))
    visitAndBuildViews(s.f.body, writeView.access(Cst(0)))
    View.initialiseNewView(call.args(1).t, call.outputDepth, call.mem.variable)
  }

  private def buildViewLambda(l: Lambda, call: FunCall, writeView: View): View = {
    visitAndBuildViews(l.body, writeView)

    // TODO: Not sure about this
    //l.params.head.outputView
  }

  private def buildViewJoin(call: FunCall, writeView: View): View = {
    call.argsType match {
      case ArrayType(ArrayTypeWS(_, chunkSize)) => writeView.split(chunkSize)
      case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + call.argsType)
    }
  }

  private def buildViewSplit(n: ArithExpr, writeView: View): View = {
    writeView.join(n)
  }

  private def buildViewAsVector(n: ArithExpr, writeView: View): View = {
    writeView.asScalar()
  }

  private def buildViewAsScalar(call: FunCall, writeView: View): View = {
    call.args.head.t match {
      case ArrayType(VectorType(_, n)) => writeView.asVector(n)
      case _ => throw new IllegalArgumentException("PANIC, expected array of vectors, found " + call.argsType)
    }
  }

  private def buildViewTransposeW(tw: TransposeW, call: FunCall, writeView: View): View = {
    call.t match {
      case ArrayTypeWS(ArrayTypeWS(typ, m), n) =>
        writeView.
          join(m).
          reorder((i:ArithExpr) => { transpose(i, ArrayTypeWSWC(ArrayTypeWSWC(typ, n), m)) }).
          split(n)
      case NoType | ScalarType(_, _) | TupleType(_) | UndefType | VectorType(_, _) | ArrayType(_) =>
        throw new TypeException(call.t, "Array", call.f)
    }
  }

  private def buildViewTranspose(t: Transpose, call: FunCall, writeView: View): View = {
    call.t match {
      case ArrayTypeWS(ArrayTypeWS(_, m), n) =>
        writeView.
          join(m).
          split(n)
      case NoType | ScalarType(_, _) | TupleType(_) | UndefType | VectorType(_, _) | ArrayType(_) =>
        throw new TypeException(call.t, "Array", call.f)
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
      funCall.mem.variable)
  }*/
}
