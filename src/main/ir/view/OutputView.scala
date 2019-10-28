package ir.view

import backends.{Backend, OpenCLBackend, SpatialBackend}
import backends.common.view.{AccessInfo, SingleAccess}
import backends.spatial.accel.ir.pattern.{AbstractSpFold, SpForeach, asLiteral}
import backends.spatial.common.ir.view.AccessInfoSp
import ir._
import ir.ast._
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.ir.pattern.{FilterSeq, InsertionSortSeq, MapSeqSlide, ReduceWhileSeq, ScanSeq}
import opencl.ir.{GlobalMemory, LocalMemory, OpenCLMemory, PrivateMemory}

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

    // Reset outputView for every expression
    Expr.visit(expr, _.outputView = NoView, _ => {})

    expr.outputView = View(expr.t, expr.mem.variable)
    visitAndBuildViews(expr, expr.outputView)
  }

  private def visitAndBuildViews(expr: Expr, writeView: View): View = {
    expr match {
      case call: FunCall => buildViewFunCall(call, writeView)
      case e: Expr =>

        if (e.outputView == NoView)
          e.outputView = writeView

        e.outputView
    }
  }

  private def buildViewFunCall(call: FunCall, writeView: View): View = {
    // first handle body
    val result = call.f match {
      case sF: SpForeach            => buildViewSpForeach(sF, call, writeView)
      case m: AbstractMap           => buildViewMap(m, call, writeView)
      case f: FilterSeq             => buildViewFilter(f,  call, writeView)
      case aSF: AbstractSpFold      => buildViewAbstrSpFold(aSF, call, writeView)
      case r: AbstractPartRed       => buildViewReduce(r, call, writeView)
      case sp: MapSeqSlide          => buildViewMapSeqSlide(sp, call, writeView)
      case s: AbstractSearch        => buildViewSearch(s, call, writeView)
      case scan:ScanSeq             => buildViewScan(scan, call, writeView)
      case iss: InsertionSortSeq    => buildViewSort(iss, call, writeView)
      case Split(n)                 => buildViewSplit(n, writeView)
      case _: Join                  => buildViewJoin(call, writeView)
      case uf: UserFun              => buildViewUserFun(writeView, uf, call)
      case uf: VectorizeUserFun     => buildViewUserFun(writeView, uf.userFun, call)
      case s: Scatter               => buildViewScatter(s, call, writeView)
      case i: Iterate               => buildViewIterate(i, call, writeView)
      case tw: TransposeW           => buildViewTransposeW(tw, call, writeView)
      case t: Transpose             => buildViewTranspose(t, call, writeView)
      case asVector(n)              => buildViewAsVector(n, writeView)
      case _: asScalar              => buildViewAsScalar(call, writeView)
      case _: Head                  => buildViewHead(call, writeView)
      case _: Tail                  => buildViewTail(call, writeView)
      case _: Zip                   => buildViewZip(call, writeView)
      case _: Unzip                 => writeView.zip()
      case l: Lambda                => buildViewLambda(l, call, writeView)
      case fp: FPattern             => buildViewLambda(fp.f, call, writeView)
      case cc: Concat               => buildViewConcat(call, View.initialiseNewView(call.t, call.inputDepth, call.mem.variable))
      case _: Slide                 =>
        View.initialiseNewView(call.args.head.t, call.args.head.inputDepth, call.args.head.mem.variable)
      case _: ArrayAccess | _: UnsafeArrayAccess | _ : CheckedArrayAccess =>
        View.initialiseNewView(call.args.head.t, call.args.head.inputDepth, call.args.head.mem.variable)
      case RewritingGuidePost(_)    => writeView
      case _: asLiteral             => writeView
      case debug.PrintType(_) | debug.PrintComment(_) | debug.AssertType(_, _) | Get(_) | _: Tuple | Gather(_) | 
           Filter() | Pad(_, _, _) | PadConstant(_, _, _) | Id()
                                    => writeView
      case dunno                    => throw new NotImplementedError(s"OutputView.scala: $dunno")
    }

    // then handle arguments
    call.f match {
      case Zip(_) | Tuple(_) =>
        val res = call.args.map(arg =>
          visitAndBuildViews(arg, View.initialiseNewView(arg.t, arg.inputDepth, arg.mem.variable)))

        ViewTuple(res, call.argsType)
      case Concat(_) =>
        // recurse into arguments by passing the modified output view along
        val res = call.args.map(arg => visitAndBuildViews(arg, arg.outputView))
        ViewTuple(res, call.argsType)
      case _: AbstractSpFold =>
        val acc = call.args.head
        visitAndBuildViews(acc, View.initialiseNewView(acc.t, acc.inputDepth, acc.mem.variable))
        visitAndBuildViews(call.args(1), result)
      case _: AbstractPartRed =>
        val acc = call.args.head
        visitAndBuildViews(acc, View.initialiseNewView(acc.t, acc.inputDepth, acc.mem.variable))
        visitAndBuildViews(call.args(1), result)
      case _: ScanSeq =>
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
              case memCollection: MemoryCollection =>
                val subviews = getSubviews(arg, memCollection)
                subviews(i) = result
                ViewTuple(subviews, arg.t)
              case _ => result
            }

            visitAndBuildViews(arg, view)
        }
      case _: UserFun | _: VectorizeUserFun if result.isInstanceOf[ViewTuple] =>

        val subviews = result.asInstanceOf[ViewTuple].ivs
        (call.args, subviews).zipped.map((a,v) => visitAndBuildViews(a, v))
        result

      // TODO: Also lambdas?
      case fp: FPattern if fp.f.params.length > 1 && !fp.isInstanceOf[InsertionSortSeq] =>

        (call.args, fp.f.params).zipped.map((arg, param) => {
          visitAndBuildViews(arg, param.outputView)})

        result
      case _ =>

        val res = call.args.map(visitAndBuildViews(_, result))
        ViewTuple(res, call.argsType)
    }
  }

  private def buildViewZip(call: FunCall, writeView: View): View = {
    val result = writeView.unzip()

    call.args.zipWithIndex.foreach({
      case (arg: Param, id) if arg.outputView == NoView => arg.outputView = result.get(id)
      case _ =>
    })

    result
  }

  private def buildViewConcat(call: FunCall, writeView: View): View = {

    var accCapacity : ArithExpr = Cst(0)

    call.args.foreach({
      case (arg) if arg.outputView == NoView => arg.outputView = writeView.offset(accCapacity)
        accCapacity = accCapacity +
          (arg.t match{
          case ArrayTypeWSWC(_,_,c) => c
        })
      case _ => throw new IllegalArgumentException("PANIC: No output view required!")
    })
   // println(writeView)
//    writeView
    View.initialiseNewView(call.t, call.outputDepth, call.mem.variable)
  }

  private def getAccessDepth(accessInfo: AccessInfo, memory: Memory): List[SingleAccess] = {
    Backend() match {
      case OpenCLBackend => getAccessDepthCL(accessInfo.asInstanceOf[AccessInfoCL], memory)
      case SpatialBackend => getAccessDepthSp(accessInfo.asInstanceOf[AccessInfoSp], memory)
    }
  }

  private def getAccessDepthCL(accessInfo: AccessInfoCL, memory: Memory): List[SingleAccess] = {
    val contLocal = OpenCLMemory.containsLocalMemory(memory)
    val contPrivate = OpenCLMemory.containsPrivateMemory(memory)

    if (contPrivate)
      accessInfo.accessInf(PrivateMemory)
    else if (contLocal)
      accessInfo.accessInf(LocalMemory)
    else
      accessInfo.accessInf(GlobalMemory)
  }

  private def getAccessDepthSp(accessInfo: AccessInfoSp, memory: Memory): List[SingleAccess] = {
    if (accessInfo.accessInf.contains(memory)) accessInfo.accessInf(memory)
    else List() //throw new IllegalArgumentException("Unknown or no memory")
  }

  private def getSubviews(expr: Expr, memCollection: MemoryCollection) = {
    if (expr.outputView != NoView)
      expr.outputView.asInstanceOf[ViewTuple].ivs.toArray
    else
      Array.fill[View](memCollection.subMemories.length)(NoView)
  }

  private def buildViewGet(i: Int, param: Param, call: FunCall): Unit = {
    param.mem match {
      case memCollection: MemoryCollection =>
        val accessInfo =
          if (param.accessInf.collection.nonEmpty) param.accessInf.collection(i) else param.accessInf

        val outDepth = getAccessDepth(accessInfo, call.mem)
        val subviews = getSubviews(param, memCollection)

        if (subviews(i) == NoView)
          subviews(i) = View.initialiseNewView(call.t, outDepth, call.mem.variable)

        call.outputView = subviews(i)
        param.outputView = ViewTuple(subviews, param.t)

      case _ =>
        val outDepth = getAccessDepth(param.accessInf, param.mem)
        param.outputView = View.initialiseNewView(param.t, outDepth, param.mem.variable)
    }
  }

  private def buildViewUserFun(writeView: View, uf:UserFun, call: FunCall): View = {

    call.outputView = writeView

    call.args.foreach({
      case p: Param  =>
        val outDepth = getAccessDepth(p.accessInf, p.mem)
        p.outputView = View.initialiseNewView(p.t, outDepth, p.mem.variable)

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
      ViewTuple(newViews, call.argsType)
  }

  private def buildViewIterate(i: Iterate, call: FunCall, writeView: View): View = {
    val v = View.initialiseNewView(call.t, call.inputDepth, i.vPtrOut)
    visitAndBuildViews(i.f.body, v)
    View.initialiseNewView(call.args.head.t, call.outputDepth, call.args.head.mem.variable)
  }

  private def buildViewSpForeach(sF: SpForeach, call: FunCall, writeView: View): View = {
    // traverse into call.f
    visitAndBuildViews(sF.f.body, writeView.access(sF.loopVar))
    // The implied Map view is ViewMap, but the implied Slide does not need
    // the outer write view, so there is no need to build ViewMap
//    ViewMap(sF.f.params.head.outputView, sF.loopVar, call.args.head.t)

    // build the implied Slide view
    val slideWriteView = View.initialiseNewView(call.args.head.t, call.args.head.inputDepth, call.args.head.mem.variable)

    val argViews = call.args.map(visitAndBuildViews(_, slideWriteView))
    ViewTuple(argViews, call.argsType)
  }


  private def buildViewMap(m: AbstractMap, call: FunCall, writeView: View): View = {
    // traverse into call.f
    visitAndBuildViews(m.f.body, writeView.access(m.loopVar))
    ViewMap(m.f.params.head.outputView, m.loopVar, call.args.head.t)
  }
  
  private def buildViewFilter(f: FilterSeq, call: FunCall,
                              writeView: View): View = {
    visitAndBuildViews(f.f.body, writeView.access(Cst(0)))
    f.f.body.outputView = View.initialiseNewView(f.f.body.t, List(), f.f.body.mem.variable)
    ViewMap(f.f.params.head.outputView, f.loopWrite, call.args.head.t)
  }

  private def buildViewAbstrSpFold(asf: AbstractSpFold,
                                   call: FunCall, writeView: View): View = {
    // fReduce: traverse into call.f
    visitAndBuildViews(asf.fReduce.body, writeView.access(Cst(0)))

    // Reduce output view is the one of the output memory of fMap
    // (for one iteration of fMap -- the Spatial will take care of unrolling this memory)
    val outViewFromReduce = View.initialiseNewView(asf.fMap.body.t,
      getAccessDepth(asf.fMap.body.accessInf, asf.fMap.body.mem), asf.fMap.body.mem.variable)

    // fMap: traverse into call.f
    visitAndBuildViews(asf.fMap.body, outViewFromReduce)//.split(asf.chunkSize).access(asf.mapLoopVar))

    // The implied Map view is ViewMap, but the implied Slide does not need
    // the outer write view, so there is no need to build ViewMap
    //    ViewMap(asf.f.params(1).outputView, asf.mapLoopVar, call.args.head.t)

    // build the implied Slide view
    val slideWriteView = View.initialiseNewView(call.args(1).t, call.args(1).inputDepth, call.args(1).mem.variable)
    slideWriteView
  }
  
  private def buildViewReduce(r: AbstractPartRed,
                              call: FunCall, writeView: View): View = {
    // traverse into call.f
    visitAndBuildViews(r.f.body, writeView.access(Cst(0)))

    // if the reduction is a while reduction, visit and build views for the predicate
    r match {
      case rws: ReduceWhileSeq =>
        visitAndBuildViews(rws.p.body, writeView.access(Cst(0)))
      case _ =>
    }

    ViewMap(r.f.params(1).outputView, r.loopVar, call.args(1).t)
  }

  private def buildViewMapSeqSlide(sp: MapSeqSlide,
                                    call: FunCall, writeView: View): View = {
    visitAndBuildViews(sp.f.body, writeView.access(sp.loopVar))
    ViewMap(sp.f.params.head.outputView, sp.loopVar, call.args.head.t)
  }


  private def buildViewSearch(s: AbstractSearch,
                              call:FunCall, writeView:View) :View = {
    visitAndBuildViews(call.args.head,
      View.initialiseNewView(call.args.head.t, call.inputDepth, call.args.head.mem.variable))
    visitAndBuildViews(s.f.body, writeView.access(Cst(0)))
    View.initialiseNewView(call.args(1).t, call.outputDepth, call.args(1).mem.variable)
  }

  private def buildViewScan(scan: ScanSeq, call:FunCall, writeView:View) : View = {
    visitAndBuildViews(scan.f.body, scan.f.params.head.view /* unsure here */)
    ViewMap(scan.f.params(1).outputView, scan.loopVar, call.args(1).t)
  }

  private def buildViewLambda(l: Lambda, call: FunCall, writeView: View): View = {
    visitAndBuildViews(l.body, writeView)
    // TODO: Not sure about this
    //l.body.outputView = writeView
    l.params.head.outputView
  }
  
  private def buildViewSort(iss: InsertionSortSeq,
                            call: FunCall,
                            writeView: View): View = {
    // Note: at this point, we can set the input view for the first argument
    //       of the comparison function as an access to the output array of
    //       the pattern.
    //       cf. `InputView.buildViewSort`
    iss.f.params(1).view = writeView.access(iss.loopWrite)
    InputView(iss.f.body)
    val compareOutputView = View.initialiseNewView(
      iss.f.body.t,
      getAccessDepth(iss.f.body.accessInf, iss.f.body.mem),
      Var("comp")
    )
    visitAndBuildViews(iss.f.body, compareOutputView)
    
    View.initialiseNewView(call.t, call.outputDepth, call.mem.variable)
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
        writeView.transpose
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
      funCall.args.head.mem.variable)
  }
}
