package ir.view

import lift.arithmetic.{ArithExpr, Cst}
import ir._
import ir.ast._
import ir.view.OutputView.Mode
import opencl.ir.{GlobalMemory, LocalMemory, OpenCLAddressSpace, PrivateMemory}
import opencl.ir.pattern._

/**
 * A helper object for constructing views.
 *
 * Visits the expressions left to right and builds the views for all
 * sub-expressions that write to memory.
 */
object OutputView {

  object Mode extends Enumeration {
    val Global, Local, Private = Value
  }

  /**
    * Build output views for the expression.
    *
    * @param expr Expression to build views for
    */
  def apply(expr: Expr): Unit = {

    val itDimNum = ItSpaceDimCount(expr)
    //InferPtrOutType(expr, itDimNum)
    BuildItVarInfo(expr, itDimNum)

    Mode.values.foreach({ mode =>
      val ov = new OutputViewBuilder(mode, itDimNum)
      expr match {
        case fc: FunCall =>
          val initialOV = mode match {
            case Mode.Global => Some(ViewMem(expr.mem.variable, expr.t))
            case _ => None
          }
          ov.visitAndBuildViews(expr, initialOV)
        case _ => // if the expression is not a funcall, there is no output view
      }
    })

  }

}

class OutputViewBuilder(val mode: Mode.Value, val itSpaceDim: Int) {


  private val paramViews : scala.collection.mutable.Map[Param, Option[View]] = scala.collection.mutable.Map()

  private var wrgDepth = 0
  private var glbOrLclDepth = 0


  private def depthAchieved() : Boolean = {
    mode match {
      case Mode.Global => true
      case Mode.Local => wrgDepth == itSpaceDim
      case Mode.Private => glbOrLclDepth == itSpaceDim
    }
  }

  /*private def wrapPtrType(outPtrType: Type, outputView: View) : (Type,View) = {
    mode match {
      case Mode.Global => outPtrType // TODO: add outer dimension from nesting info
      case Mode.Local => wrgDepth == itSpaceDim
      case Mode.Private => glbOrLclDepth == itSpaceDim
    }
  }*/

  /*private def getPtrType(outPtrType: Option[Type], currentType: Type) : Option[Type] = {
    mode match {
      case Mode.Global => if (outPtrType.nonEmpty) outPtrType else Some(currentType) // TODO: add outer dimension from nesting info
      case Mode.Local => wrgDepth == itSpaceDim
      case Mode.Private => glbOrLclDepth == itSpaceDim
    }
  }*/

  private def addrSpaceMatchMode(addressSpace: OpenCLAddressSpace) : Boolean = {
    mode match {
      case Mode.Global => addressSpace == GlobalMemory
      case Mode.Local => addressSpace == LocalMemory
      case Mode.Private => addressSpace == PrivateMemory
    }
  }

  def visitAndBuildViews(expr: Expr, outputView: Option[View]): Option[View] = {
    expr match {
      case call: FunCall => buildViewFunCall(call, outputView)
      case p: Param =>

        /* Retrieve the buildView function associated with the Param.
        *  A new buildView function is created which first call the current buildView function and only then call the buildView function associated with the Param.
        *  The buildView function from the param has to be called last, since this corresponds to the output view created for the argument of a lambda. */

        val pView = paramViews.get(p)
        if (pView.isEmpty)
          // the param was probably implicit (e.g. Map)
          outputView
        else
          pView.get

      case e: Expr=> outputView
    }
  }


  private def getItVarsAndArrayFun(e: Expr) = {
    e.addressSpace match {
      case GlobalMemory => e.writeItVarsAndArrayFun(0) // TODO: use parallel version, but need to make sure Inputview uses the same logic
      case LocalMemory => e.writeItVarsAndArrayFun(1)  // TODO: use parallel version, but need to make sure Inputview uses the same logic
      case PrivateMemory => e.writeItVarsAndArrayFunParallel(2)
    }
  }

    /**
    *
    * @param call
    * @return
    */
  private def buildViewFunCall(call: FunCall, outputView: Option[View]) : Option[View] = {

    outputView match {
      case Some(ov) => Some(ov)
      case None =>
        if (call.isConcrete)
          // we allocate the view now

    }



    call.f match {

      case _: UserFun | _: VectorizeUserFun =>
        //val viewMemNew = ViewMem(call.mem.variable, call.outPtrType)
        //assert (call.outPtrType != NoType)
        //val viewMem = ViewMem(call.mem.variable, call.outPtrType)

        if (depthAchieved && addrSpaceMatchMode(call.addressSpace)) {

          val ufOutputView = outputView match {
            case Some(v) => v
            case None =>
              //val v1 = View.initialiseNewView(call.t, call.outputDepth,/*
              //this.mode match {
              //  case Mode.Global => call.accessInf.globalAccessInf
              //  case Mode.Local => call.accessInf.localAccessInf
              //  case Mode.Private => call.accessInf.privateAccessInf
              //},*/ call.mem.variable)
              View.initialiseNewOutputView2(call.t, getItVarsAndArrayFun(call), call.mem.variable)
          }

          call.outputView = ufOutputView
          assert(call.outputView.t == call.t)
        }

        call.args.map(arg => visitAndBuildViews(arg, None))
        None
        //((v: View) => ViewTuple(argsViews.map(f => f._1(v)), call.argsType).unzip(), None)


      case m: AbstractMap =>

        //val da = depthAchieved()

        m match {
          case _: MapWarp | _:MapGlb | _ : MapLcl | _: MapAtomLcl => glbOrLclDepth+=1
          case _: MapWrg => wrgDepth+=1
          case _ =>
        }

        val mapFOutputView = visitAndBuildViews(m.f.body, outputView match {
          case Some(ov) => Some(ov.access(m.loopVar))
          case None => None})//if (da) (v: View) => outputView(v).access(m.loopVar) else buildView, outPtrType)

        m match {
          case _: MapWarp | _:MapGlb | _ : MapLcl | _: MapAtomLcl => glbOrLclDepth-=1
          case _: MapWrg => wrgDepth-=1
          case _ =>
        }

        visitAndBuildViews(call.args.head, mapFOutputView match {
          case Some(ov) => Some(ViewMap(ov, m.loopVar, call.args.head.t))
          case None => None
        })

          //if (mapOutputViewFun._2.nonEmpty) (v: View) => ViewMap(mapOutputViewFun._1(v), m.loopVar, call.args.head.t) else buildView, mapOutputViewFun._2)

      case z: Zip =>

        val argsViews = call.args.map(arg => visitAndBuildViews(arg, outputView))

        // all the argument must either be None or all contain some view
        val cntSome = argsViews.count({
            case None => false
            case Some(_) => true
          })
        assert(cntSome == 0 || cntSome == argsViews.length)

        if (cntSome == 0)
          None
        else
          Some(ViewTuple(argsViews.map(av => av.get), call.argsType).unzip())

      case uz: Unzip =>
        visitAndBuildViews(call.args.head,
          outputView match {
            case Some(ov) => Some(ov.zip())
            case None => None
          })

      case _:Tuple =>

        val argsViews = call.args.map(arg => visitAndBuildViews(arg, outputView))

        // all the argument must either be None or all contain some view
        val cntSome = argsViews.count({
          case None => false
          case Some(_) => true
        })
        assert(cntSome == 0 || cntSome == argsViews.length)

        if (cntSome == 0)
          None
        else
          Some(ViewTuple(argsViews.map(av => av.get), call.argsType))

      case Get(i) =>

        // TODO: do we encounter this case??
        outputView match {
          case Some(_) => throw new IllegalArgumentException("Get(i) encountered when building an output view")
          case None =>
        }
        visitAndBuildViews(call.args.head, outputView)


      /* val arg = call.args.head
       val tt: TupleType = arg.t match {
         case tt:TupleType => tt
       }

       visitAndBuildViews(arg,
         Some(ViewTuple(
           for (argI <- 0 until tt.elemsT.length) yield
             if (argI == i)
               outputView.get
             else NoView , call.argsType))
*/

/*



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
*/



      case _:asVector =>
        visitAndBuildViews(call.args.head,
          outputView match {
            case Some(ov) => Some(ov.asScalar())
            case None => None
          })

      case _: asScalar =>

        call.args.head.t match {
          case ArrayType(VectorType(_, n)) =>
            visitAndBuildViews(call.args.head,
              outputView match {
                case Some(ov) => Some(ov.asVector(n))
                case None => None
              })
          case _ => throw new IllegalArgumentException("PANIC, expected array of vectors, found " + call.argsType)
        }

      case l: Lambda =>
        /* We first visit each arguments and remember what the output view is.
           We then visit the body of the lambda.
           When a param is encountered, the corresponding output view will be retrieved and a new output view constructed (see visitAndBuildViews).
         */

        val lParamViews: scala.collection.immutable.Map[Param, Option[View]] =
          l.params.zip(call.args).map({ case (param,arg) => param -> visitAndBuildViews(arg, None)}).toMap

        assert (paramViews.keySet.intersect(lParamViews.keySet).isEmpty)
        paramViews ++= lParamViews
        val resultFun = visitAndBuildViews(l.body, outputView)
        paramViews --= lParamViews.keys

        resultFun


      case r: AbstractPartRed =>

        // if the reduction is a while reduction, visit and build views for the predicate
        r match {
          case rws: ReduceWhileSeq =>
            visitAndBuildViews(rws.p.body, None)
              /*outputView match {
                case Some(ov) => Some(ov.access(Cst(0))) // TODO: not sure if this is correct
                case None => None
              })*/
          case _ =>
        }

        // deal with the accumulator
        val acc = call.args(0)
        visitAndBuildViews(acc, outputView)

        // deal with f
        val redOutputView = visitAndBuildViews(r.f.body,
          outputView match {
            case Some(ov) => Some(ov.access(Cst(0)))
            case None => None
          })

        // deal with the input argument
        val input = call.args(1)
        visitAndBuildViews(input,
          redOutputView match {
            case Some(ov) => Some(ViewMap(ov, r.loopVar, call.args.head.t))
            case None => None
          })


      case _: toGlobal | _: toLocal | _:toPrivate =>
        val fp = call.f.asInstanceOf[FPattern]

        val fpOutputView = visitAndBuildViews(fp.f.body, outputView)
        visitAndBuildViews(call.args.head, fpOutputView)

      case s: Scatter =>

        val scatterOutputView = outputView match {
          case Some(v) => v
          case None =>
            View.initialiseNewOutputView2(call.t, getItVarsAndArrayFun(call), call.mem.variable)
          //View.initialiseNewView(call.t, call.outputDepth, call.mem.variable)
        }
        visitAndBuildViews(call.args.head, Some(scatterOutputView.reorder((i: ArithExpr) => s.idx.f(i, call.t))))

      case g: Gather =>
        outputView match {
          case Some(_) => throw new IllegalArgumentException("Gather encountered when building an output view")
          case None =>
        }
        visitAndBuildViews(call.args.head, outputView)

      case Split(n) =>
        visitAndBuildViews(call.args.head,
          outputView match {
            case Some(ov) => Some(ov.join(n))
            case None => None
          })

      case _: Join =>
        val arg = call.args.head
        arg.t match {
          case ArrayType(ArrayTypeWS(_, chunkSize)) => visitAndBuildViews(arg,
            outputView match {
              case Some(ov) => Some(ov.split(chunkSize))
              case None => None
            })
          case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + arg.t)
        }
      case tw: TransposeW =>

        val tWOutputView = outputView match {
          case Some(v) => v
          case None =>
            View.initialiseNewOutputView2(call.t, getItVarsAndArrayFun(call), call.mem.variable)
            //View.initialiseNewView(call.t, call.outputDepth, call.mem.variable)
        }

        visitAndBuildViews(call.args.head,
          call.t match {
            case ArrayTypeWS(ArrayTypeWS(et, m), n) =>
              Some(tWOutputView.
                join(m).
                reorder((i: ArithExpr) => {
                  transpose(i, ArrayTypeWSWC(ArrayTypeWSWC(et, n), m))
                }).
                split(n))
            case _ =>
              throw new TypeException(call.t, "ArrayTypeWS", call.f)
          }
        )

      case t: Transpose =>
        outputView match {
          case Some(ov) => throw new IllegalArgumentException("Transpose encountered when building an output view")
          case None => visitAndBuildViews(call.args.head, None)
        }


      case _:Tail =>

        // TODO: check that this works: TestTail.tailBetweenMapsScatterAfter
        // TODO: check that this works: TestTail.tailBetweenMapsScatterBeforeAndAfter

        outputView match {
          case Some(ov) => throw new IllegalArgumentException("Tail encountered when building an output view")
          case None => visitAndBuildViews(call.args.head, None)
        }

      case _: Head =>

        outputView match {
          case Some(ov) => throw new IllegalArgumentException("Head encountered when building an output view")
          case None => visitAndBuildViews(call.args.head, None)
        }

      case i: Iterate =>

        val innerFunOutView =
          //Some(View.initialiseNewView(call.t, call.outputDepth, i.vPtrOut))
          Some(View.initialiseNewOutputView2(call.t, getItVarsAndArrayFun(call), i.vPtrOut))
        visitAndBuildViews(i.f.body, innerFunOutView)

        visitAndBuildViews(call.args.head, None)


/*
      case sp: MapSeqSlide => buildViewMapSeqSlide(sp, call, writeView)
      case s: AbstractSearch => buildViewSearch(s, call, writeView)

      case fp: FPattern => buildViewLambda(fp.f, call, writeView)
      case _: Slide =>
        View.initialiseNewView(call.args.head.t, call.args.head.inputDepth, call.mem.variable)
      case _: ArrayAccess | _: UnsafeArrayAccess | _ : CheckedArrayAccess =>
        View.initialiseNewView(call.args.head.t, call.args.head.inputDepth, call.mem.variable)
      case PrintType() | Get(_) | _: Tuple | Gather(_) | Filter() =>
        writeView*/
      case _:PrintType | _:Pad =>
        visitAndBuildViews(call.args.head, outputView)

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
