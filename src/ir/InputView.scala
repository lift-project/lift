package ir

import opencl.ir._

import scala.collection.immutable.Stack

abstract class InputView(val t: Type = UndefType) {
  def replaced(oldExpr: ArithExpr, newExpr: ArithExpr): InputView = {
    val subst = new scala.collection.mutable.HashMap[ArithExpr,ArithExpr]()
    subst.put(oldExpr, newExpr)

    this match {
      case map: InputViewMap => new InputViewMap(map.iv.replaced(oldExpr,newExpr), map.itVar, t)
      case access: InputViewAccess => new InputViewAccess(ArithExpr.substitute(access.i, subst.toMap), access.iv.replaced(oldExpr,newExpr), t)
      case zip: InputViewZip => new InputViewZip(zip.ivs.map(_.replaced(oldExpr,newExpr)), t)
      case split: InputViewSplit => new InputViewSplit(ArithExpr.substitute(split.n, subst.toMap), split.iv.replaced(oldExpr,newExpr), t)
      case join: InputViewJoin => new InputViewJoin(ArithExpr.substitute(join.n, subst.toMap), join.iv.replaced(oldExpr,newExpr), t)
      case gather: InputViewGather => new InputViewGather(gather.f, gather.iv.replaced(oldExpr, newExpr), t)
      case asVector: InputViewAsVector => new InputViewAsVector(asVector.n, asVector.iv.replaced(oldExpr, newExpr), t)
      case asScalar: InputViewAsScalar => new InputViewAsScalar(asScalar.iv.replaced(oldExpr, newExpr), asScalar.n, t)
      case filter: InputViewFilter => new InputViewFilter(filter.iv.replaced(oldExpr, newExpr), filter.ids.replaced(oldExpr, newExpr), t)
      case component: InputViewZipComponent => new InputViewZipComponent(component.i, component.iv.replaced(oldExpr,newExpr), t)

      case  _ => this
    }
  }

  def access(idx: ArithExpr): InputView = {
    new InputViewAccess(idx, this, t.asInstanceOf[ArrayType].elemT)
  }

  def split(chunkSize : ArithExpr): InputView = {
    this.t match {
      case ArrayType(elemT, n) => new InputViewSplit(chunkSize, this,
        ArrayType(ArrayType(elemT, chunkSize), n div chunkSize))
    }
  }

  def join(chunkSize: ArithExpr): InputView = {
    this.t match {
      case ArrayType(ArrayType(elemT, n), m) => new InputViewJoin(chunkSize, this, ArrayType(elemT, n*m))
    }
  }

  def reorder(f: (ArithExpr) => ArithExpr): InputView = {
    new InputViewGather(f, this, this.t)
  }

  def asVector(n: ArithExpr): InputView = {
    t match {
      case st: ScalarType =>
        new InputViewAsVector(n, this, Type.vectorize(st, n))
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into vector types")
    }
  }

  def filter(ids: InputView): InputView = {
    new InputViewFilter(this, ids,
      ArrayType(this.t.asInstanceOf[ArrayType].elemT, ids.t.asInstanceOf[ArrayType].len))
  }

  def asScalar(): InputView = {
    t match {
      case VectorType(st, n) =>
        new InputViewAsScalar(this, n, st)
      case st: ScalarType => this
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into scalar types")
    }
  }

  def get(i: Int): InputView = {
   new InputViewZipComponent(i, this, t.asInstanceOf[TupleType].elemsT(i))
  }
}

class InputViewMem(val name: String, override val t: Type) extends InputView(t)

class InputViewAccess(val i: ArithExpr, val iv: InputView, override val t: Type) extends InputView(t)

class InputViewSplit(val n: ArithExpr, val iv: InputView, override val t: Type) extends InputView(t)

class InputViewJoin(val n: ArithExpr, val iv: InputView, override val t: Type) extends InputView(t)

class InputViewZip(val ivs: Seq[InputView], override val t: Type) extends InputView(t)

class InputViewGather(val f: ArithExpr => ArithExpr, val iv: InputView, override val t: Type) extends InputView(t)

class InputViewAsVector(val n: ArithExpr, val iv: InputView, override val t: Type) extends InputView(t)

class InputViewAsScalar(val iv: InputView, val n: ArithExpr, override val t: Type) extends InputView(t)

class InputViewFilter(val iv: InputView, val ids: InputView, override val t: Type) extends InputView(t)

class InputViewMap(val iv: InputView, val itVar: Var, override val t: Type) extends InputView(t)

class InputViewZipComponent(val i: Int, val iv: InputView, override val t: Type) extends InputView(t)

object NoView extends InputView()

object InputView {
  // create new view based on the given type
  def apply(t: Type, name: String): InputView = {
    InputView.create(name, t)
  }

  def zip(ivs: InputView*): InputView = {
    new InputViewZip(ivs, ArrayType(TupleType(ivs.map(_.t.asInstanceOf[ArrayType].elemT):_*),
      ivs(0).t.asInstanceOf[ArrayType].len))
  }

  def create(name: String, t: Type): InputView = {
    new InputViewMem(name, t)
  }

  def visitAndBuildViews(expr: Expr, outputAccessInf:  List[(ArithExpr, ArithExpr)] = List()): InputView = {
    val result = expr match {
      case pr: ParamReference => getViewAtIndex(pr.p.view, pr.i)
      case p: Param => p.view //View(expr.t, new InputAccess())
      case call: FunCall => buildViewFunCall(call, outputAccessInf)
    }
    expr.view = result
    result
  }

  private def getViewAtIndex(v: InputView, index: Int): InputView = {
    v.get(index)
  }

  private def getViewFromArgs(call: FunCall, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    if (call.args.isEmpty) {
      NoView
    } else if (call.args.length == 1) {
      visitAndBuildViews(call.args(0), outputAccessInf)
    } else {
      InputView.zip(call.args.map((expr: Expr) => visitAndBuildViews(expr, outputAccessInf)):_*)
    }
  }

  private def buildViewFunCall(call: FunCall, outputAccessInf: List[(ArithExpr, ArithExpr)]): InputView = {
    val argView = getViewFromArgs(call, outputAccessInf)

    call match {
      case call: MapCall => buildViewMapCall(call, argView, outputAccessInf)
      case call: ReduceCall => buildViewReduceCall(call, argView, outputAccessInf)
      case call: FunCall =>
        call.f match {
          case l: Lambda => buildViewLambda(l, call, argView, outputAccessInf)
          case cf: CompFunDef => buildViewCompFunDef(cf, argView, outputAccessInf)
          case z: Zip => createViewZip(z, call, argView)
          case Split(n) => createViewSplit(n, argView)
          case _: Join => createViewJoin(call, argView)
          case uf: UserFunDef => createViewUserFunDef(uf, argView, outputAccessInf)
          case ReorderStride(s) => createViewReorderStride(s, call, argView)
          case g: Gather => createViewGather(g, call, argView, outputAccessInf)
          case s: Scatter => createViewScatter(s, call, argView, outputAccessInf)
          case tL: toLocal => buildViewToLocal(tL, argView, outputAccessInf)
          case tG: toGlobal => buildViewToGlobal(tG, argView, outputAccessInf)
          case i: Iterate => buildViewIterate(i, call, argView, outputAccessInf)
          case t: Transpose => createViewTranspose(t, call, argView)
          case tw: TransposeW => createViewTransposeW(tw, call, argView, outputAccessInf)
          case asVector(n) => createViewAsVector(n, argView)
          case _: asScalar => createViewAsScalar(argView)
          case f: Filter => createViewFilter(f, call, argView)
          /*case uz: Unzip =>
          case SplitDim2(n) =>
          case j: JoinDim2 =>

          case _: Swap =>
          */
          case _ => argView
        }
    }
  }

  private def buildViewIterate(i: Iterate, call:FunCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    i.f.params(0).view = argView
    visitAndBuildViews(i.f.body, outputAccessInf)
    initialiseNewView(call.t, outputAccessInf)
  }

  private def buildViewToGlobal(tG: toGlobal, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    tG.f.params(0).view = argView
    visitAndBuildViews(tG.f.body, outputAccessInf)
  }

  private def buildViewToLocal(tL: toLocal, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    tL.f.params(0).view = argView
    visitAndBuildViews(tL.f.body, outputAccessInf)
  }

  private def buildViewMapCall(call: MapCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    // pass down input view
    call.f.f.params(0).view = argView.access(call.loopVar)
    // build information where call.f should write
    val newOutputAccessInf = (Type.getLength(call.t), call.loopVar) :: outputAccessInf

    // traverse into call.f
    val innerView = visitAndBuildViews(call.f.f.body, newOutputAccessInf)
    // get full type of the output memory object
    val fullReturnType = getFullType(call.t, outputAccessInf)

    if (call.isConcrete) {
      // create fresh input view for following function
      InputView.create(call.mem.variable.name, fullReturnType)
    } else { // call.isAbstract and return input map view
      new InputViewMap(innerView, call.loopVar, fullReturnType)
    }
  }


  private def buildViewReduceCall(call: ReduceCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    // pass down input view
    call.f.f.params(0).view = argView.get(0)
    call.f.f.params(1).view = argView.get(1).access(call.loopVar)
    // build information where call.f should write
    val newOutputAccessInf = (Type.getLength(call.t), Cst(0)) :: outputAccessInf
    // traverse into call.f
    visitAndBuildViews(call.f.f.body, newOutputAccessInf)
    // get full type of the output memory object
    val fullReturnType = getFullType(call.t, outputAccessInf)
    // create fresh input view for following function
    InputView.create(call.mem.variable.name, fullReturnType)
  }

  private def buildViewLambda(l: Lambda, call: FunCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).view = argView
    } else {
      l.params.zipWithIndex.foreach({ case (p, i) => p.view = argView.access(i) })
    }
    visitAndBuildViews(l.body, outputAccessInf)
  }

  private def buildViewCompFunDef(cf: CompFunDef, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {

    val outputAccessInfs = scala.collection.mutable.Stack(outputAccessInf)

    cf.funs.foldRight(argView)((f, v) => {
      if (f.params.length != 1) throw new NumberOfArgumentsException
      f.params(0).view = v

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

  private def createViewZip(z: Zip, call: FunCall, argView: InputView): InputView = {
    argView
  }

  private def createViewFilter(f: Filter, call: FunCall, argView: InputView): InputView = {
    argView.get(0).filter(argView.get(1))
  }

  private def createViewJoin(call: FunCall, argView: InputView): InputView = {
    val chunkSize = call.argsType match {
      case ArrayType(ArrayType(_, n), _) => n
      case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + call.argsType)
    }

    argView.join(chunkSize)
  }

  private def createViewSplit(n: ArithExpr, argView: InputView): InputView = {
    argView.split(n)
  }

  private def createViewAsVector(n: ArithExpr, argView: InputView): InputView = {
    argView.asVector(n)
  }

  private def createViewAsScalar(argView: InputView): InputView = {
    argView.asScalar()
  }

  private def createViewUserFunDef(uf: UserFunDef, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    initialiseNewView(uf.outT, outputAccessInf)
  }

  private def initialiseNewView(t: Type, outputAccessInf: List[(ArithExpr, ArithExpr)], name: String = ""): InputView = {
    // Use the lengths and iteration vars to mimic inputs
    val outArray = getFullType(t, outputAccessInf)
    val outView = InputView.create(name, outArray)
    outputAccessInf.foldRight(outView)((idx, view) => view.access(idx._2))
  }

  private def createViewReorderStride(s: ArithExpr, call: FunCall, argView: InputView): InputView = {
    val n = Type.getLength(call.argsType) / s

    argView.reorder( (i:ArithExpr) => { (i div n) + s * ( i % n) } )
  }

  private def createViewTranspose(t: Transpose, call: FunCall, argView: InputView): InputView = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
        argView.
          join(n).
          reorder((i:ArithExpr) => { IndexFunction.transpose(i, call.t) }).
          split(m)
    }
  }

  // TODO: Implement in OutputView
  private def createViewTransposeW(tw: TransposeW, call: FunCall, argView: InputView, outputAccessInf: List[(ArithExpr, ArithExpr)]): InputView = {
    call.t match {
      case ArrayType(ArrayType(typ, m), n) =>
//        findAccessAndReorder(argView, IndexFunction.transpose, call.t, 0, transpose = true)
    }

    initialiseNewView(call.t, outputAccessInf)
  }

  private def createViewGather(gather: Gather, call: FunCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    argView.reorder( (i:ArithExpr) => { gather.idx.f(i, call.t) } )
  }

  // TODO: Implement in OutputView
  private def createViewScatter(scatter: Scatter, call: FunCall, argView: InputView, outputAccessInf:  List[(ArithExpr, ArithExpr)]): InputView = {
    scatter.f.params(0).view = argView
    visitAndBuildViews(scatter.f.body, outputAccessInf)

    // Find the matching ArrayAccess to the first ArrayCreation,
    // and reorder the ArrayView in the access
//    findAccessAndReorder(scatter.f.body.view, scatter.idx, call.t, 0)

    argView
  }

  // TODO: Implement in OutputView
  /*private def findAccessAndReorder(view: InputView, idx: IndexFunction, t:Type, count: scala.Int, transpose: Boolean = false): Unit = {
    view.operation match {
      case access: ArrayAccess =>
        if (count == 1) {
          if (!transpose)
            access.av = access.av.reorder( (i:ArithExpr) => { idx.f(i, t) } )
          else
            access.av = t match {
              case ArrayType(ArrayType(typ, m), n) =>
                access.av.elemT = ArrayType(access.av.elemT.asInstanceOf[ArrayType].elemT, m)

                access.av.
                  join(m).
                  reorder((i:ArithExpr) => { IndexFunction.transpose(i, ArrayType(ArrayType(typ, n), m)) }).
                  split(n)
            }
        } else {
          findAccessAndReorder(access.av, idx, t, count-1, transpose)
        }
      case ar: ArrayReorder => findAccessAndReorder(ar.av, idx, t, count, transpose)
      case as: ArraySplit => findAccessAndReorder(as.av, idx, t, count, transpose)
      case aj: ArrayJoin => findAccessAndReorder(aj.av, idx, t, count, transpose)
      case ac: ArrayCreation => findAccessAndReorder(ac.v, idx, t, count+1, transpose)
      case op => throw new NotImplementedError(op.getClass.toString)
    }
  }*/

  private def getFullType(outputType: Type, outputAccessInf: List[(ArithExpr, ArithExpr)]): Type = {
    outputAccessInf.foldLeft(outputType)((t, len) => ArrayType(t, len._1))
  }

  def getInputAccess(sv : InputView, tupleAccessStack : Stack[Int] = new Stack()) : InputViewMem = {
    sv match {
      case map : InputViewMem => map
      case access : InputViewAccess => InputView.getInputAccess(access.iv, tupleAccessStack)
      case map : InputViewMap =>InputView.getInputAccess(map.iv, tupleAccessStack)
      case split : InputViewSplit => InputView.getInputAccess(split.iv, tupleAccessStack)
      case join : InputViewJoin => InputView.getInputAccess(join.iv, tupleAccessStack)
      case gather : InputViewGather => InputView.getInputAccess(gather.iv, tupleAccessStack)
      case filter : InputViewFilter => InputView.getInputAccess(filter.iv, tupleAccessStack)
      case asVector: InputViewAsVector => InputView.getInputAccess(asVector.iv, tupleAccessStack)
      case asScalar: InputViewAsScalar => InputView.getInputAccess(asScalar.iv, tupleAccessStack)

      case component : InputViewZipComponent =>
        val newTAS = tupleAccessStack.push(component.i)
        getInputAccess(component.iv, newTAS)

      case zip : InputViewZip =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        getInputAccess(zip.ivs(i),newTAS)

      case op => throw new NotImplementedError(op.getClass.toString)
    }
  }

}

object ViewPrinter {

  def emit(sv : InputView) : ArithExpr = {
      emitView(sv, new Stack(), new Stack())
  }

  // TODO: Continue refactoring here
  private def emitView(sv : InputView,
                       arrayAccessStack : Stack[(ArithExpr, ArithExpr)], // id, dimension size
                       tupleAccessStack : Stack[Int]) : ArithExpr = {
    sv match {
      case mem : InputViewMem =>
        assert(tupleAccessStack.isEmpty)
        arrayAccessStack.map(x => (x._1*x._2).asInstanceOf[ArithExpr]).foldLeft(Cst(0).asInstanceOf[ArithExpr])((x, y) => (x+y).asInstanceOf[ArithExpr])

      case access : InputViewAccess =>
        val length: ArithExpr = getLengthForArrayAccess(sv.t, tupleAccessStack)
        val newAAS = arrayAccessStack.push((access.i, length))
        emitView(access.iv,newAAS, tupleAccessStack)

      case map : InputViewMap =>
        val idx = arrayAccessStack.top._1
        val newAAS = arrayAccessStack.pop
        val newV = map.iv.replaced(map.itVar, idx)
        emitView(newV,newAAS,tupleAccessStack)

      case split : InputViewSplit =>
        val (chunkId,stack1) = arrayAccessStack.pop2
        val (chunkElemId,stack2) = stack1.pop2
        val newIdx = chunkId._1*split.n+chunkElemId._1
        val newAAS = stack2.push((newIdx, chunkElemId._2))
        emitView(split.iv, newAAS,tupleAccessStack)

      case join : InputViewJoin =>
        val (idx,stack) = arrayAccessStack.pop2
        val chunkSize: ArithExpr = join.n
        val chunkId = idx._1 div chunkSize
        val chunkElemId = idx._1 % chunkSize
        val newAS = stack.push((chunkElemId, Type.getLengths(sv.t.asInstanceOf[ArrayType].elemT).reduce(_*_))).
          push((chunkId, Type.getLengths(join.t.asInstanceOf[ArrayType].elemT).reduce(_*_)*join.n))
        emitView(join.iv,newAS,tupleAccessStack)

      case gather : InputViewGather =>
        val (idx,stack) = arrayAccessStack.pop2
        val newIdx = gather.f(idx._1)
        val newAS = stack.push((newIdx, idx._2))
        emitView(gather.iv,newAS,tupleAccessStack)

      case filter : InputViewFilter =>
        val ((idx, len), stack) = arrayAccessStack.pop2

        val newIdx = emit(filter.ids.access(idx))
        val indirection = new AccessVar(InputView.getInputAccess(filter.ids).name, newIdx)

        emitView(filter.iv, stack.push((indirection, len)), tupleAccessStack)

      case ta : InputViewZipComponent =>
        val newTAS = tupleAccessStack.push(ta.i)
        emitView(ta.iv,arrayAccessStack,newTAS)

      case tc : InputViewZip =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        emitView(tc.ivs(i),arrayAccessStack,newTAS)

      case asVector: InputViewAsVector =>
        val top = arrayAccessStack.top
        val newAAS = arrayAccessStack.pop.push((top._1 * asVector.n, top._2)).map(x => (x._1, x._2 / asVector.n))
        emitView(asVector.iv, newAAS, tupleAccessStack)

      case asScalar: InputViewAsScalar =>
        val top = arrayAccessStack.top
        val newAAS = arrayAccessStack.pop.push((top._1 / asScalar.n, top._2)).map(x => (x._1, x._2 * asScalar.n))
        emitView(asScalar.iv, newAAS, tupleAccessStack)

      case op => throw new NotImplementedError(op.getClass.toString)
     }
  }

  private def getLengthForArrayAccess(t: Type, tupleAccesses: Stack[Int]): ArithExpr = {

    if (tupleAccesses.isEmpty) {
      Type.getLengths(t).reduce(_*_)
    } else {
      t match {
        case tt: TupleType => getLengthForArrayAccess(Type.getTypeAtIndex(tt, tupleAccesses.top), tupleAccesses.pop)
        case ArrayType(elemT, n) => getLengthForArrayAccess(elemT, tupleAccesses) * n
      }
    }
  }
}


