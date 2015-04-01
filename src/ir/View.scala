package ir

import opencl.ir._

import scala.collection.immutable.Stack

abstract class View(val t: Type = UndefType) {
  def replaced(oldExpr: ArithExpr, newExpr: ArithExpr): View = {
    val subst = new scala.collection.mutable.HashMap[ArithExpr,ArithExpr]()
    subst.put(oldExpr, newExpr)

    this match {
      case map: ViewMap => new ViewMap(map.iv.replaced(oldExpr,newExpr), map.itVar, t)
      case access: ViewAccess => new ViewAccess(ArithExpr.substitute(access.i, subst.toMap), access.iv.replaced(oldExpr,newExpr), t)
      case zip: ViewZip => new ViewZip(zip.ivs.map(_.replaced(oldExpr,newExpr)), t)
      case split: ViewSplit => new ViewSplit(ArithExpr.substitute(split.n, subst.toMap), split.iv.replaced(oldExpr,newExpr), t)
      case join: ViewJoin => new ViewJoin(ArithExpr.substitute(join.n, subst.toMap), join.iv.replaced(oldExpr,newExpr), t)
      case gather: ViewGather => new ViewGather(gather.f, gather.iv.replaced(oldExpr, newExpr), t)
      case asVector: ViewAsVector => new ViewAsVector(asVector.n, asVector.iv.replaced(oldExpr, newExpr), t)
      case asScalar: ViewAsScalar => new ViewAsScalar(asScalar.iv.replaced(oldExpr, newExpr), asScalar.n, t)
      case filter: ViewFilter => new ViewFilter(filter.iv.replaced(oldExpr, newExpr), filter.ids.replaced(oldExpr, newExpr), t)
      case tuple: ViewTuple => new ViewTuple(tuple.ivs.map(_.replaced(oldExpr, newExpr)), t)
      case component: ViewTupleComponent => new ViewTupleComponent(component.i, component.iv.replaced(oldExpr,newExpr), t)

      case  _ => this
    }
  }

  def access(idx: ArithExpr): View = {
    new ViewAccess(idx, this, t.asInstanceOf[ArrayType].elemT)
  }

  def split(chunkSize : ArithExpr): View = {
    this.t match {
      case ArrayType(elemT, n) => new ViewSplit(chunkSize, this,
        ArrayType(ArrayType(elemT, chunkSize), n div chunkSize))
    }
  }

  def join(chunkSize: ArithExpr): View = {
    this.t match {
      case ArrayType(ArrayType(elemT, n), m) =>
        new ViewJoin(chunkSize, this, ArrayType(elemT, n*m))
    }
  }

  def reorder(f: (ArithExpr) => ArithExpr): View = {
    new ViewGather(f, this, this.t)
  }

  def asVector(n: ArithExpr): View = {
    t match {
      case ArrayType(st: ScalarType, len) =>
        new ViewAsVector(n, this, ArrayType(Type.vectorize(st, n), len))
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into vector types")
    }
  }

  def filter(ids: View): View = {
    new ViewFilter(this, ids,
      ArrayType(this.t.asInstanceOf[ArrayType].elemT, ids.t.asInstanceOf[ArrayType].len))
  }

  def asScalar(): View = {
    t match {
      case ArrayType(VectorType(st, n), len) =>
        new ViewAsScalar(this, n, ArrayType(st, len))
      case st: ScalarType => this
      case _ => throw new IllegalArgumentException("PANIC: Can't convert elements of type " + t + " into scalar types")
    }
  }

  def get(i: Int): View = {
    new ViewTupleComponent(i, this, t.asInstanceOf[TupleType].elemsT(i))
  }

  def zip(): View = {
    this match {
      case tuple: ViewTuple =>
        new ViewZip(tuple.ivs, ArrayType(TupleType(tuple.ivs.map(_.t.asInstanceOf[ArrayType].elemT):_*),
          tuple.ivs.head.t.asInstanceOf[ArrayType].len))
      case other => throw new IllegalArgumentException("Can't zip " + other.getClass)
    }
  }
}

class ViewMem(val name: String, override val t: Type) extends View(t)

class ViewAccess(val i: ArithExpr, val iv: View, override val t: Type) extends View(t)

class ViewSplit(val n: ArithExpr, val iv: View, override val t: Type) extends View(t)

class ViewJoin(val n: ArithExpr, val iv: View, override val t: Type) extends View(t)

class ViewZip(val ivs: Seq[View], override val t: Type) extends View(t)

class ViewGather(val f: ArithExpr => ArithExpr, val iv: View, override val t: Type) extends View(t)

class ViewAsVector(val n: ArithExpr, val iv: View, override val t: Type) extends View(t)

class ViewAsScalar(val iv: View, val n: ArithExpr, override val t: Type) extends View(t)

class ViewFilter(val iv: View, val ids: View, override val t: Type) extends View(t)

class ViewMap(val iv: View, val itVar: Var, override val t: Type) extends View(t)

class ViewTupleComponent(val i: Int, val iv: View, override val t: Type) extends View(t)

class ViewTuple(val ivs: Seq[View], override val t: Type) extends View(t)

object NoView extends View()

object View {
  // create new view based on the given type
  def apply(t: Type, name: String): View = {
    View.create(name, t)
  }

  def tuple(ivs: View*) = {
    new ViewTuple(ivs, TupleType(ivs.map(_.t):_*))
  }

  def create(name: String, t: Type): View = {
    new ViewMem(name, t)
  }

  def getInputAccess(sv : View, tupleAccessStack : Stack[Int] = new Stack()) : ViewMem = {
    sv match {
      case map : ViewMem => map
      case access : ViewAccess => View.getInputAccess(access.iv, tupleAccessStack)
      case map : ViewMap =>View.getInputAccess(map.iv, tupleAccessStack)
      case split : ViewSplit => View.getInputAccess(split.iv, tupleAccessStack)
      case join : ViewJoin => View.getInputAccess(join.iv, tupleAccessStack)
      case gather : ViewGather => View.getInputAccess(gather.iv, tupleAccessStack)
      case filter : ViewFilter => View.getInputAccess(filter.iv, tupleAccessStack)
      case asVector: ViewAsVector => View.getInputAccess(asVector.iv, tupleAccessStack)
      case asScalar: ViewAsScalar => View.getInputAccess(asScalar.iv, tupleAccessStack)

      case component : ViewTupleComponent =>
        val newTAS = tupleAccessStack.push(component.i)
        getInputAccess(component.iv, newTAS)

      case zip : ViewZip =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        getInputAccess(zip.ivs(i),newTAS)

      case tuple: ViewTuple =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        getInputAccess(tuple.ivs(i),newTAS)

      case op => throw new NotImplementedError(op.getClass.toString)
    }
  }
}

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
          //          case tw: TransposeW => buildViewTransposeW(tw, call, argView, outputAccessInf)
          case asVector(n) => buildViewAsVector(n, argView)
          case _: asScalar => buildViewAsScalar(argView)
          case f: Filter => buildViewFilter(f, call, argView)
          /*case uz: Unzip =>
          case SplitDim2(n) =>
          case j: JoinDim2 =>

          case _: Swap =>
          */
          case _ => argView
        }
    }
  }

  private def buildViewIterate(i: Iterate, call:FunCall, argView: View, outputAccessInf:  List[(ArithExpr, ArithExpr)]): View = {
    i.f.params(0).view = argView
    visitAndBuildViews(i.f.body, outputAccessInf)
    initialiseNewView(call.t, outputAccessInf)
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
      initialiseNewView(call.t, outputAccessInf, call.mem.variable.name)
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
    initialiseNewView(call.t, outputAccessInf, call.mem.variable.name)
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

  private def initialiseNewView(t: Type, outputAccessInf: List[(ArithExpr, ArithExpr)], name: String = ""): View = {
    // Use the lengths and iteration vars to mimic inputs
    val outArray = getFullType(t, outputAccessInf)
    val outView = View.create(name, outArray)
    outputAccessInf.foldRight(outView)((idx, view) => view.access(idx._2))
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
    visitAndBuildViews(scatter.f.body, outputAccessInf)
  }

  private def getFullType(outputType: Type, outputAccessInf: List[(ArithExpr, ArithExpr)]): Type = {
    outputAccessInf.foldLeft(outputType)((t, len) => ArrayType(t, len._1))
  }

}

object ViewPrinter {

  def emit(sv : View) : ArithExpr = {
    emitView(sv, new Stack(), new Stack())
  }

  private def emitView(sv : View,
                       arrayAccessStack : Stack[(ArithExpr, ArithExpr)], // id, dimension size
                       tupleAccessStack : Stack[Int]) : ArithExpr = {
    sv match {
      case mem : ViewMem =>
        assert(tupleAccessStack.isEmpty)
        arrayAccessStack.map(x => (x._1*x._2).asInstanceOf[ArithExpr]).foldLeft(Cst(0).asInstanceOf[ArithExpr])((x, y) => (x+y).asInstanceOf[ArithExpr])

      case access : ViewAccess =>
        val length: ArithExpr = getLengthForArrayAccess(sv.t, tupleAccessStack)
        val newAAS = arrayAccessStack.push((access.i, length))
        emitView(access.iv,newAAS, tupleAccessStack)

      case map : ViewMap =>
        val idx = arrayAccessStack.top._1
        val newAAS = arrayAccessStack.pop
        val newV = map.iv.replaced(map.itVar, idx)
        emitView(newV,newAAS,tupleAccessStack)

      case split : ViewSplit =>
        val (chunkId,stack1) = arrayAccessStack.pop2
        val (chunkElemId,stack2) = stack1.pop2
        val newIdx = chunkId._1*split.n+chunkElemId._1
        val newAAS = stack2.push((newIdx, chunkElemId._2))
        emitView(split.iv, newAAS,tupleAccessStack)

      case join : ViewJoin =>
        val (idx,stack) = arrayAccessStack.pop2
        val chunkSize: ArithExpr = join.n
        val chunkId = idx._1 div chunkSize
        val chunkElemId = idx._1 % chunkSize
        val newAS = stack.push((chunkElemId, Type.getLengths(sv.t.asInstanceOf[ArrayType].elemT).reduce(_*_))).
          push((chunkId, Type.getLengths(join.t.asInstanceOf[ArrayType].elemT).reduce(_*_)*join.n))
        emitView(join.iv,newAS,tupleAccessStack)

      case gather : ViewGather =>
        val (idx,stack) = arrayAccessStack.pop2
        val newIdx = gather.f(idx._1)
        val newAS = stack.push((newIdx, idx._2))
        emitView(gather.iv,newAS,tupleAccessStack)

      case filter : ViewFilter =>
        val ((idx, len), stack) = arrayAccessStack.pop2

        val newIdx = emit(filter.ids.access(idx))
        val indirection = new AccessVar(View.getInputAccess(filter.ids).name, newIdx)

        emitView(filter.iv, stack.push((indirection, len)), tupleAccessStack)

      case component : ViewTupleComponent =>
        val newTAS = tupleAccessStack.push(component.i)
        emitView(component.iv,arrayAccessStack,newTAS)

      case zip : ViewZip =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        emitView(zip.ivs(i),arrayAccessStack,newTAS)

      case tuple : ViewTuple =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        emitView(tuple.ivs(i),arrayAccessStack,newTAS)

      case asVector: ViewAsVector =>
        val top = arrayAccessStack.top
        val newAAS = arrayAccessStack.pop.push((top._1 * asVector.n, top._2)).map(x => (x._1, x._2 / asVector.n))
        emitView(asVector.iv, newAAS, tupleAccessStack)

      case asScalar: ViewAsScalar =>
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


