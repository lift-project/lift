package ir

import opencl.ir._
import org.junit.Test

sealed abstract class Operation

object NoOperation extends Operation

class InputAccess(val name: String) extends Operation

class ArrayCreation(val v: View, val len: ArithExpr, val itVar: Var) extends Operation
class ArrayAccess(var av: ArrayView, val idx: ArithExpr) extends Operation
class ArrayReorder(val av: ArrayView, val f: (ArithExpr) => ArithExpr) extends Operation
class ArraySplit(val av: ArrayView, val chunkSize: ArithExpr) extends Operation
class ArrayJoin(val av: ArrayView, val chunkSize: ArithExpr) extends Operation
class ArrayZip(val tv: TupleView) extends Operation

class TupleCreation(val views: Seq[View]) extends Operation
class TupleAccess(val tv: TupleView, val i: Int) extends Operation


abstract class View(val operation: Operation) {
  def replaced(oldExpr: ArithExpr, newExpr: ArithExpr): View = {
    val subst = new scala.collection.mutable.HashMap[ArithExpr,ArithExpr]()
    subst.put(oldExpr, newExpr)

    val newOperation = this.operation match {
      case ac: ArrayCreation => new ArrayCreation(ac.v.replaced(oldExpr,newExpr), ArithExpr.substitute(ac.len, subst.toMap), ArithExpr.substitute(ac.itVar, subst.toMap).asInstanceOf[Var])
      case aa: ArrayAccess => new ArrayAccess(aa.av.replaced(oldExpr,newExpr).asInstanceOf[ArrayView], ArithExpr.substitute(aa.idx, subst.toMap))
      case az: ArrayZip => new ArrayZip(az.tv.replaced(oldExpr,newExpr).asInstanceOf[TupleView])
      case as: ArraySplit => new ArraySplit(as.av.replaced(oldExpr,newExpr).asInstanceOf[ArrayView], ArithExpr.substitute(as.chunkSize, subst.toMap))

      case tc: TupleCreation => new TupleCreation(tc.views.map(_.replaced(oldExpr,newExpr)))
      case ta: TupleAccess => new TupleAccess(ta.tv.replaced(oldExpr,newExpr).asInstanceOf[TupleView],ta.i)

      case  _ => this.operation
    }

    this match {
      case av: ArrayView => new ArrayView(av.elemT, newOperation)
      case sv: PrimitiveView => new PrimitiveView(newOperation)
      case tv: TupleView => new TupleView(tv.tupleType, newOperation)
    }
  }
}

object NoView extends View(NoOperation)

class ArrayView(val elemT: Type, override val operation: Operation) extends View(operation) {

  def access(idx: ArithExpr): View = {
    val aa = new ArrayAccess(this, idx)
    View(elemT, aa)
  }

  def split(chunkSize : ArithExpr): ArrayView = {
    val as = new ArraySplit(this, chunkSize)
    new ArrayView(new ArrayType(elemT, chunkSize), as)
  }

  def join(chunkSize: ArithExpr): ArrayView = {
    val aj = new ArrayJoin(this, chunkSize)
    new ArrayView(elemT.asInstanceOf[ArrayType].elemT, aj)
  }

  def reorder(f: (ArithExpr) => ArithExpr): ArrayView = {
    val ar = new ArrayReorder(this, f)
    new ArrayView(elemT, ar)
  }


  override def toString = {
    elemT.toString + " " + operation.toString
  }

}

class TupleView(val tupleType: TupleType, override val operation: Operation) extends View(operation) {

  def access(i: Int) : View = {
    val ta = new TupleAccess(this, i)
    val elemT = tupleType.elemsT(i)
    View(elemT, ta)
  }

}

object TupleView {
  def apply(t: Type, op: Operation): TupleView = {
    assert(t.isInstanceOf[TupleType])
    new TupleView(t.asInstanceOf[TupleType], op)
  }
}

class PrimitiveView(override val operation: Operation) extends View(operation)



object View {
  // create new view based on the given type
  def apply(t: Type, op: Operation): View = {
    t match {
      case at: ArrayType => new ArrayView(at.elemT, op)
      case st: ScalarType => new PrimitiveView(op)
      case tt: TupleType => new TupleView(tt, op)
    }
  }

  def createView(expr: Expr, f: (Type) => View = (t) => View(t, new InputAccess(""))): View = {
    val result = expr match {
      case pr: ParamReference => getViewAtIndex(pr.p.view, pr.i)
      case p: Param => p.view //View(expr.t, new InputAccess())
      case call: FunCall => createViewFunCall(call, f)
    }
    expr.view = result
    result
  }

  private def getViewAtIndex(v: View, index: Int): View = {
    v match {
      case tv: TupleView =>
        assert(index < tv.tupleType.elemsT.length)
        tv.access(index)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def createViewFunCall(call: FunCall, f: (Type) => View): View = {
    val argView = getViewFromArgs(call)

    call match {
      case call: MapCall => createViewMap(call, argView, f)
      case call: ReduceCall => createViewReduce(call, argView, f)
      case call: FunCall =>
        call.f match {
          case l: Lambda => createViewLambda(l, call, argView, f)
          case cf: CompFunDef => createViewCompFunDef(cf, argView, f)
          case z: Zip => createViewZip(z, call, argView)
          case Split(n) => createViewSplit(n, argView)
          case _: Join =>
            val chunkSize = call.argsType match {
              case ArrayType(ArrayType(_, n), _) => n
              case _ => throw new IllegalArgumentException("PANIC, expected 2D array, found " + call.argsType)
            }
            createViewJoin(chunkSize, argView)
          case uf: UserFunDef => createViewUserFunDef(uf, argView, f)
          case _: ReorderStride => createViewReorderStride(call, argView)
          case g: Gather => createViewGather(g, call, argView, f)
          case s: Scatter => createViewScatter(s, call, argView, f)
          case tL: toLocal =>
            tL.f.params(0).view = argView
            createView(tL.f.body, f)
          case tG: toGlobal =>
            tG.f.params(0).view = argView
            createView(tG.f.body, f)
          case i: Iterate =>
            i.f.params(0).view = argView
            createView(i.f.body, f)
          /*case uz: Unzip =>
          case SplitDim2(n) =>
          case j: JoinDim2 =>
          case _: asScalar =>
          case asVector(n) =>

          case _: Transpose =>
          case _: Swap =>
          */
          case _ => argView
        }
    }
  }

  private def getViewFromArgs(call: FunCall): View = {
    if (call.args.isEmpty) {
      NoView
    } else if (call.args.length == 1) {
      createView(call.args(0))
    } else {
      TupleView(call.argsType, new TupleCreation(call.args.map((expr: Expr) => createView(expr))))
    }
  }

  private def createViewMap(call: MapCall, argView: View, f: (Type) => View): View = {
    argView match {
      case av: ArrayView =>
        call.f.f.params(0).view = av.access(call.loopVar)

        val newF: (Type) => View = (t) => f(ArrayType(t, Type.getLength(call.t))).asInstanceOf[ArrayView].access(call.loopVar)

        val innerView = createView(call.f.f.body, newF)
        new ArrayView(Type.getElemT(call.t), new ArrayCreation(innerView, Type.getLength(call.t), call.loopVar))
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def createViewReduce(call: ReduceCall, argView: View, f: (Type) => View): View = {
    argView match {
      case tv: TupleView =>
        call.f.f.params(0).view = tv.access(0)
        call.f.f.params(1).view = tv.access(1).asInstanceOf[ArrayView].access(call.loopVar)

        val newF: (Type) => View = (t) => f(ArrayType(t, Type.getLength(call.t))).asInstanceOf[ArrayView].access(Cst(0))

        val innerView = createView(call.f.f.body, newF)
        new ArrayView(Type.getElemT(call.t), new ArrayCreation(innerView, Type.getLength(call.t), call.loopVar))
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def createViewLambda(l: Lambda, call: FunCall, argView: View, f: (Type) => View): View = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).view = argView
    } else {
      val tv = argView match { case tv: TupleView => tv }

      l.params.zipWithIndex.map({ case (p, i) => p.view = tv.access(i) })
    }
    createView(l.body, f)
  }

  private def createViewCompFunDef(cf: CompFunDef, argView: View, f: (Type) => View): View = {

    val funs = scala.collection.mutable.Stack(f)

    cf.funs.foldRight(argView)((f, v) => {
      if (f.params.length != 1) throw new NumberOfArgumentsException
      f.params(0).view = v

      f.body match {
        case call: FunCall =>
          call.f match {
            case tL: toLocal => funs.push((t) => View(t, new InputAccess("")))
            case tG: toGlobal => if (funs.length > 1) funs.pop()
            case _ =>
          }
        case _ =>
      }

      createView(f.body, funs.top)
    })
  }

  private def createViewZip(z: Zip, call: FunCall, argView: View): View = {
    argView match {
      case tv: TupleView => new ArrayView(Type.getElemT(call.t), new ArrayZip(tv))
      // new ArrayCreation(tv, Type.getLength(call.t), ???))
      //case tv: TupleView => tv
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def createViewJoin(n: ArithExpr, argView: View): View = {
    argView match {
      case av: ArrayView => av.join(n)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def createViewSplit(n: ArithExpr, argView: View): View = {
    argView match {
      case av: ArrayView => av.split(n)
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def createViewUserFunDef(uf: UserFunDef, argView: View, f: (Type) => View): View = {
    // Use the lengths and iteration vars to mimic inputs
    f(uf.outT)
  }

  private def createViewReorderStride(call: FunCall, argView: View): View = {
    val s = Type.getLength(call.argsType)
    val n = Type.getLength(Type.getElemT(call.argsType))

    argView match {
      case av: ArrayView => av.reorder( (i:ArithExpr) => { i / n + s * ( i % n) } )
      case _ => throw new IllegalArgumentException("PANIC")
    }
  }

  private def createViewGather(gather: Gather, call: FunCall, argView: View, f: (Type) => View): View = {
    argView match {
      case av: ArrayView =>
        gather.f.params(0).view = av.reorder( (i:ArithExpr) => { gather.idx.f(i, call.t) } )
        createView(gather.f.body, f)
      case _ => throw new IllegalArgumentException("PANIC")    }
  }

  private def createViewScatter(scatter: Scatter, call: FunCall, argView: View, f: (Type) => View): View = {
    argView match {
      case av: ArrayView =>
        scatter.f.params(0).view = av
        createView(scatter.f.body, f)

        // Find the matching ArrayAccess to the first ArrayCreation,
        // and reorder the ArrayView in the access
        scatterReorder(scatter.f.body.view, scatter.idx, call.t, 0)

        scatter.f.body.view
      case _ => throw new IllegalArgumentException("PANIC")    }
  }

  private def scatterReorder(view: View, idx: IndexFunction, t:Type, count: scala.Int): Unit = {
    view match {
      case av: ArrayView =>
        scatterReorder(av.operation.asInstanceOf[ArrayCreation].v, idx, t, count+1)
      case pv: PrimitiveView =>
        findAccessAndReorder(pv, idx, t, count)
    }
  }

  private def findAccessAndReorder(view: View, idx: IndexFunction, t:Type, count: scala.Int): Unit = {

    view.operation match {
      case access: ArrayAccess =>
        if (count == 1) {
          access.av = access.av.reorder( (i:ArithExpr) => { idx.f(i, t) } )
        } else {
          findAccessAndReorder(access.av, idx, t, count-1)
        }
      case ar: ArrayReorder => findAccessAndReorder(ar.av, idx, t, count)
      case as: ArraySplit => findAccessAndReorder(as.av, idx, t, count)
      case aj: ArrayJoin => findAccessAndReorder(aj.av, idx, t, count)
    }
  }



}





object ViewPrinter {

  def emit(sv : View) : ArithExpr = {
    sv match {
      case _: PrimitiveView => emitView(sv, new scala.collection.immutable.Stack(), new scala.collection.immutable.Stack())
      case _: TupleView => emitView(sv, new scala.collection.immutable.Stack(), new scala.collection.immutable.Stack())
      case t => throw new IllegalArgumentException(t + " found, TupleView/PrimitiveView expected")
    }
  }


  private def emitView(sv : View,
                       arrayAccessStack : scala.collection.immutable.Stack[(ArithExpr, ArithExpr)], // id, dimension size
                       tupleAccessStack : scala.collection.immutable.Stack[Int]) : ArithExpr = {
    sv.operation match {
      case ia : InputAccess =>
        print(ia.name)
        assert(tupleAccessStack.isEmpty)
        val res = arrayAccessStack.map(x => (x._1*x._2).asInstanceOf[ArithExpr]).foldLeft(Cst(0).asInstanceOf[ArithExpr])((x, y) => (x+y).asInstanceOf[ArithExpr])
        println("["+res+"]")
        res

      case aa : ArrayAccess =>
        val newAAS = arrayAccessStack.push((aa.idx, Type.getLengths(aa.av.elemT).reduce(_*_)))
        emitView(aa.av,newAAS, tupleAccessStack)

      case ac : ArrayCreation =>
        val idx = arrayAccessStack.top._1
        val newAAS = arrayAccessStack.pop
        val newV = ac.v.replaced(ac.itVar, idx)
        emitView(newV,newAAS,tupleAccessStack)

      case as : ArraySplit =>
        val (chunkId,stack1) = arrayAccessStack.pop2
        val (chunkElemId,stack2) = stack1.pop2
        val newIdx = chunkId._1*as.chunkSize+chunkElemId._1
        val newAAS = stack2.push((newIdx, Type.getLength(as.av.elemT)))
        emitView(as.av,newAAS,tupleAccessStack)

      case aj : ArrayJoin =>
        val (idx,stack) = arrayAccessStack.pop2
        val chunkSize: ArithExpr = aj.chunkSize
        val chunkId = idx._1/chunkSize
        val chunkElemId = idx._1 - (chunkId *  chunkSize)//idx % aj.chunkSize
        val newAS = stack.push((chunkElemId, Type.getLengths(sv.asInstanceOf[ArrayView].elemT).reduce(_*_))).push((chunkId, Type.getLengths(aj.av.elemT).reduce(_*_)))
        emitView(aj.av,newAS,tupleAccessStack)

      case ar : ArrayReorder =>
        val (idx,stack) = arrayAccessStack.pop2
        val newIdx = ar.f(idx._1)
        val newAS = stack.push((newIdx, idx._2))
        emitView(ar.av,newAS,tupleAccessStack)

      case ta : TupleAccess =>
        val newTAS = tupleAccessStack.push(ta.i)
        emitView(ta.tv,arrayAccessStack,newTAS)

      case tc : TupleCreation =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        emitView(tc.views(i),arrayAccessStack,newTAS)

      case az : ArrayZip =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        emitView(az.tv.access(i) ,arrayAccessStack,newTAS)


     }
  }
}


class ViewTest {

  val int = ScalarType("int", 4)

  @Test
  def testUserFun(): Unit = {

    val id = UserFunDef("id", "x", "{ return x; }", Float, Float)

    val f = fun(
        ArrayType(Float, Var("N")),
        x => Map(id) $ x
    )

    Type.check(f.body)

    f.params.map(p => p.view = View(p.t, new InputAccess("A")))

    View.createView(f.body)

    val mapCall: MapCall = f.body.asInstanceOf[MapCall]
    val funCall = mapCall.f.f.body.asInstanceOf[FunCall]

    println("Uf read accesss: ")
    funCall.args.map(a => {
      ViewPrinter.emit(a.view.asInstanceOf[PrimitiveView])
      println()
    })

    println("Uf write access: ")
    ViewPrinter.emit(funCall.view.asInstanceOf[PrimitiveView])
    println()

    println("Next read: ")
    ViewPrinter.emit(mapCall.view.asInstanceOf[ArrayView].access(Var("j")).asInstanceOf[PrimitiveView])
    println()
  }

  def test1() {

    val a = new PrimitiveView(new InputAccess("a"))
    val B = new ArrayView(int, new InputAccess("B"))

    // The map below is not valid, zip on primitives would fail during type-checking
    // map(b => zip(a,b)) o B
    val var_i = new Var("i", RangeUnkown)
    val b = B.access(var_i).asInstanceOf[PrimitiveView] // TODO B.get should return the appropriate type (use generics)
    val zip_ab = new TupleView(TupleType(int, int), new TupleCreation(List(a, b)))
    val map_zip_ab = new ArrayView(TupleType(int, int), new ArrayCreation(zip_ab, Cst(10), var_i))

    // map (f) o ...
    val var_j = new Var("j", RangeUnkown)
    val mapf = map_zip_ab.access(var_j).asInstanceOf[TupleView]

    val mapf0 = mapf.access(0).asInstanceOf[PrimitiveView]
    val mapf1 = mapf.access(1).asInstanceOf[PrimitiveView]


    ViewPrinter.emit(a)
    println()
    //ViewPrinter.emit(B); println()
    ViewPrinter.emit(b)
    println()
    //ViewPrinter.emit(zip_ab); println()
    //ViewPrinter.emit(map_zip_ab); println()
    ViewPrinter.emit(mapf0)
    println()
    ViewPrinter.emit(mapf1)
    println()
  }

  @Test
  def test2() {

    val A = new ArrayView(new ArrayType(new ArrayType(int, 8), 4), new InputAccess("A"))
    val B = new ArrayView(new ArrayType(new ArrayType(int, 8), 4), new InputAccess("B"))

    //  map(map(map(f))) o map(a => map(b => map(zip(a,b)) o B) o A equivalent to
    // map(a => map(b => map(f) $ zip(a,b)) o B) o A

    // map(a => ... ) $ A
    val var_i = new Var("i", RangeUnkown)
    val a = A.access(var_i).asInstanceOf[ArrayView]
    // ... map(b => ...) $ B ...
    val var_j = new Var("j", RangeUnkown)
    val b = B.access(var_j).asInstanceOf[ArrayView]
    // ... $ zip(a, b) ...
    val zip_ab = new TupleView(TupleType(new ArrayType(int, 8), new ArrayType(int, 8)), new TupleCreation(List(a, b)))
    val map_zip_ab = new ArrayView(TupleType(new ArrayType(int, 8), new ArrayType(int, 8)), new ArrayCreation(zip_ab, Cst(4), var_j))
    val map_map_zip_ab = new ArrayView(new ArrayType(TupleType(new ArrayType(int, 8), new ArrayType(int, 8)),4), new ArrayCreation(map_zip_ab, Cst(4), var_i))

    // ... map(f) $ ...
    var var_x = new Var("x", RangeUnkown)


    // map(map (f)) o ...
    val var_k = new Var("k", RangeUnkown)
    val var_l = new Var("l", RangeUnkown)
    val map_f = map_map_zip_ab.access(var_k).asInstanceOf[ArrayView]
    val map_map_f = map_f.access(var_l).asInstanceOf[TupleView]

    val map_map_f0 = map_map_f.access(0).asInstanceOf[ArrayView]
    val map_map_f1 = map_map_f.access(1).asInstanceOf[ArrayView]

    val map_map_f0_9 = map_map_f0.access(9).asInstanceOf[PrimitiveView]
    val map_map_f1_7 = map_map_f1.access(7).asInstanceOf[PrimitiveView]

    print("gold = A[k][9], emitted = ")
    ViewPrinter.emit(map_map_f0_9)
    println()
    print("gold = B[l][7], emitted = ")
    ViewPrinter.emit(map_map_f1_7)
    println()

    val n = Var("N")
    val m = Var("M")

    val add = UserFunDef("add", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float)

    val f = fun(
    ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      (X, Y) => MapWrg(fun(x => MapLcl(fun(y => MapSeq(add) $ Zip(x, y))) $ Y )) $ X
    )

  }

  @Test
  def test3() {

    val A = new ArrayView(new ArrayType(new ArrayType(int, 8), 4), new InputAccess("A"))
    val B = new ArrayView(new ArrayType(new ArrayType(int, 8), 4), new InputAccess("B"))

    // map(a => map(b => map(fun(t => Get(t, 0) * Get(t, 1))) o zip(a,b)) o B) o A
    val var_i = new Var("i", RangeUnkown)
    val var_j = new Var("j", RangeUnkown)
    val a = A.access(var_i).asInstanceOf[ArrayView]
    val b = B.access(var_j).asInstanceOf[ArrayView]
    val zip_ab = new TupleView(TupleType(new ArrayType(int, 8), new ArrayType(int, 8)), new TupleCreation(List(a, b)))
    val zip_ab0 = zip_ab.access(0).asInstanceOf[ArrayView]
    val zip_ab1 = zip_ab.access(1).asInstanceOf[ArrayView]

    val zip_ab0_3 = zip_ab0.access(3).asInstanceOf[PrimitiveView]
    val zip_ab1_7 = zip_ab1.access(7).asInstanceOf[PrimitiveView]


    print("gold = A[i][3], emitted = ")
    ViewPrinter.emit(zip_ab0_3)
    println()
    print("gold = B[j][7], emitted = ")
    ViewPrinter.emit(zip_ab1_7)
    println()

  }

  @Test
  def testSplit() {

    val A = new ArrayView(new ArrayType(int, 8), new InputAccess("A"))


    // split-2 o A
    val split2A = A.split(2)
    val var_i = new Var("i", RangeUnkown)
    val var_j = new Var("j", RangeUnkown)

    val split2A_i = split2A.access(var_i).asInstanceOf[ArrayView]
    val split2A_i_j = split2A_i.access(var_j).asInstanceOf[ArrayView]

    val split2A_i_j_7 = split2A_i_j.access(7).asInstanceOf[PrimitiveView]

    print("gold = A[i*2+j][7], emitted = ")
    ViewPrinter.emit(split2A_i_j_7)
    println()
  }

  @Test
  def testSplitJoin() {

    val A = new ArrayView(int, new InputAccess("A"))


    // split-2 o A
    val split2A = A.split(2)

    // join-2 o split-2 o A
    val join_split_2A = split2A.join(2)

    val var_i = new Var("i", RangeUnkown)

    val join_split_2A_i = join_split_2A.access(var_i).asInstanceOf[PrimitiveView]

    print("gold = A[i], emitted = ")
    ViewPrinter.emit(join_split_2A_i)
    println()
  }

  @Test
  def testReorder() {

    val A = new ArrayView(int, new InputAccess("A"))


    // reorder o A
    val reorder_A = A.reorder((idx) => 40-idx)

    // split o reorder o A
    val split_reorder_A = reorder_A.split(4)

    val reorder_split_reorder_A_1 = split_reorder_A.access(1).asInstanceOf[ArrayView]
    val reorder_split_reorder_A_1_3 = reorder_split_reorder_A_1.access(3).asInstanceOf[PrimitiveView]


    print("gold = A[33], emitted = ")
    ViewPrinter.emit(reorder_split_reorder_A_1_3)
    println()
  }

}

