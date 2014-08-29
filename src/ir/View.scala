package ir

import opencl.ir.OpenCLMemory


abstract class View(val parent : Operation) {

  def replaced(old: ArithExpr, rep: ArithExpr): View = {

    val subst = new scala.collection.mutable.HashMap[ArithExpr,ArithExpr]()
    subst.put(old, rep)

    val newAccess = this.parent match {
      case aa : ArrayAccess => new ArrayAccess(aa.av.replaced(old,rep).asInstanceOf[ArrayView], ArithExpr.substitute(aa.idx, subst.toMap))
      case ac : ArrayCreation => new ArrayCreation(ac.v.replaced(old,rep), ArithExpr.substitute(ac.len, subst.toMap), ArithExpr.substitute(ac.itVar, subst.toMap).asInstanceOf[Var])
      case ta : TupleAccess => new TupleAccess(ta.tv.replaced(old,rep).asInstanceOf[TupleView],ta.i)
      case tc : TupleCreation => new TupleCreation(tc.views.map(_.replaced(old,rep)))
      case  _ => this.parent
    }

    this match {
      case av : ArrayView => new ArrayView(av.elemT, newAccess)
      case sv : ScalarView => new ScalarView(newAccess)
      case tv : TupleView => new TupleView(tv.elemsT, newAccess)
    }
  }
}

object View {
  def apply(t: Type, a: Operation) : View = {
    t match {
      case at:ArrayType => new ArrayView(at.elemT, a)
      case st:ScalarType => new ScalarView(a)
      case tt:TupleType => new TupleView(tt.elemsT,a)
    }
  }

}

class ArrayView(val elemT: Type, override val parent : Operation) extends View(parent) {

  def get(idx : ArithExpr) : View = {
    val aa = new ArrayAccess(this, idx)
    View(elemT, aa)
  }

  def split(chunkSize : ArithExpr) : ArrayView = {
    val as = new ArraySplit(this, chunkSize)
    new ArrayView(new ArrayType(elemT, chunkSize), as)
  }

  /*def reorder(f : (ArithExpr) => ArithExpr) = {
    val ra = new ReorderAccess(this, f)
    View(elemT, ra)
  }*/

}

class TupleView(val elemsT : Seq[Type], override val parent : Operation) extends View(parent) {

  def get(i : Cst) : View = {
    val ta = new TupleAccess(this, i.c)
    View(elemsT(i.c), ta)
  }

}


class ScalarView(override val parent : Operation) extends View(parent) {}





sealed abstract class Operation

class InputAccess(val name : String) extends Operation
class ArraySplit(val av: ArrayView, val chunkSize : ArithExpr) extends Operation
class ArrayAccess(val av: ArrayView, val idx : ArithExpr) extends Operation
class ArrayCreation(val v: View, val len: ArithExpr, val itVar: Var) extends Operation
//class ReorderAccess(val av: ArrayView, val f : (ArithExpr) => ArithExpr) extends Access
class TupleAccess(val tv: TupleView, val i : Int) extends Operation
class TupleCreation(val views: Seq[View]) extends Operation




object ViewPrinter {

  def emit(sv : ScalarView) : Unit = {
    emitView(sv, new scala.collection.immutable.Stack(), new scala.collection.immutable.Stack())
  }


  private def emitView(sv : View,
                       arrayAccessStack : scala.collection.immutable.Stack[ArithExpr],
                       tupleAccessStack : scala.collection.immutable.Stack[Int]) : Unit = {
    sv.parent match {
      case ia : InputAccess =>
        print(ia.name)
        assert(tupleAccessStack.isEmpty)
        arrayAccessStack.foreach(idx => print("["+idx+"]"))


      case aa : ArrayAccess =>
        val newAAS = arrayAccessStack.push(aa.idx)
        emitView(aa.av,newAAS, tupleAccessStack)

      case ac : ArrayCreation =>
        val idx = arrayAccessStack.top
        val newAAS = arrayAccessStack.pop
        val newV = ac.v.replaced(ac.itVar, idx)
        emitView(newV,newAAS,tupleAccessStack)

      case ta : TupleAccess =>
        val newTAS = tupleAccessStack.push(ta.i)
        emitView(ta.tv,arrayAccessStack,newTAS)

      case tc : TupleCreation =>
        val i = tupleAccessStack.top
        val newTAS = tupleAccessStack.pop
        emitView(tc.views(i),arrayAccessStack,newTAS)


     }
  }

}


object Test extends App {

  val int = ScalarType("int", 4)

  def test1() {

    val a = new ScalarView(new InputAccess("a"))
    val B = new ArrayView(int, new InputAccess("B"))

    // map(b => zip(a,b)) o B
    val var_i = new Var("i", RangeUnkown)
    val b = B.get(var_i).asInstanceOf[ScalarView] // TODO B.get should return the appropirate type (use generics)
    val zip_ab = new TupleView(List(int, int), new TupleCreation(List(a, b)))
    val map_zip_ab = new ArrayView(TupleType(int, int), new ArrayCreation(zip_ab, Cst(10), var_i))

    // map (f) o ...
    val var_j = new Var("j", RangeUnkown)
    val mapf = map_zip_ab.get(var_j).asInstanceOf[TupleView]

    val mapf0 = mapf.get(0).asInstanceOf[ScalarView]
    val mapf1 = mapf.get(1).asInstanceOf[ScalarView]


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

  def test2() {

    val A = new ArrayView(new ArrayType(new ArrayType(int, 8), 4), new InputAccess("A"))
    val B = new ArrayView(new ArrayType(new ArrayType(int, 8), 4), new InputAccess("B"))


    /*val A = new ArrayView(
      new ArrayType(new ArrayType(int, 8), 4),
      new ArrayCreation(new ArrayView(
        new ArrayType(int, 8),
        new ArrayCreation(
          new ScalarView(new InputAccess("A")),
          Cst(8),
          Var(RangeUnkown))),
        Cst(4),
        Var(RangeUnkown)))

    val B = new ArrayView(
      new ArrayType(new ArrayType(int, 8), 4),
      new ArrayCreation(new ArrayView(
        new ArrayType(int, 8),
        new ArrayCreation(
          new ScalarView(new InputAccess("A")),
          Cst(8),
          Var(RangeUnkown))),
        Cst(4),
        Var(RangeUnkown)))*/

    // map(a => map(b => map(fun(t => Get(t, 0) * Get(t, 1))) o zip(a,b)) o B) o A
    val var_i = new Var("i", RangeUnkown)
    val var_j = new Var("j", RangeUnkown)
    val a = A.get(var_i).asInstanceOf[ArrayView]
    val b = B.get(var_j).asInstanceOf[ArrayView]
    val zip_ab = new TupleView(List(new ArrayType(int, 8), new ArrayType(int, 8)), new TupleCreation(List(a, b)))
    val map_zip_ab = new ArrayView(TupleType(new ArrayType(int, 8), new ArrayType(int, 8)), new ArrayCreation(zip_ab, Cst(4), var_j))
    val map_map_zip_ab = new ArrayView(new ArrayType(TupleType(new ArrayType(int, 8), new ArrayType(int, 8)),4), new ArrayCreation(map_zip_ab, Cst(4), var_i))

    // map(map (f)) o ...
    val var_k = new Var("k", RangeUnkown)
    val var_l = new Var("l", RangeUnkown)
    val map_f = map_map_zip_ab.get(var_k).asInstanceOf[ArrayView]
    val map_map_f = map_f.get(var_l).asInstanceOf[TupleView]

    val map_map_f0 = map_map_f.get(0).asInstanceOf[ArrayView]
    val map_map_f1 = map_map_f.get(1).asInstanceOf[ArrayView]

    val map_map_f0_9 = map_map_f0.get(9).asInstanceOf[ScalarView]
    val map_map_f1_7 = map_map_f1.get(7).asInstanceOf[ScalarView]

    ViewPrinter.emit(map_map_f0_9)
    println()
    ViewPrinter.emit(map_map_f1_7)
    println()

  }


  test2()
}