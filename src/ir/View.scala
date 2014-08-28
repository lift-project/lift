package ir

import opencl.ir.OpenCLMemory


abstract class View(val parent : Access) {

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
  def apply(t: Type, a: Access) : View = {
    t match {
      case at:ArrayType => new ArrayView(at.elemT, a)
      case st:ScalarType => new ScalarView(a)
      case tt:TupleType => new TupleView(tt.elemsT,a)
    }
  }

}

class ArrayView(val elemT: Type, override val parent : Access) extends View(parent) {

  def get(idx : ArithExpr) : View = {
    val aa = new ArrayAccess(this, idx)
    View(elemT, aa)
  }

  /*def reorder(f : (ArithExpr) => ArithExpr) = {
    val ra = new ReorderAccess(this, f)
    View(elemT, ra)
  }*/

}

class TupleView(val elemsT : Seq[Type], override val parent : Access) extends View(parent) {

  def get(i : Cst) : View = {
    val ta = new TupleAccess(this, i.c)
    View(elemsT(i.c), ta)
  }

}


class ScalarView(override val parent : Access) extends View(parent) {}





sealed abstract class Access

class InputAccess(val name : String) extends Access
class ArrayAccess(val av: ArrayView, val idx : ArithExpr) extends Access
class ArrayCreation(val v: View, val len: ArithExpr, val itVar: Var) extends Access
//class ReorderAccess(val av: ArrayView, val f : (ArithExpr) => ArithExpr) extends Access
class TupleAccess(val tv: TupleView, val i : Int) extends Access
class TupleCreation(val views: Seq[View]) extends Access




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


      /*case aa : ArrayAccess =>

        val idx = aa.av.parent match {
          case ac : ArrayCreation =>
            println("### replacing "+ac.itVar+" with "+aa.idx)
            val newV = ac.v.replaced(ac.itVar, aa.idx)
            emitView(newV,arrayAccessStack,tupleAccessStack)
          case ia : InputAccess =>
            emitView(aa.av,arrayAccessStack,tupleAccessStack)
            print("["+aa.idx+"]")
        }*/

      case aa : ArrayAccess =>
        val newAAS = arrayAccessStack :+ aa.idx
        emitView(aa.av,newAAS, tupleAccessStack)

      case ac : ArrayCreation =>
        val idx = arrayAccessStack.last
        val newAAS = arrayAccessStack.pop
        val newV = ac.v.replaced(ac.itVar, idx)
        emitView(newV,newAAS,tupleAccessStack)

      case ta : TupleAccess =>
        val newTAS = tupleAccessStack :+ ta.i
        emitView(ta.tv,arrayAccessStack,newTAS)

      case tc : TupleCreation =>
        val i = tupleAccessStack.last
        val newTAS = tupleAccessStack.pop
        emitView(tc.views(i),arrayAccessStack,newTAS)

        /*print("{")
        tc.views.foreach(v=>{
          emitView(v,arrayAccessStack,tupleAccessStack)
          print(",")
        })
        print("}")*/

      /*case ac : ArrayCreation =>
        emitView(ac.v)*/


     }
  }

}


object Test extends App {

  val int = ScalarType("int", 4)

  val a = new ScalarView(new InputAccess("a"))
  val B = new ArrayView(int, new InputAccess("B"))

  // map(b => zip(a,b)) o B
  val var_i = new Var("i",RangeUnkown)
  val b = B.get(var_i).asInstanceOf[ScalarView] // TODO B.get should return the appropirate type (use generics)
  val zip_ab = new TupleView(List(int, int),new TupleCreation(List(a, b)))
  val map_zip_ab = new ArrayView(TupleType(int, int),new ArrayCreation(zip_ab,Cst(10),var_i))

  // map (f) o ...
  val var_j = new Var("j",RangeUnkown)
  val mapf = map_zip_ab.get(var_j).asInstanceOf[TupleView]

  val mapf0 = mapf.get(0).asInstanceOf[ScalarView]
  val mapf1 = mapf.get(1).asInstanceOf[ScalarView]


  ViewPrinter.emit(a); println()
  //ViewPrinter.emit(B); println()
  ViewPrinter.emit(b); println()
  //ViewPrinter.emit(zip_ab); println()
  //ViewPrinter.emit(map_zip_ab); println()
  ViewPrinter.emit(mapf0); println()
  ViewPrinter.emit(mapf1); println()

  println(b)
}