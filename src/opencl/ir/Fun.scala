package opencl.ir

import java.util.function.BiFunction

import ir._

import language.implicitConversions

case class MapGlb(dim: Int, f: Lambda1) extends GenerableMap(f){
  override def apply(args: Expr*) : MapCall = {
    assert(args.length == 1)
    new MapCall("MapGlbl", Var("gl_id"), this, args(0))
  }

  override def $(that: Expr) : MapCall = {
    apply(that)
  }
}

object MapGlb {
  def apply(f: Lambda1) = new MapGlb(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda) => new MapGlb(dim, f)
}

case class MapWrg(dim: Int, f: Lambda1) extends GenerableMap(f) {
  override def apply(args: Expr*) : MapCall = {
    assert(args.length == 1)
    new MapCall("MapWrg", Var("wg_id"), this, args(0))
  }

  override def $(that: Expr) : MapCall = {
    apply(that)
  }
}

object MapWrg {
  def apply(f: Lambda1) = new MapWrg(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapWrg(dim, f)
}

case class MapLcl(dim: Int, f: Lambda1) extends GenerableMap(f) {
  override def apply(args: Expr*) : MapCall = {
    assert(args.length == 1)
    new MapCall("MapLcl", Var("l_id"), this, args(0))
  }

  override def $(that: Expr) : MapCall = {
    apply(that)
  }
}

object MapLcl {
  def apply(f: Lambda1) = new MapLcl(0, f) // o is default

  def apply(dim: Int) = (f: Lambda1) => new MapLcl(dim, f)
}

case class MapWarp(f: Lambda1) extends GenerableMap(f) {
  override def apply(args: Expr*) : MapCall = {
    assert(args.length == 1)
    new MapCall("MapWarp", Var("warp_id"), this, args(0))
  }

  override def $(that: Expr) : MapCall = {
    apply(that)
  }
}

case class MapLane(f: Lambda1) extends GenerableMap(f) {
  override def apply(args: Expr*) : MapCall = {
    assert(args.length == 1)
    new MapCall("MapLane", Var("lane_id"), this, args(0))
  }

  override def $(that: Expr) : MapCall = {
    apply(that)
  }
}

case class MapSeq(f: Lambda1) extends GenerableMap(f) {
  override def apply(args: Expr*) : MapCall = {
    assert(args.length == 1)
    new MapCall("MapSeq", Var("i"), this, args(0))
  }

  override def $(that: Expr) : MapCall = {
    apply(that)
  }
}

case class ReduceSeq(f: Lambda2) extends AbstractReduce(f) with isGenerable {
  override def apply(args: Expr*) : ReduceCall = {
    assert(args.length == 2)
    new ReduceCall(Var("i"), this, args(0), args(1))
  }

  override def $(that: Expr) : ReduceCall = {
    apply(that)
  }
}

object ReduceSeq {
  def apply(f: Lambda2, init: Value): Lambda1 = fun((x) => ReduceSeq(f)(init, x))
}

case class ReduceHost(f: Lambda2) extends AbstractReduce(f) with isGenerable  {
  override def apply(args: Expr*) : ReduceCall = {
    assert(args.length == 2)
    new ReduceCall(Var("i"), this, args(0), args(1))
  }

  override def $(that: Expr) : ReduceCall = {
    apply(that)
  }
}
object ReduceHost {
  def apply(f: Lambda2, init: Value): Lambda1 = fun((x) => ReduceHost(f)(init, x))
}

case class toGlobal(f: Lambda1) extends Pattern(Array[Param](Param(UndefType))) with FPattern with isGenerable
  //override def copy() = toGlobal(f)

case class toLocal(f: Lambda1) extends Pattern(Array[Param](Param(UndefType))) with FPattern with isGenerable
  //override def copy() = toLocal(f)

case class Barrier() extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  var valid = true
}

case class ReorderStride(s: ArithExpr) extends Pattern(Array[Param](Param(UndefType))) with isGenerable
  //override def copy() = ReorderStride()

case class TransposeW() extends Pattern(Array[Param](Param(UndefType))) with isGenerable

case class Transpose() extends Pattern(Array[Param](Param(UndefType))) with isGenerable

case class Swap() extends Pattern(Array[Param](Param(UndefType))) with isGenerable

class IndexFunction(val f: (ArithExpr, Type) => ArithExpr)

object IndexFunction {
  implicit def apply(f: (ArithExpr, Type) => ArithExpr): IndexFunction = new IndexFunction(f)

  // predefined reorder functions ...
  val transpose = (i: ArithExpr, t: Type) => {
    val outerType = t match { case at: ArrayType => at }
    val innerType = outerType.elemT match { case at: ArrayType => at }

    val outerSize = outerType.len
    val innerSize = innerType.len

    val col = (i % innerSize) * outerSize
    val row = i div innerSize

    // TODO: simplify this ...
    row + col
  }

  val reverse = (i: ArithExpr, t: Type) => {
    val n = Type.getLength(t)

    n - 1 - i
  }
}

case class Gather(idx: IndexFunction, f: Lambda1) extends Pattern(Array[Param](Param(UndefType))) with FPattern with isGenerable

object Gather {
  def apply(idx: IndexFunction) = (f: Lambda1) => new Gather(idx, f)
}

case class Scatter(idx: IndexFunction, f: Lambda1) extends Pattern(Array[Param](Param(UndefType))) with FPattern with isGenerable

object Scatter {
  def apply(idx: IndexFunction) = (f: Lambda1) => new Scatter(idx, f)
}

// TODO: find a way for splitting the Fun.visit() function between non-opencl and opencl part
/*
object Fun {

  def replaceRef(f: Fun, oriF: Fun, newF: Fun) : Fun = {
    visit(f,
      (inF: Fun) => if (inF.eq(oriF)) newF else inF,
      (inF: Fun) => inF)       
  }
  
  def visit[T](z:T)(f: Fun, vfn: (Fun,T) => T): T = {
    val result = vfn(f,z)
    f match {
      case FPattern(inF, _) => visit(result)(inF, vfn)
      case cf: CompFun => cf.funs.foldRight(result)((inF,x)=>visit(x)(inF, vfn))      
      case _ => result
    }
  }
  
  /*
   * Visit the expression of function f (recursively) and rebuild along the way
   */
  def visitExpr(f: Fun, exprF: (Expr) => (Expr)) : Fun = {   
    visit(f, inF => inF match {
      case Split(e) => Split(exprF(e))
      case asVector(e) => asVector(exprF(e))
      case _ => inF
    }, inF => inF)
  }  
  
  /*
   * Visit the function f recursively and rebuild along the way
   */
   def visit(f: Fun, pre: (Fun) => (Fun), post: (Fun) => (Fun)) : Fun = {
    var newF = pre(f)
    newF = newF match {
      case NullFun => f

      case cf: CompFun => CompFun(cf.funs.map(inF => visit(inF, pre, post)):_*)
      
      case Map(f)    => Map(visit(f,pre,post))
      case MapSeq(f) => MapSeq(visit(f,pre,post))
      case MapGlb(f) => MapGlb(visit(f,pre,post))
      case MapWrg(f) => MapWrg(visit(f,pre,post))
      case MapLcl(f) => MapLcl(visit(f,pre,post))
      
      case Reduce(f)    => Reduce(visit(f,pre,post))
      case ReduceSeq(f) => ReduceSeq(visit(f,pre,post))

      case PartRed(f) => PartRed(visit(f,pre,post))
      
      case _ => newF
    }
    post(newF)
  }

  /*
   * Visit the function f recursively
   */
  def visit(f: Fun, pre: (Fun) => (Unit), post: (Fun) => (Unit)): Unit = {
    pre(f)
    f match {
      case FPattern(inF, _) => visit(inF, pre, post)
      case cf: CompFun => cf.funs.reverseMap(inF => visit(inF, pre, post))
      case _ =>
    }
    post(f)
  }
   
}
*/
