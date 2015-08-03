package opencl.ir

import apart.arithmetic.{Cst, ArithExprFunction, Var, ArithExpr}
import ir.UserFunDef._
import ir._

import language.implicitConversions

object CompositePatterns {

  def Tile(size: ArithExpr): CompFunDef = Tile(size, size)

  def Tile(x: ArithExpr, y: ArithExpr) =
    Map(Map(Transpose()) o Split(y) o Transpose()) o Split(x)

  def Untile() = Join() o Map(Map(Join()) o TransposeW())

  def ReorderStride(s: ArithExpr) = Gather(IndexFunction.reorderStride(s))
 }

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

// Map over a matrix - more abstract, to please the typechecker

case class MapMatrix(dim: Int, f: Lambda1) extends GenerableMap(f) {
  override def apply(args: Expr*) : MapCall = {
    assert(args.length == 1)
    new MapCall("MapMatrix", Var("wg_id"), this, args(0))
  }

  override def $(that: Expr) : MapCall = {
    apply(that)
  }
}

object MapMatrix {
  def apply(f: Lambda1) = new MapMatrix(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapMatrix(dim, f)
}

// Reductions

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
  def apply(f: Lambda2, init: Expr): Lambda1 = fun((x) => ReduceSeq(f)(init, x))
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

case class toLocal(f: Lambda1) extends Pattern(Array[Param](Param(UndefType))) with FPattern with isGenerable

case class toPrivate(f: Lambda1) extends Pattern(Array[Param](Param(UndefType))) with FPattern with isGenerable

case class Barrier() extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  var valid = true
}

case class TransposeW() extends Pattern(Array[Param](Param(UndefType))) with isGenerable

case class Transpose() extends Pattern(Array[Param](Param(UndefType))) with isGenerable

case class Pad(offset: Int, boundary: (ArithExpr, ArithExpr) => ArithExpr)
  extends Pattern(Array[Param](Param(UndefType))) with isGenerable

object Pad {
  type BoundaryFct = (ArithExpr, ArithExpr) => ArithExpr

  object Boundary {
    val Wrap: BoundaryFct = (idx: ArithExpr, len: ArithExpr) => (idx % len + len) % len

    val Clamp: BoundaryFct = (idx: ArithExpr, len: ArithExpr) => ArithExpr.Math.Clamp(idx, 0, len-1)

    val Mirror: BoundaryFct = (idx: ArithExpr, len: ArithExpr) => {
      val id = ((idx lt 0) ?? (-1-idx) !! idx) % (2*len)
      (id ge len) ?? (len+len-id-1) !! id
    }

    val Bounce: BoundaryFct = (idx: ArithExpr, len: ArithExpr) => {
      throw new NotImplementedError("Not Implemented")
    }
  }
}

object Pad2D {
  def apply(offset: Int, boundary: (ArithExpr, ArithExpr) => ArithExpr): CompFunDef = {
    Transpose() o Pad(offset, boundary) o Transpose() o Pad(offset, boundary)
  }
}

case class Group(relIndices: Array[Int]) extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  Group.cnt += 1
  val id = Group.cnt
}

object Group {
  var cnt: Int = -1
}

object Group2D {
  /** Symmetrical grouping */
  def apply(neighbors: Array[Int]): CompFunDef = {
    Map(
      Map(
        Transpose()
      ) o Group(neighbors) o Transpose()
    ) o Group(neighbors)
  }

  /** Asymmetrical grouping */
  def apply(relColumns: Array[Int],
            relRows: Array[Int]): CompFunDef = {
    Map(
      Map(
        Transpose()
      ) o Group(relColumns) o Transpose()
    ) o Group(relRows)
  }
}

/**
 * Create a stencil from an array of offsets and a boundary condition. This effectively create a Pad and Group
 * to compensate for out-of-bound elements
 */
object Stencil {
  def apply(neighbors: Array[Int], boundary: (ArithExpr, ArithExpr) => ArithExpr): CompFunDef = {
    Group(neighbors.map(_+neighbors.map(Math.abs).max)) o Pad(neighbors.map(Math.abs).max, boundary)
  }
}

object Stencil2D {
  def apply(neighbors: Array[Int], boundary: (ArithExpr, ArithExpr) => ArithExpr): CompFunDef = {
    Group2D(neighbors.map(_+neighbors.map(Math.abs).max)) o Pad2D(neighbors.map(Math.abs).max, boundary)
  }
}

class GroupCall(val group: Group, val outerAe: ArithExpr, val innerAe: ArithExpr) extends ArithExprFunction(s"groupComp${group.id}") {
  "groupComp(" + outerAe + ", " + innerAe + ")"
  //"groupComp" + group.id + "(" + outerAe + ", " + innerAe + ")"
}

/*
// Group that returns a constant
case class GroupConstant(relIndices: Array[Int], constant: ArithExpr) extends Pattern(Array[Param](Param(UndefType))) with isGenerable {
  Group.cnt += 1
  val id = Group.cnt
}
object jGroupConstant {
  def create(relIndices: Array[Int], constant: ArithExpr) = GroupConstant(relIndices, constant)
}
*/

class IndexFunction(val f: (ArithExpr, Type) => ArithExpr)

object IndexFunction {
  implicit def apply(f: (ArithExpr, Type) => ArithExpr): IndexFunction = new IndexFunction(f)

  // predefined reorder functions ...
  val transposeFunction = (outerSize: ArithExpr, innerSize: ArithExpr) => (i: ArithExpr, t: Type) => {
    val col = (i % innerSize) * outerSize
    val row = i / innerSize

    row + col
  }

  val transpose = (i: ArithExpr, t: Type) => {
    val outerType = t match { case at: ArrayType => at; case _ => ??? }
    val innerType = outerType.elemT match { case at: ArrayType => at; case _ => ??? }

    transposeFunction(outerType.len, innerType.len)(i, t)
  }

  val reverse = (i: ArithExpr, t: Type) => {
    val n = Type.getLength(t)

    n - 1 - i
  }

  val reorderStride = (s:ArithExpr) => (i: ArithExpr, t:Type) => {
    val n = Type.getLength(t) /^ s
    (i / n) + s * (i % n)
  }
}

case class Gather(idx: IndexFunction) extends Pattern(Array[Param](Param(UndefType))) with isGenerable

case class Scatter(idx: IndexFunction) extends Pattern(Array[Param](Param(UndefType))) with isGenerable

case class Head() extends Pattern(Array[Param](Param(UndefType))) with isGenerable

case class Tail() extends Pattern(Array[Param](Param(UndefType))) with isGenerable


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
