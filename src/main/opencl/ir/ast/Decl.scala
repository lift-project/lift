package opencl.ir.ast

import arithmetic.{ArithExpr, ArithExprFunction, Var}
import ir.ast._

import scala.language.implicitConversions

object CompositePatterns {

  def Tile(size: ArithExpr): CompFun = Tile(size, size)

  def Tile(x: ArithExpr, y: ArithExpr) =
    Map(Map(Transpose()) o Split(y) o Transpose()) o Split(x)

  def Untile() = Join() o Map(Map(Join()) o TransposeW())

  def ReorderStride(s: ArithExpr) = Gather(IndexFunction.reorderStride(s))
}

abstract class GenerableMap(f:Lambda1) extends AbstractMap(f) with isGenerable

/**
 * Apply the lambda <code>f</code> to every element of the input
 *
 * Applicable rules:
 *  - Map(f) => Join() o Map(Map(f)) o Split(I)
 *  - Map(f) o Map(g) => Map(f o g)
 *  - Map(f) => asScalar() o Map(Vectorize(k)(f)) o asVector(k) (input a multiple of k)
 *  - Map(f) => MapGlb(f)
 *  - Map(f) => MapWrg(f)
 *  - Map(f) => Barrier() o MapLcl(f)
 *  - Map(f) => MapWarp(f)
 *  - Map(f) => MapLane(f)
 *  - Map(f) => MapSeq(f)
 *
 * @param f Lambda to apply to every element of the input
 */

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

/**
 *
 * Applicable rules:
 *  - MapLcl(f) => toGlobal(MapLcl(f))
 *  - MapLcl(f) => toLocal(MapLcl(f))
 *
 * @param dim
 * @param f
 */
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

// TODO(tlutz) remove lambda and use composition operator
case class toGlobal(f: Lambda1) extends Pattern(arity = 1) with FPattern with isGenerable

// TODO(tlutz) remove lambda and use composition operator
case class toLocal(f: Lambda1) extends Pattern(arity = 1) with FPattern with isGenerable

// TODO(tlutz) remove lambda and use composition operator
case class toPrivate(f: Lambda1) extends Pattern(arity = 1) with FPattern with isGenerable

case class Barrier() extends Pattern(arity = 1) with isGenerable {
  var valid = true
}

// TODO(tlutz) remove to ir package or view?
class GroupCall(val group: Group, val outerAe: ArithExpr, val innerAe: ArithExpr, val len: ArithExpr) extends ArithExprFunction {
  "groupComp" + group.id + "(" + outerAe + ", " + innerAe + ", " + len + ")"
}
