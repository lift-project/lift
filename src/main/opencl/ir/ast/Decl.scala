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

abstract class GenerableMap(f: Lambda1, name: String, loopVar: Var)
  extends AbstractMap(f, name, loopVar) with isGenerable

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

case class MapGlb(dim: Int, override val f: Lambda1)
  extends GenerableMap(f, "MapGlbl", Var("gl_id"))

object MapGlb {
  def apply(f: Lambda1) = new MapGlb(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda) => new MapGlb(dim, f)
}

case class MapWrg(dim: Int, override val f: Lambda1)
  extends GenerableMap(f, "MapWrg", Var("wg_id"))

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
case class MapLcl(dim: Int, override val f: Lambda1)
  extends GenerableMap(f, "MapLcl", Var("l_id"))

object MapLcl {
  def apply(f: Lambda1) = new MapLcl(0, f) // o is default

  def apply(dim: Int) = (f: Lambda1) => new MapLcl(dim, f)
}

case class MapWarp(override val f: Lambda1)
  extends GenerableMap(f, "MapWarp", Var("warp_id"))

case class MapLane(override val f: Lambda1)
  extends GenerableMap(f, "MapLane", Var("lane_id"))

case class MapSeq(override val f: Lambda1) extends GenerableMap(f, "MapSeq",
                                                                Var("i"))

// Reductions

case class ReduceSeq(override val f: Lambda2)
  extends AbstractReduce(f, Var("i")) with isGenerable

object ReduceSeq {
  def apply(f: Lambda2, init: Expr): Lambda1 = fun((x) => ReduceSeq(f)(init, x))
}

case class ReduceHost(override val f: Lambda2)
  extends AbstractReduce(f, Var("i")) with isGenerable

object ReduceHost {
  def apply(f: Lambda2, init: Expr): Lambda1 = fun((x) => ReduceHost(f)(init, x))
}

// TODO(tlutz) remove lambda and use composition operator
case class toGlobal(f: Lambda1) extends Pattern(arity = 1)
                                        with FPattern with isGenerable

// TODO(tlutz) remove lambda and use composition operator
case class toLocal(f: Lambda1) extends Pattern(arity = 1)
                                       with FPattern with isGenerable

// TODO(tlutz) remove lambda and use composition operator
case class toPrivate(f: Lambda1) extends Pattern(arity = 1)
                                         with FPattern with isGenerable

case class Barrier() extends Pattern(arity = 1) with isGenerable {
  var valid = true
}

// TODO(tlutz) remove to ir package or view?
class GroupCall(val group: Group, val outerAe: ArithExpr, val innerAe: ArithExpr, val len: ArithExpr) extends ArithExprFunction {
  "groupComp" + group.id + "(" + outerAe + ", " + innerAe + ", " + len + ")"
}
