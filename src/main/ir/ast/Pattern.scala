package ir.ast

import arithmetic.{ArithExpr, Var}
import ir.{ArrayType, Type, UndefType}

abstract class Pattern(override val params: Array[Param]) extends FunDecl(params) {
  def this(arity: Int) = this(Array.fill(arity)(Param(UndefType)))
}

trait FPattern {
  def f: Lambda
}

abstract class AbstractMap(f:Lambda) extends Pattern(arity = 1) with FPattern

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
case class Map(f:Lambda1) extends AbstractMap(f) {
  override def apply(args: Expr*): MapCall = mapCall(args:_*)

  override def $(that: Expr): MapCall = mapCall(that)

  private def mapCall(args: Expr*): MapCall = {
    assert(args.length == 1)
    new MapCall("Map", Var(""), this, args(0))
  }
}

object Map {
  def apply(f: Lambda1, expr: Expr): MapCall = {
    Map(f).mapCall(expr)
  }
}

abstract class GenerableMap(f:Lambda) extends AbstractMap(f) with isGenerable

abstract class AbstractPartRed(f:Lambda) extends Pattern(arity = 2) with FPattern {
  def init: Value = params(0) match { case v: Value => v}
}

abstract class AbstractReduce(f:Lambda) extends AbstractPartRed(f)

/**
 * Perform a reduction on the input.
 *
 * Applicable rules:
 *  - Reduce(f) => Reduce(f) o PartRed(f)
 *  - Reduce(f) => ReduceSeq(f)
 *
 * @param f The lambda to apply to the next element and partial result
 */
case class Reduce(f: Lambda) extends AbstractReduce(f) {
  override def apply(args: Expr*) : ReduceCall = reduceCall(args:_*)

  private def reduceCall(args: Expr*): ReduceCall = {
    assert(args.length == 2)
    new ReduceCall(Var("i"), this, args(0), args(1))
  }
}
object Reduce {
  def apply(f: Lambda2, init: Value): Lambda1 = fun((x) => Reduce(f)(init, x))
  def apply(f: Lambda2, init: Value, expr: Expr): ReduceCall = Reduce(f)(init, expr)
}

/**
 * Partial reduction
 *
 * Applicable re-write rules:
 *  - PartRed(f) => Reduce(f)
 *  - PartRed(f) => PartRed(f) o Reorder
 *  - PartRed(f) => Iterate(k, PartRed(f)) (input a multiple of k)
 *  - PartRed(f) => Join() o Map(PartRed(f)) o Split(k) (input a multiple of k)
 *
 * @param f The lambda to apply to the next element and partial result
 */
case class PartRed(f: Lambda2) extends AbstractPartRed(f) with FPattern {
  override def apply(args: Expr*) : ReduceCall = reduceCall(args:_*)

  private def reduceCall(args: Expr*): ReduceCall = {
    assert(args.length == 2)
    new ReduceCall(Var("i"), this, args(0), args(1))
  }
}
object PartRed {
  def apply(f: Lambda2, init: Value): Lambda1 = fun((x) => PartRed(f)(init, x))
  def apply(f: Lambda2, init: Value, expr: Expr): ReduceCall = PartRed(f)(init, expr)
}

/**
 * Applicable rules:
 *  - Join() o Split(chunkSize) | Split(chunkSize) o Join(chunkSize) => id
 */
case class Join() extends Pattern(arity = 1) with isGenerable

/**
 * Splits the input into chunks of <code>chunkSize</code>.
 *
 * Applicable rules:
 *  - Join() o Split(chunkSize) | Split(chunkSize) o Join(chunkSize) => id
 *
 * @param chunkSize Size of the chunks the input will be split into
 */
case class Split(chunkSize: ArithExpr) extends Pattern(arity = 1) with isGenerable

/**
 *
 * Applicable rules:
 *  - asScalar() o asVector(len) | asVector(len) o asScalar(len) => id
 */
case class asScalar() extends Pattern(arity = 1) with isGenerable

/**
 *
 * Applicable rules:
 *  - asScalar() o asVector(len) | asVector(len) o asScalar(len) => id
 *
 * @param len Vector length
 */
case class asVector(len: ArithExpr) extends Pattern(arity = 1) with isGenerable

/*
// TODO: discuss if this should be a Fun again (if so, this has to be replaced in the very first pass before type checking)
case class Vectorize(n: Expr, f: Fun) extends FPattern {
  def isGenerable() = true
  override def copy() = Vectorize(n, f)
}
*/

object Vectorize {
  class Helper(n: ArithExpr) {
    def apply(uf: UserFun): UserFun = uf.vectorize(n)

    def apply(v: Value): Value = v.vectorize(n)

    def apply(p: Param): Param = p.vectorize(n)
  }

  @deprecated("Use function.vectorize(ArithExpr) instead")
  def apply(n: ArithExpr): Helper = new Helper(n)
}


/**
 * Iterate the lambda <code>f</code> <code>n</code> such that the output of one iteration is the input
 * for the next.
 *
 * Applicable rules:
 *  - Iterate(n+m, f) => Iterate(n, f) o Iterate(m, f)
 *
 * @param n Number of times to iterate
 * @param f Lamda to use for iteration
 */
case class Iterate(n: ArithExpr, f: Lambda1) extends Pattern(arity = 1) with FPattern with isGenerable {

  override def apply(args: Expr*): IterateCall = iterateCall(args: _*)

  override def $(that: Expr): IterateCall = iterateCall(that)

  private def iterateCall(args: Expr*): IterateCall = {
    assert(args.length == 1)
    new IterateCall(this, args(0))
  }
}

object Iterate {
  def apply(n: ArithExpr): ((Lambda1) => Iterate)  = (f: Lambda1) => Iterate(n ,f)

  def varName(): String = "iterSize"
}

case class Filter() extends FunDecl(arity = 2) with isGenerable

object Filter {
  def apply(input: Param, ids: Param): FunCall = {
    Filter()(input, ids)
  }
}

case class Tuple(n: Int) extends FunDecl(arity = n) with isGenerable

object Tuple {
  def apply(args : Expr*) : FunCall = {
    assert(args.length >= 2)
    Tuple(args.length)(args:_*)
  }
}

case class Zip(n : Int) extends FunDecl(arity = n) with isGenerable

object Zip {
  def apply(args : Expr*) : FunCall = {
    assert(args.length >= 2)
    Zip(args.length)(args:_*)
  }
}

case class Unzip() extends FunDecl(arity = 1) with isGenerable

/**
 * Transpose on output
 */
case class TransposeW() extends Pattern(arity = 1) with isGenerable

/**
 * Transpose on input
 */
case class Transpose() extends Pattern(arity = 1) with isGenerable

object Group {
  var cnt: Int = -1

  // Predefined out-of-boundary cases
  val edgeNeg: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => 0
  val edgePos: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => len - 1
  val reflectNeg: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => -1 - idx
  val reflectPos: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => len - idx
  val wrapNeg: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => len + idx
  val wrapPos: (ArithExpr, ArithExpr) => ArithExpr = (idx, len) => idx - 1
}

case class Group(relIndices: Array[Int],
                 negOutOfBoundsF: (ArithExpr, ArithExpr) => ArithExpr,
                 posOutOfBoundsF: (ArithExpr, ArithExpr) => ArithExpr) extends Pattern(arity = 1) with isGenerable {
  Group.cnt += 1
  val id = Group.cnt
}

object Group2D {
  def apply(relColumns: Array[Int],
            relRows: Array[Int],
            negOOB: (ArithExpr, ArithExpr) => ArithExpr,
            posOOB: (ArithExpr, ArithExpr) => ArithExpr): CompFun = {
    Map(
         Map(
              Transpose()
            ) o Group(relColumns, negOOB, posOOB) o Transpose()
       ) o Group(relRows, negOOB, posOOB)
  }
}

/**
 * Reorder on input
 * @param idx The function to use for reordering
 */
case class Gather(idx: IndexFunction) extends Pattern(arity = 1) with isGenerable

/**
 * Reorder on output
 * @param idx The function to use for reordering
 */
case class Scatter(idx: IndexFunction) extends Pattern(arity = 1) with isGenerable

case class Head() extends Pattern(arity = 1) with isGenerable

case class Tail() extends Pattern(arity = 1) with isGenerable

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
    val outerType = t match { case at: ArrayType => at }
    val innerType = outerType.elemT match { case at: ArrayType => at }

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