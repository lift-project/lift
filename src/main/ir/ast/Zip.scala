package ir.ast

import ir._
import ir.ast.debug.PrintType
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.ArithExpr
import lift.arithmetic.ArithExpr.Math.Min
import lift.arithmetic.simplifier.ExprSimplifier
import opencl.generator.StrictZip

/**
 * Zip pattern.
 * Code for this pattern can be generated.
 *
 * The zip pattern has the following high-level semantics:
 *   <code>Zip(2)( [x,,1,,, ..., x,,n,,], [y,,1,,, ..., y,,n,,] )
 *      = [ (x,,1,,, y,,1,,), ..., (x,,n,,, y,,n,,) ]</code>
 * The definitions for `n > 2` are accordingly.
 *
 * The zip pattern has the following type:
 *   `Zip(2) : [a],,i,, -> [b],,i,, -> [a x b],,i,,`
 * The definitions for `n > 2` are accordingly.
 *
 * @param n The number of arrays which are combined. Must be >= 2.
 */
case class Zip(n : Int) extends Pattern(arity = n) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case tt: TupleType =>
        if (tt.elemsT.length != n) throw new NumberOfArgumentsException

        Zip.computeOutType(tt)
      case _ => throw new TypeException(argType, "TupleType", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    (n, args) match {
      case (2, Seq(a: Vector[_], b: Vector[_])) => a zip b
//      case (3, a, b, c) =>
      case _ => throw new NotImplementedError()
    }
  }

  override def toString: String = "Zip()"
}

object Zip {
  /**
   * Create an instance of the zip pattern.
   * This function infers the number of arrays which are combined with the zip
   * pattern.
   *
   * @param args The arrays to be combined with the zip pattern.
   * @return An instance of the zip pattern combining the arrays given by `args`
   */
  def apply(args : Expr*) : Expr = {
    assert(args.length >= 2)
    Zip(args.length)(args:_*)
  }

def minWithCheck(x: ArithExpr, y: ArithExpr, tt: TupleType): ArithExpr = {
    if (ExprSimplifier(x) == ExprSimplifier(y)) x
    else {
      if (StrictZip()) throw new ZipTypeException(tt)
      else Min(x, y)
    }
  }

  /**
   * Combination is defined the following way:
   * - If the two array types have a capacity, we keep the minimum value.
   * - If the two array types have a size, we keep the minimum value.
   * - If a size (resp. capacity) is not known in one array type, we drop
   *   it and the result will have no size (resp. capacity).
   */
  private def combineArrayTypes(min: (ArithExpr, ArithExpr) => ArithExpr,
                                at1: ArrayType, at2: ArrayType): ArrayType = (at1, at2) match {
    case (ArrayTypeWSWC(_, s1, c1), ArrayTypeWSWC(_, s2, c2)) =>
      ArrayTypeWSWC(UndefType, min(s1, s2), min(c1, c2))
    case (ArrayTypeWS(_, s1), ArrayTypeWS(_, s2)) =>
      ArrayTypeWS(UndefType, min(s1, s2))
    case (ArrayTypeWC(_, c1), ArrayTypeWC(_, c2)) =>
      ArrayTypeWC(UndefType, min(c1, c2))
    case (ArrayType(_), ArrayType(_)) =>
      ArrayType(UndefType)
  }

  /**
   * Compute the type of Zip out the ArrayTypes of its arguments.
   *
   * The potential sizes and capacities are dealt with in the
   * `combineArrayTypes` method above and the element type of the resulting
   * array is a TupleType with all the element types of the input arrays as
   * arguments.
   */
  def computeOutType(tt: TupleType, where: IRNode = null): ArrayType = {
    // make sure all arguments are array types
    val arrayTypes = tt.elemsT.map({
      case (at: ArrayType) => at
      case t => throw new TypeException(t, "ArrayType", where)
    })

    val elemT = TupleType(arrayTypes.map(_.elemT): _*)
    def min(x: ArithExpr, y: ArithExpr): ArithExpr = minWithCheck(x, y, tt)
    arrayTypes.reduce(combineArrayTypes(min, _, _)).replacedElemT(elemT)
  }
}

object Zip3D {

  def apply(arg1: Expr, arg2: Expr, args: Expr*): Expr = {
    val allArgs = arg1 +: arg2 +: args

    val tuple = Param()
    val tuple2 = Param()

    val dim2Args = allArgs.indices.map(Get(tuple, _))
    val dim3Args = allArgs.indices.map(Get(tuple2, _))

    Map(Map(Lambda(Array(tuple2), Zip(dim3Args:_*)))) o Map(Lambda(Array(tuple), Zip(dim2Args:_*))) $ Zip(allArgs:_*)
  }
}

object Zip2D {

  def apply(arg1: Expr, arg2: Expr, args: Expr*): Expr = {

    val allArgs = arg1 +: arg2 +: args

    val tuple = Param()

    val dim2Args = allArgs.indices.map(Get(tuple, _))

    Map(Lambda(Array(tuple), Zip(dim2Args:_*))) $ Zip(allArgs:_*)
  }

}

object ZipND {
  // NB: This is a very expensive operation for a Lift compiler due to the exponential growth of View size with each usage of Get.
  //     A cheaper alternative is ZipNDWithCompactViews() below
  def apply(dims: Int, arg1: Expr, arg2: Expr, args: Expr*): Expr = {
    assert(dims > 0)

    val allArgs = arg1 +: arg2 +: args

    if (dims == 1)
      Zip(allArgs: _*)
    else {

      val zippedArgParam = Param()
      val gets = allArgs.indices.map(Get(zippedArgParam, _))

      GenerateIR.wrapInMaps(Lambda(Array(zippedArgParam), Zip(gets: _*)), dims - 1) $
        ZipND(dims - 1, arg1, arg2, args: _*)
    }
  }
}

object ZipNDWithCompactViews {
  // A less expensive operation due to smaller views, but requires that zipped dimension sizes are known
  def apply(n: Int, dimSizes: Seq[ArithExpr], arg1: Expr, arg2: Expr, args: Expr*): Expr = {
    assert(n > 0)
    assert(n == dimSizes.length)

    val allArgs = arg1 +: arg2 +: args

    if (n == 1)
      Zip(allArgs: _*)
    else {
      dimSizes.tail.foldRight[FunDecl](fun(p => p)) {
        case (dimSize, previousSplits) => Split(dimSize) o previousSplits
      } $ Zip(allArgs.map(arg => GenerateIR.applyNTimes(Join(), n - 1) $ arg): _*)
    }
  }
}
