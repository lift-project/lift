package opencl.ir.pattern

import ir.ast.{FPattern, Lambda, Lambda1, Lambda2, Pattern, fun}
import ir.interpreter.Interpreter.ValueMap
import ir.{ArrayType, Type, TypeChecker, TypeException}
import lift.arithmetic.{PosVar, Var}

/**
 * This is internal, see the `InsertionSort` object below for the high-level
 * documentation.
 *
 * @param f the comparison function.
 * @param loopRead the index used to read the data from the input array
 * @param loopWrite the index used to write data to the output array.
 */
case class InsertionSortSeq(f: Lambda2, var loopRead: Var, var loopWrite: Var) extends Pattern(arity=1) with FPattern {
  override def checkType(argType: Type, setType: Boolean): Type = {
    argType match {
      case at @ ArrayType(ty) =>
        f.params.foreach(p => p.t = ty)
        TypeChecker.assertTypeIs(f.body, opencl.ir.Int, setType)
        // The return type is always the input type
        at
      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }
  
  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case a: Vector[_] =>
        a.sortWith(f.eval(valueMap, _, _).asInstanceOf[Boolean])
    }
  }
  
  def copy(f: Lambda): InsertionSortSeq =
    InsertionSortSeq(f, PosVar("s"), PosVar("t"))
}

/**
 * A sequential implementation of the insertion sort.
 *
 * Type:
 *
 *   `InsertionSeq(cmp): [a] → [a]`
 *   where `cmp: (a → a → bool)`
 *
 *   The two arrays have the same size and capacity characteristics i.e. if
 *   they are in the type of the input, their are in the type output with the
 *   same value.
 *
 * Semantics:
 *
 *   - The first argument is a comparison function: `cmp x y` is true iff `x < y`
 *     regarding to the order we want to use for sorting.
 *
 *   - The sort is stable: if  `x` appears before `y` in the input and `x = y`,
 *     then `x` will appear before `y` in the output.
 *
 *   Hence, if `ys = InsertionSort(compare) $ xs`, then
 *   for all `i ≤ j`, `!compare(ys[j], ys[i])`
 */
object InsertionSortSeq {
  def apply(compare: Lambda2): Lambda1 = {
    fun(l =>
      InsertionSortSeq(compare, PosVar("s"), PosVar("t")) $ l
    )
  }
}