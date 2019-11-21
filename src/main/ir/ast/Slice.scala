package ir.ast

import lift.arithmetic.{ArithExpr, Cst}

object Slice {

  /**
   * Slice pattern.
   * Defined using existing primitives, i.e. it is syntactic sugar.
   *
   * The slice pattern has the following high-level semantics:
   *   `Slice(from, until)( [x,,0,,, ..., x,,m - 1,,] ) =
   *     [x,,from,,, .. x,,until - 1,,]`
   *
   * The slice pattern has the following type:
   *   `Slice(from, until) : [a],,n,, -> [ a ],,until-from,,`
   *
   * @param from  The index of the first element to return. Must be bigger than zero.
   * @param until The next index after that of the last element to return.
   *              Must be less or equal to the length of the argument.
   */
  def apply(from: ArithExpr, until: ArithExpr): Lambda = {
    // 2. Get rid of all the values after from by splitting the array into
    //    windows of size "until - from" and discarding all but the first window
    Join() o Head() o Slide(until - from, until - from) o
    // 1. Get rid of the first "from" values by splitting the argument into
    //    windows of size "from" and discarding the first window
    (if (from == Cst(0)) Id()
    else Join() o Tail() o Slide(from, from))
  }
}
