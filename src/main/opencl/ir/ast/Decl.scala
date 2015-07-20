package opencl.ir.ast

import arithmetic.{ArithExpr, ArithExprFunction, Var}
import ir.ast._

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
 */


// TODO(tlutz) remove to ir package or view?
class GroupCall(val group: Group,
                val outerAe: ArithExpr,
                val innerAe: ArithExpr,
                val len: ArithExpr) extends ArithExprFunction {
  "groupComp" + group.id + "(" + outerAe + ", " + innerAe + ", " + len + ")"
}
