package backends.spatial.accel.ir

import ir.ast.{FPattern, Lambda, Pattern}
import lift.arithmetic.ArithExpr

package object pattern {
  trait Sequential
  trait Piped {
    val factor: ArithExpr
  }

  abstract class SchedulingPattern(override val f: Lambda) extends Pattern(arity = 1) with FPattern
}
