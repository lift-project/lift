package opencl.ir.pattern

import ir.ast.isGenerable
import lift.arithmetic.ArithExpr
import ir.ast._

case class ScanPlus(size: ArithExpr, step: ArithExpr) extends Pattern(arity = 1) with isGenerable {
{

}
