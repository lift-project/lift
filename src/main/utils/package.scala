import lift.arithmetic.ArithExpr

package object utils {
  def log(value: ArithExpr, base: ArithExpr): Int = (Math.log(value.evalInt) / Math.log(base.evalInt)).toInt
}
