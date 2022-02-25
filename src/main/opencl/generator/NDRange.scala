package opencl.generator

import lift.arithmetic.{?, ArithExpr, Cst}

object NDRange {
  def numberOfWorkgroups(global: NDRange, local:NDRange): Int = {
    (global(0).eval / local(0).eval) *
      (global(1).eval / local(1).eval) *
        (global(2).eval / local(2).eval)
  }

}

case class NDRange(x: ArithExpr, y: ArithExpr = 1, z: ArithExpr = 1) {
  def apply(i: Int): ArithExpr = {
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IndexOutOfBoundsException
    }
  }

  def evaluated: NDRange = {
    this.copy(
      x = try { x.eval } catch { case _: Throwable => x },
      y = try { y.eval } catch { case _: Throwable => y },
      z = try { z.eval } catch { case _: Throwable => z }
    )
  }

  lazy val numberOfDimensionsNotOne: Int = {
    var c = 0
    if (x != Cst(1)) c = c + 1
    if (y != Cst(1)) c = c + 1
    if (z != Cst(1)) c = c + 1
    c
  }

  lazy val numberOfWorkItems: Int = {
    x.eval * y.eval * z.eval
  }

  def map(f: ArithExpr => ArithExpr): NDRange = {
    this.copy(f(x), f(y), f(z))
  }

  def exists(p: ArithExpr => Boolean): Boolean = {
    p(x) || p(y) || p(z)
  }

  def forall(p: ArithExpr => Boolean): Boolean = {
    p(x) && p(y) && p(z)
  }

  override def toString: String = {
    if (x == ? && y == ? && z == ?)
      //represent unknown size, let code generator decide how to generate the local or global size
      ""
    else
      s"$x,$y,$z"
  }
}
