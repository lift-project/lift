package backends.spatial.accel.ir.pattern

import ir.Type
import ir.ast.UserFun
import lift.arithmetic.ArithExpr

/**
 * Representation of a user function that performs burst transfer.
 * Bursted operation is determined by the address spaces:
 * 1. Burst load is issued if the function reads from DRAM and writes into SRAM memory.
 * 2. Burst store is issued if the function reads from SRAM and writes into DRAM memory.
 *
 * Can only be used for functions of one argument.
 */
case class BurstUserFun(override val name: String, override val paramNames: Array[String],
                        override val body: String, override val inTs: Seq[Type],
                        override val outT: Type, factor: ArithExpr)
  extends UserFun(name, paramNames, body, inTs, outT) {
  assert(paramNames.length == 1)

  override def setScalaFun(f: Seq[Any] => Any): BurstUserFun = {
    scalaFun = f
    this
  }

  override def vectorize(n: ArithExpr): UserFun = throw new NotImplementedError()

  override def equals(other: Any): Boolean = other match {
    case that: BurstUserFun => super.equals(other) && that.factor == factor
    case _ => false
  }
}

object BurstUserFun {
  /**
   * Constructor for creating instances of BurstUserFun.
   * This provides convenience for creating instances with a single parameter.
   *
   * @param name      The name of the function. This has to follow standard C naming conventions.
   * @param paramName The parameter name.
   * @param body      The body of the function as a string.
   *                  The body currently must be valid OpenCL C code.
   * @param inT       The type of the parameter.
   * @param outT      The return type of the user function.
   * @param factor    Burst factor.
   * @return
   */
  def apply(name: String, paramName: String, body: String,
            inT: Type, outT: Type, factor: ArithExpr): BurstUserFun = {
    new BurstUserFun(name, Array(paramName), body, Seq(inT), outT, factor)
  }

  def apply(uf: UserFun, factor: ArithExpr): BurstUserFun = {
    assume(uf.paramNames.length == 1, "BurstUserFun can only have one argument")

    apply(uf.name, uf.paramNames.head, uf.body, uf.inTs.head, uf.outT, factor)
  }
}