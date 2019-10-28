package backends.spatial.common.generator

import lift.arithmetic._

class SpatialArithmeticMethod private(val obj: Var,
                                      override val name: String,
                                      val param: Option[Int],
                                      range: Range)
  extends ArithExprFunction(name, range) {

  lazy val toSpatialString: String = s"$obj.$name(" + param.getOrElse("").toString + ")"

  override lazy val digest: Int = HashSeed ^ /*range.digest() ^*/ name.hashCode ^ param.getOrElse(0)

  override val HashSeed = 0x31111112

  override def equals(that: Any): Boolean = that match {
    case m: SpatialArithmeticMethod => this.obj.equals(m.obj) && this.name.equals(m.name) && this.param == m.param
    case _ => false
  }

  override lazy val (min : ArithExpr, max: ArithExpr) = (range.min.min, range.max.max)
  override lazy val sign: Sign.Value = Sign.Positive

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr =
    f(new SpatialArithmeticMethod(obj.visitAndRebuild(f).asInstanceOf[Var], name, param, range.visitAndRebuild(f)))
}

object SpatialArithmeticMethod {
  def apply(obj: Var, name: String, param: Option[Int] = None, range: Range = RangeUnknown) : SpatialArithmeticMethod =
    new SpatialArithmeticMethod(obj, name, param, range)
}