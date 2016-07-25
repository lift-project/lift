package opencl.generator

import apart.arithmetic._

class OclFunction private (name: String, val param: Int, range: Range)
  extends ArithExprFunction(name, range) {

  lazy val toOCLString = s"$name($param)"

  override lazy val digest: Int = HashSeed ^ /*range.digest() ^*/ name.hashCode ^ param

  override val HashSeed = 0x31111111

  override def equals(that: Any) = that match {
    case f: OclFunction => this.name.equals(f.name) && this.param == f.param
    case _ => false
  }

  override lazy val (min : ArithExpr, max: ArithExpr) = (range.min.min, range.max.max)
  override lazy val sign: Sign.Value = Sign.Positive

  override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr =
    f(new OclFunction(name, param, range.visitAndRebuild(f)))

}

object OclFunction {
  def apply(name: String, param: Int, range: Range = RangeUnknown) : OclFunction =
    new OclFunction(name, param, range)
}

object get_num_groups {
  def apply(param:Int, range : Range = ContinuousRange(1, PosInf)) =
    OclFunction("get_num_groups", param, range)
}

object get_global_size {
  def apply(param: Int, range : Range = ContinuousRange(1, PosInf)) =
    OclFunction("get_global_size", param, range)
}

object get_local_size {
  def apply(param: Int, range : Range = ContinuousRange(1, PosInf)) =
    OclFunction("get_local_size", param, range)
}

object get_local_id {
  def apply(param:Int, range : Range) =
    OclFunction("get_local_id", param, range)

  def apply(param:Int) =
    OclFunction("get_local_id", param, ContinuousRange(0, get_local_size(param)))
}

object get_global_id {
  def apply(param:Int, range : Range) =
    OclFunction("get_global_id", param, range)

  def apply(param:Int) =
    OclFunction("get_global_id", param, ContinuousRange(0, get_global_size(param)))
}

object get_group_id {
  def apply(param:Int, range : Range) =
    OclFunction("get_group_id", param, range)

  def apply(param:Int) =
    OclFunction("get_group_id", param, ContinuousRange(0, get_num_groups(param)))

}
