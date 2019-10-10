package backends.spatial.common.ir.ast

import ir.Type
import ir.ast.UserFun

class SpatialBuiltInFun(override val name: String,
                        override val inTs: Seq[Type],
                        override val outT: Type)
  extends UserFun(name, inTs.map(_.toString).toArray, "", inTs, outT)

object SpatialBuiltInFun {
  def apply(name: String, inTs: Seq[Type], outT: Type): SpatialBuiltInFun =
    new SpatialBuiltInFun(name, inTs, outT)

  def apply(name: String, inT: Type, outT: Type): SpatialBuiltInFun =
    new SpatialBuiltInFun(name, Seq(inT), outT)
}