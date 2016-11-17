package opencl.ir.ast

import ir.ast.UserFun
import ir.Type

class OpenCLBuiltInFun(override val name: String,
                       override val inTs: Seq[Type],
                       override val outT: Type
                      ) extends UserFun(name, inTs.map(_.toString).toArray, "", inTs, outT)

object OpenCLBuiltInFun {
  def apply(name: String, inTs: Seq[Type], outT: Type): OpenCLBuiltInFun =
    new OpenCLBuiltInFun(name, inTs, outT)

  def apply(name: String, inT: Type, outT: Type): OpenCLBuiltInFun =
    new OpenCLBuiltInFun(name, Seq(inT), outT)
}
