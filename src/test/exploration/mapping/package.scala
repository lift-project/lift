package exploration

import ir.TupleType
import ir.ast.UserFun
import lift.arithmetic.SizeVar
import opencl.ir.Float
import rewriting.EnabledMappings

package object mapping {

  val X = SizeVar("X")
  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")

  val v__1 = SizeVar("")
  val v__2 = SizeVar("")
  val v__3 = SizeVar("")
  val v__4 = SizeVar("")
  val v__5 = SizeVar("")
  val v__6 = SizeVar("")
  val v__7 = SizeVar("")

  val idTuple2_float_float = UserFun("idTuple2_float_float", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Float)), TupleType(Float, Float))

  val enabledMappings = EnabledMappings(global0 = true, global01 = true, global10 = false, global012 = false, global210 = false, group0 = true, group01 = false, group10 = true)
}
