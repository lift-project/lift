package backends.spatial.accel

import _root_.ir.ast.UserFun
import _root_.ir.ast.Value
import backends.spatial.common.ir._
import scala.language.implicitConversions

package object ir {
  val add: UserFun = UserFun("add", Array("x", "y"), "x + y", Seq(Float, Float), Float).
    setScalaFun( xs => xs.head.asInstanceOf[Float] + xs(1).asInstanceOf[Float] )

  val mult: UserFun = UserFun("mult", Array("l", "r"), "l * r", Seq(Float, Float), Float)

  implicit def FloatToValue(f: Float): Value = Value(f.toString + "f", Float)
}
