package ir.interpreter

import ir.ast._
import scala.collection.immutable.Map

object Interpreter {

  type ValueMap = scala.collection.immutable.Map[Param, Any]

  def apply(f: Lambda, args: Any*): Array[_] = {
    f.eval(Map[Param, Any](), args:_*)
  }

}
