package ir.interpreter

import ir.ast._
import scala.collection.immutable.Map

object Interpreter {

  type ValueMap = scala.collection.immutable.Map[Param, Any]

  def apply(f: Lambda, args: Any*) = new {

    def argsArrayToSeq(argsWithArrays: Seq[Any]): Seq[Any] =
      argsWithArrays.map(arrayToSeq)

    def arrayToSeq(arg: Any): Any = {
      arg match {
        case a: Array[_] => a.map(arrayToSeq): Seq[_]
        case a: Any => a
      }
    }

    def compute: Any = {
      f.eval(Map[Param, Any](), argsArrayToSeq(args):_*)
    }

    def asSeq[T]: Seq[T] = {
      compute.asInstanceOf[Seq[_]].map(_.asInstanceOf[T])
    }
  }

}
