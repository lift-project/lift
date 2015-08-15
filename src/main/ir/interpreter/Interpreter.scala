package ir.interpreter

import ir.ast._
import scala.collection.immutable.Map

object Interpreter {

  type ValueMap = scala.collection.immutable.Map[Param, Any]

  def apply(f: Lambda, args: Any*) = new {

    val argsNoArray = args.map({
      case a: Array[_] => a: Seq[_]
      case a: Any => a
    })

    def compute: Seq[_] = {
      val t0 = System.nanoTime()
      val res = f.eval(Map[Param, Any](), argsNoArray: _*).asInstanceOf[Seq[_]]
      val t1 = System.nanoTime()
      println("compute: " + (t1 - t0) + "ns")
      res
    }

    def asFloatSeq: Seq[Float] = {
      compute.map(_.asInstanceOf[Float])
    }

    def asIntSeq: Seq[Int] = {
      compute.map(_.asInstanceOf[Int])
    }
  }

}
