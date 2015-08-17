package ir.interpreter

import ir.ast._
import scala.collection.immutable.Map

object Interpreter {

  type ValueMap = scala.collection.immutable.Map[Param, Any]

  def apply(f: Lambda) = new {

    private def argsArrayToVector(argsWithArrays: Seq[Any]): Seq[Any] =
      argsWithArrays.map(arrayToVector)

    private def arrayToVector(arg: Any): Any = {
      arg match {
        case a: Array[_] => a.map(arrayToVector).toVector : Vector[_]
        case a: Any => a
      }
    }

    def ->[R] = new {
      def run(args: Any*): R = {
        val res = f.eval(Map[Param, Any](), argsArrayToVector(args): _*).asInstanceOf[R]
        res
      }

      def runAndFlatten(args: Any*)(implicit can: CanFlatten[R]): Seq[can.Elem] = {
        can.flatten(run(args:_*))
      }
    }
  }

  sealed trait InnerMost {
    implicit def innerSeq[A]: CanFlatten[Vector[A]] { type Elem = A } =
      new CanFlatten[Vector[A]] {
        type Elem = A
        def flatten(v: Vector[A]): Vector[A] = v.toVector
      }
  }
  object CanFlatten extends InnerMost {
    implicit def nestedI[A](implicit inner: CanFlatten[A])
    : CanFlatten[Vector[A]] { type Elem = inner.Elem } =
      new CanFlatten[Vector[A]] {
        type Elem = inner.Elem
        def flatten(v: Vector[A]) : Vector[inner.Elem] = v.toVector.flatMap(vi => inner.flatten(vi))
      }
  }
  sealed trait CanFlatten[-A] {
    type Elem
    def flatten(v: A): Vector[Elem]
  }

}

