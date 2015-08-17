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

    def asSeqFrom[T](implicit can: CanFlatten[T]): Seq[can.Elem] = {
      can.flatten(compute.asInstanceOf[T])
    }

  }

  sealed trait InnerMost {
    implicit def innerSeq[A]: CanFlatten[Seq[A]] { type Elem = A } =
      new CanFlatten[Seq[A]] {
        type Elem = A
        def flatten(seq: Seq[A]): Seq[A] = seq
      }
  }
  object CanFlatten extends InnerMost {
    implicit def nestedSeq[A](implicit inner: CanFlatten[A])
    : CanFlatten[Seq[A]] { type Elem = inner.Elem } =
      new CanFlatten[Seq[A]] {
        type Elem = inner.Elem
        def flatten(seq: Seq[A]) : Seq[inner.Elem] = seq.flatMap(a => inner.flatten(a))
      }
  }
  sealed trait CanFlatten[-A] {
    type Elem
    def flatten(seq: A): Seq[Elem]
  }

  def flattenAll[A](seq: A)(implicit can: CanFlatten[A]): Seq[can.Elem] = {
    can.flatten(seq)
  }

}

