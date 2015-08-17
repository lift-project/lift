package ir.interpreter

import ir.ast._
import scala.collection.immutable.Map

object Interpreter {

  type ValueMap = scala.collection.immutable.Map[Param, Any]

  def apply(f: Lambda) = new {

    private def argsArrayToSeq(argsWithArrays: Seq[Any]): Seq[Any] =
      argsWithArrays.map(arrayToSeq)

    private def arrayToSeq(arg: Any): Any = {
      arg match {
        case a: Array[_] => a.map(arrayToSeq): Seq[_]
        case a: Any => a
      }
    }

    private def argsArrayToIterator(argsWithArrays: Seq[Any]): Seq[Any] =
      argsWithArrays.map(arrayToIterator)

    private def arrayToIterator(arg: Any): Any = {
      arg match {
        case a: Array[_] => a.map(arrayToIterator).iterator : Iterator[_]
        case a: Any => a
      }
    }

    def ->[R] = new {
      def run(args: Any*): R = {
        f.eval(Map[Param, Any](), argsArrayToIterator(args): _*).asInstanceOf[R]
      }

      def runAndFlatten(args: Any*)(implicit can: CanFlatten[R]): Seq[can.Elem] = {
        can.flatten(run(args:_*))
      }
    }
  }

  sealed trait InnerMost {
    implicit def innerSeq[A]: CanFlatten[Iterator[A]] { type Elem = A } =
      new CanFlatten[Iterator[A]] {
        type Elem = A
        def flatten(iter: Iterator[A]): Seq[A] = iter.toVector
      }
  }
  object CanFlatten extends InnerMost {
    implicit def nestedI[A](implicit inner: CanFlatten[A])
    : CanFlatten[Iterator[A]] { type Elem = inner.Elem } =
      new CanFlatten[Iterator[A]] {
        type Elem = inner.Elem
        def flatten(iter: Iterator[A]) : Seq[inner.Elem] = iter.toVector.flatMap(a => inner.flatten(a))
      }
  }
  sealed trait CanFlatten[-A] {
    type Elem
    def flatten(iter: A): Seq[Elem]
  }

//  sealed trait InnerMost {
//    implicit def innerSeq[A]: CanFlatten[Seq[A]] { type Elem = A } =
//      new CanFlatten[Seq[A]] {
//        type Elem = A
//        def flatten(seq: Seq[A]): Seq[A] = seq
//      }
//  }
//  object CanFlatten extends InnerMost {
//    implicit def nestedSeq[A](implicit inner: CanFlatten[A])
//    : CanFlatten[Seq[A]] { type Elem = inner.Elem } =
//      new CanFlatten[Seq[A]] {
//        type Elem = inner.Elem
//        def flatten(seq: Seq[A]) : Seq[inner.Elem] = seq.flatMap(a => inner.flatten(a))
//      }
//  }
//  sealed trait CanFlatten[-A] {
//    type Elem
//    def flatten(seq: A): Seq[Elem]
//  }

}

