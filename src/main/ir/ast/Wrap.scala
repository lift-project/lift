package ir.ast

import ir._
import ir.interpreter.Interpreter._

/**
  * A pattern for wrapping data into an array of size 1.
  * An example use case is for mapping over one thread: MapLcl(0) o Wrap() o ...
  */
case class Wrap(f: Lambda) extends Pattern(arity = 1) with isGenerable {

  override def toString: String = "Wrap()"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    ArrayTypeWSWC(argType, 1, 1)
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    args.head
  }
}

object Wrap {
  def apply(f: Lambda): Lambda = {
    f.body.t match {
      case at: ArrayType =>
        val size = at match {
          case ats: Size => ats.size
          case atc: Capacity => atc.capacity
          case _ =>
            throw new IllegalArgumentException(
              "Wrap cannot obtain the number of elements in the argument array as the array has no size, nor capacity.")
        }
        Split(size) o f
      case _ => throw new NotImplementedError(
        "Wrapping elements of type other than Array requires a new array generator. See issue #122.")
    }
  }
}