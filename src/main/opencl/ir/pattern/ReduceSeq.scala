package opencl.ir.pattern

import lift.arithmetic.PosVar
import ir._
import ir.ast._

case class ReduceSeq(override val f: Lambda)
  extends AbstractReduce(f, PosVar("i"))  {
  assert(f.body.isConcrete)

  override def checkType(argType: Type, setType: Boolean): Type =  {
    // TODO: Duplication with AbstractPartRed. ReduceSeq is somewhat less strict.
    argType match {
      case TupleType(initT, ArrayType(elemT)) =>
        f.params(0).t = initT // initial elem type
        f.params(1).t = elemT // array element type

        val bodyType = TypeChecker.check(f.body, setType) // check the body

        if (bodyType != initT)
          throw TypeException(s"ReduceSeq operator returns $bodyType instead of the expected $initT")

        ArrayTypeWSWC(initT, 1)

      case _ => throw new TypeException(argType, "TupleType(_, ArrayType(_, _))", this)
    }
  }

  override def copy(f: Lambda): Pattern = ReduceSeq(f)
  var shouldUnroll = false
}

class ReduceSeqUnroll(override val f: Lambda) extends ReduceSeq(f) {
  shouldUnroll = true
  override def toString: String = s"ReduceSeqUnroll($f)"

  override def copy(f: Lambda): Pattern = ReduceSeqUnroll(f)
}

object ReduceSeqUnroll {
  def apply(f: Lambda) = new ReduceSeqUnroll(f)
  def apply(f: Lambda2, init: Expr): Lambda1 = fun((x) => ReduceSeqUnroll(f)(init, x))
}

object ReduceSeq {
  def apply(f: Lambda2, init: Expr): Lambda1 = fun((x) => ReduceSeq(f)(init, x))
}
