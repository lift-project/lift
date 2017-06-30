package opencl.ir.pattern

import lift.arithmetic.PosVar
import ir._
import ir.ast._

case class ReduceWhileSeq(override val f: Lambda, p: Lambda)
  extends AbstractReduce(f, PosVar("i")) with isGenerable {
  assert(f.body.isConcrete)

  var pmem : Memory = UnallocatedMemory

  override def checkType(argType: Type, setType: Boolean): Type =  {
    // TODO: Duplication with AbstractPartRed. ReduceWhileSeq is somewhat less strict.
    argType match {
      case TupleType(initT, ArrayType(elemT)) =>
        f.params(0).t = initT // initial elem type
        f.params(1).t = elemT // array element type

        val bodyType = TypeChecker.check(f.body, setType) // check the body

        if (bodyType != initT)
          throw new TypeException(bodyType, initT, this)

        p.params(0).t = initT
        p.params(1).t = elemT

        val predType = TypeChecker.check(p.body, setType)

        if (predType != opencl.ir.Int)
        	throw new TypeException(predType, opencl.ir.Int, this)

        ArrayTypeWSWC(initT, 1)

      case _ => throw new TypeException(argType, "TupleType(_, ArrayType(_, _))", this)
    }
  }

  override def copy(f: Lambda): Pattern = ReduceWhileSeq(f, p)
  var shouldUnroll = false
}

class ReduceWhileSeqUnroll(override val f: Lambda, override val p: Lambda) extends ReduceWhileSeq(f,p) {
  shouldUnroll = false
}

object ReduceWhileSeqUnroll {
  def apply(f: Lambda, p: Lambda) = new ReduceWhileSeqUnroll(f, p)
  def apply(f: Lambda2, p: Lambda2, init: Expr): Lambda1 = fun((x) => ReduceWhileSeqUnroll(f, p)(init, x))
}

object ReduceWhileSeq {
  def apply(f: Lambda2, p: Lambda2, init: Expr): Lambda1 = fun((x) => ReduceWhileSeq(f, p)(init, x))
}
