package backends.spatial.accel.ir.pattern

import ir._
import ir.ast._
import lift.arithmetic.{ArithExpr, PosVar}

case class ReduceParStride(override val f: Lambda, s: ArithExpr, p: ArithExpr)
  extends AbstractReduce(f, PosVar("i"))  {
  assert(f.body.isConcrete)

  override def checkType(argType: Type, setType: Boolean): Type =  {
    // TODO: Duplication with AbstractPartRed. ReduceParStride is somewhat less strict.
    argType match {
      case TupleType(initT, ArrayType(elemT)) =>
        f.params(0).t = initT // initial elem type
        f.params(1).t = elemT // array element type

        val bodyType = TypeChecker.check(f.body, setType) // check the body

        if (bodyType != initT)
          throw TypeException(s"ReduceParStride operator returns $bodyType instead of the expected $initT")

        ArrayTypeWSWC(initT, 1)

      case _ => throw new TypeException(argType, "TupleType(_, ArrayType(_, _))", this)
    }
  }

  def copy(f: Lambda): Pattern = ReduceParStride(f, s, p)
}

object ReduceParStride {
  def apply(f: Lambda2, init: Expr, s: ArithExpr, p: ArithExpr): Lambda1 =
    fun((x) => ReduceParStride(f, s, p)(init, x))
}




