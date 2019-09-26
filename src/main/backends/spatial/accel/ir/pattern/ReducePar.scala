package backends.spatial.accel.ir.pattern

import ir._
import ir.ast._
import lift.arithmetic.{ArithExpr, PosVar}

case class ReducePar(override val f: Lambda, p: ArithExpr)
  extends AbstractReduce(f, PosVar("i"))  {
  assert(f.body.isConcrete)

  override def checkType(argType: Type, setType: Boolean): Type =  {
    // TODO: Duplication with AbstractPartRed. ReducePar is somewhat less strict.
    argType match {
      case TupleType(initT, ArrayType(elemT)) =>
        f.params(0).t = initT // initial elem type
        f.params(1).t = elemT // array element type

        val bodyType = TypeChecker.check(f.body, setType) // check the body

        if (bodyType != initT)
          throw TypeException(s"ReducePar operator returns $bodyType instead of the expected $initT")

        ArrayTypeWSWC(initT, 1)

      case _ => throw new TypeException(argType, "TupleType(_, ArrayType(_, _))", this)
    }
  }

  def copy(f: Lambda): Pattern = ReducePar(f, p)
}


object ReducePar {
  def apply(f: Lambda2, init: Expr, p: ArithExpr): Lambda1 = fun((x) => ReducePar(f, p)(init, x))
}

