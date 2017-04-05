package opencl.ir.pattern

import ir.ast.isGenerable
import lift.arithmetic.{ArithExpr, PosVar, Var}
import ir._
import ir.ast._
import ir.interpreter.Interpreter.ValueMap

case class SlideSeqPlus(val f: Lambda, size: ArithExpr, step: ArithExpr, var loopVar: Var) extends Pattern(arity = 2) with isGenerable with FPattern
{

  val iterationCount = loopVar.range.numVals

  def checkType(argType: Type, setType: Boolean): Type =  {
    argType match {
      case TupleType(initT, ArrayType(elemT, _)) =>
        f.params(0).t = initT // initial elem type
        f.params(1).t = elemT // array element type

        val bodyType = TypeChecker.check(f.body, setType) // check the body

        if (bodyType != initT)
          throw TypeException(s"SlideSeqPlus operator returns $bodyType instead of the expected $initT")

        ArrayType(initT, 1)

      case _ => throw new TypeException(argType, "TupleType(_, ArrayType(_, _))")
    }
  }

  def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    val init = args.head
    val input = args(1) match { case a: Vector[_] => a }
    Vector( input.foldLeft(init)( (acc, x) => f.eval(valueMap, acc, x) ))
  }

  var shouldUnroll = false

  override def copy(f: Lambda): Pattern = SlideSeqPlus(f,size,step,PosVar("i"))
}

object SlideSeqPlus {
  def apply(f: Lambda2, size: ArithExpr, step: ArithExpr, init: Expr): Lambda1 = fun((x) => SlideSeqPlus(f,size,step,PosVar("i"))(init, x))
}
