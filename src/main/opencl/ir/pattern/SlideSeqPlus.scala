package opencl.ir.pattern

import ir.ast.isGenerable
import lift.arithmetic.{ArithExpr, PosVar, Var}
import ir._
import ir.ast._
import ir.interpreter.Interpreter.ValueMap

case class SlideSeqPlus(val f: Lambda, size: ArithExpr, step: ArithExpr, var loopVar: Var) extends Pattern(arity = 1) with isGenerable with FPattern
{

  val iterationCount = loopVar.range.numVals

  def checkType(argType: Type, setType: Boolean): Type =  {
    argType match {
      case ArrayType(t, n) =>
        // todo check that the sliding window always ends at the last element of the input
        //if (((n - (size - step)) % step) != Cst(0)) throw new TypeException(argType, "slide args not as")
        val innerLength = size
        val outerLength = (n - (size - step)) / step
        f.params(0).t = ArrayType(t,innerLength)
        ArrayType(TypeChecker.check(f.body,setType), outerLength)
      case _ => throw new TypeException(argType, "ArrayType")
    }
  }

  def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    val init = args.head
    val input = args(1) match { case a: Vector[_] => a }
  // needs to be updated (from reduce)
    Vector( input.foldLeft(init)( (acc, x) => f.eval(valueMap, acc, x) ))
  }

  var shouldUnroll = false

  override def copy(f: Lambda): Pattern = SlideSeqPlus(f,size,step,PosVar("i"))
}

object SlideSeqPlus {
  def apply(f: Lambda1, size: ArithExpr, step: ArithExpr): Lambda1 = fun((x) => SlideSeqPlus(f,size,step,PosVar("i"))(x))
}
