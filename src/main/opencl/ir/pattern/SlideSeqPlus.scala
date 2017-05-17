package opencl.ir.pattern

import ir.ast.isGenerable
import lift.arithmetic.{ArithExpr, PosVar, Var}
import ir._
import ir.ast._
import ir.interpreter.Interpreter.ValueMap

case class SlideSeqPlus(val f: Lambda, size: ArithExpr, step: ArithExpr, var loopVar: Var, var windowVar: Var) extends Pattern(arity = 1) with isGenerable with FPattern
{

  override def toString: String = "SlideSeqPlus(" + f + ", " + size + ", " + step + ")"

  SlideSeqPlus.cnt += 1
  val id = SlideSeqPlus.cnt
  override def hashCode = id.hashCode()

  val iterationCount = loopVar.range.numVals

  def checkType(argType: Type, setType: Boolean): Type =  {
    argType match {
      case ArrayTypeWSWC(t, _, n) =>
        // todo check that the sliding window always ends at the last element of the input
        //if (((n - (size - step)) % step) != Cst(0)) throw new TypeException(argType, "slide args not as")
        val innerLength = size
        val outerLength = (n - (size - step)) / step
        f.params(0).t = ArrayTypeWSWC(t,innerLength)
        ArrayTypeWSWC(TypeChecker.check(f.body,setType), outerLength)
      case _ => throw new TypeException(argType, "ArrayTypeWSWC", this)
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

  override def copy(f: Lambda): Pattern = SlideSeqPlus(f,size,step,PosVar("i"),PosVar("window"))
}

object SlideSeqPlus {
  var cnt: Int = -1
  def apply(f: Lambda1, size: ArithExpr, step: ArithExpr): Lambda1 = fun((x) => SlideSeqPlus(f,size,step,PosVar("i"),PosVar("window"))(x))
}
