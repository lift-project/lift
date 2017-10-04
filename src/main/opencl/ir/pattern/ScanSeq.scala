package opencl.ir.pattern

import ir._
import ir.ast.{Expr, Lambda, Lambda1, Lambda2, Pattern, fun}
import ir.interpreter.Interpreter.ValueMap

/**
  * Created by federico on 04/10/17.
  */
case class ScanSeq(f:Lambda) extends Pattern(arity = 2) {
  override def eval(valueMap: ValueMap, args: Any*): Any = {
    throw new NotImplementedError()
  }

  override def checkType(argType: Type, setType: Boolean): Type = {
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
}
object ScanSeq {
  def apply(f: Lambda2, init: Expr): Lambda1 = fun((x) => ScanSeq(f)(init, x))
}
