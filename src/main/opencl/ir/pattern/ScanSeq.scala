package opencl.ir.pattern

import ir._
import ir.ast.{Expr, FPattern, IRNode, Lambda, Lambda1, Lambda2, Pattern, fun}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{ArithExpr, PosVar, Var}

/**
  * Created by federico on 04/10/17.
  */
case class ScanSeq(f:Lambda, var loopVar: Var) extends Pattern(arity = 2) with FPattern {
  override def eval(valueMap: ValueMap, args: Any*): Any = {
    throw new NotImplementedError()
  }
  var shouldUnroll = false

  //ScanSeq::((a, b) -> a) -> a -> [b]_n -> [a]_n
  override def checkType(argType: Type, setType: Boolean): Type = {
    argType match {
      case TupleType(initT, arr@ArrayType(elemT)) =>
        f.params(0).t = initT // initial elem type => a
        f.params(1).t = elemT // array element type => b

        val bodyType = TypeChecker.check(f.body, setType) // check the body (make sure it returns an a

        if (bodyType != initT)
          throw TypeException(s"ScanSeq operator returns $bodyType instead of the expected $initT")
        arr.replacedElemT(initT) //generate [a]_n from [b]_n

      case _ => throw new TypeException(argType, "TupleType(_, ArrayType(_, _))", this)
    }
  }

  def iterationCount: ArithExpr = loopVar.range.numVals

  override def copy(f: Lambda): ScanSeq = ScanSeq(f, PosVar("i"))

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)
}
object ScanSeq {
  def apply(f: Lambda2, init: Expr): Lambda1 = fun((x) => ScanSeq(f, PosVar("i"))(init, x))
}
