package backends.spatial.accel.ir.pattern

import ir.ast.{FPattern, IRNode, Lambda, Lambda1, Pattern}
import ir.interpreter.Interpreter.ValueMap
import ir._
import lift.arithmetic.{ArithExpr, Cst, PosVar, Var}

case class SpForeach(iterSize: ArithExpr,
                     stride: ArithExpr = Cst(1),
                     factor: ArithExpr = Cst(1),
                     override val f: Lambda1)
  extends Pattern(arity = 1) with FPattern {
  assert(f.params.length == 1)

  var loopVar: Var = PosVar("i")
  def iterationCount: ArithExpr = loopVar.range.numVals

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case at @ ArrayTypeWSWC(elemT, s, c) if s == c =>
        f.params(0).t = ArrayType(elemT, iterSize)
        TypeChecker.check(f.body, setType)

        // TODO: make sure that these are divisible:
        val outerSize = (s - (iterSize - stride)) / stride

        ArrayType(elemT, outerSize)

      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }

  override def copy(f: Lambda): Pattern = new SpForeach(iterSize, stride, factor, f)
  var shouldUnroll = false

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)

  override def eval(valueMap: ValueMap, args: Any*): Any = throw new NotImplementedError()
}

object SpForeach {
  def apply(iterSize: ArithExpr,
            stride: ArithExpr = Cst(1),
            factor: ArithExpr = Cst(1),
            f: Lambda): Lambda1 =
    new SpForeach(iterSize, stride,factor, f)
}