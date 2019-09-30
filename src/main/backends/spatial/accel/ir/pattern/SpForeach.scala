package backends.spatial.accel.ir.pattern

import ir.{ArrayType, ArrayTypeWSWC, Type, TypeChecker, TypeException}
import ir.ast.{AbstractMap, FPattern, IRNode, Lambda, Lambda1, Pattern}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{ArithExpr, Cst, PosVar, Var}

case class SpForeach(override val f: Lambda1,
                     iterSize: ArithExpr,
                     stride: Option[ArithExpr],
                     factor: Option[ArithExpr])
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
        val outerSize = (s - (iterSize - stride.getOrElse(1))) / stride.getOrElse(1)

        ArrayType(elemT, outerSize)

      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }

  override def copy(f: Lambda): Pattern = SpForeach(f, iterSize, stride, factor)
  var shouldUnroll = false

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)

  override def eval(valueMap: ValueMap, args: Any*): Any = throw new NotImplementedError()
}

object SpForeach {
  def apply(iterSize: ArithExpr,
            stride: ArithExpr,
            factor: ArithExpr,
            f: Lambda): Lambda1 =
    new SpForeach(f, iterSize, Some(stride), Some(factor))

  def apply(iterSize: ArithExpr,
            stride: Option[ArithExpr] = None,
            factor: Option[ArithExpr] = None,
            f: Lambda): Lambda1 =
    new SpForeach(f, iterSize, stride, factor)
}