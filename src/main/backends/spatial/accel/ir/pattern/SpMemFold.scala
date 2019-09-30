package backends.spatial.accel.ir.pattern

import ir.ast.{Expr, IRNode, Lambda, Lambda1, Lambda2, Pattern, fun}
import ir.interpreter.Interpreter.ValueMap
import ir._
import lift.arithmetic.{ArithExpr, PosVar, SimplifiedExpr, Var}

case class SpMemFold(fMap: Lambda,
                     fReduce: Lambda,
                     iterSize: ArithExpr,
                     stride: Option[ArithExpr] = None,
                     factor: Option[ArithExpr] = None) extends Pattern(arity = 2) {
  assert(fMap.params.length == 1)
  assert(fReduce.params.length == 2)

  val loopVar: Var = PosVar("i")
  val iterationCount: ArithExpr with SimplifiedExpr = loopVar.range.numVals
  var shouldUnroll = false

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case TupleType(initT, at@ArrayType(elemT)) =>
        fMap.params(0).t = ArrayType(elemT, iterSize) // map input array element type

        val mapBodyType = TypeChecker.check(fMap.body, setType) // check the body

        fReduce.params(0).t = initT // initial element type
        fReduce.params(1).t = mapBodyType // reduce input array element type

        val reduceBodyType = TypeChecker.check(fReduce.body, setType) // check the body

        if (initT != mapBodyType || initT != reduceBodyType)
          throw TypeException(
            s"Illegal customising function in:\n``$this``.\n" +
              s"``($initT, $mapBodyType) -> $reduceBodyType`` does not match ``(α, α) -> α``"
          )

        ArrayTypeWSWC(initT, 1)

      case _ => throw new TypeException(argType, "TupleType(_, ArrayType(_, _))", this)
    }
  }

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = {
    fMap.visit_pp(prePost)
    fReduce.visit_pp(prePost)
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    throw new NotImplementedError()
  }
}

object SpMemFold {
  def apply(iterSize: ArithExpr,
            stride: ArithExpr,
            factor: ArithExpr,
            fMap: Lambda, fReduce: Lambda2,
            init: Expr): Lambda1 =
    fun((x) => SpMemFold(fMap, fReduce, iterSize, Some(stride), Some(factor))(init, x))

  def apply(iterSize: ArithExpr,
            stride: Option[ArithExpr] = None,
            factor: Option[ArithExpr] = None,
            fMap: Lambda, fReduce: Lambda2,
            init: Expr): Lambda1 =
    fun((x) => SpMemFold(fMap, fReduce, iterSize, stride, factor)(init, x))
}

