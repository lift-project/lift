package backends.spatial.accel.ir.pattern

import ir.{ArrayType, ArrayTypeWSWC, ScalarType, TupleType, Type, TypeChecker, TypeException}
import ir.ast.{Expr, IRNode, Lambda, Lambda1, Lambda2, Pattern, fun}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{ArithExpr, SimplifiedExpr, Var}

abstract class AbstractSpFold(val fMap: Lambda,
                              val fReduce: Lambda,
                              val loopVar: Var,
                              val iterSize: ArithExpr,
                              val stride: Option[ArithExpr],
                              val factor: Option[ArithExpr]) extends Pattern(arity = 2) {
  assert(fMap.params.length == 1)
  assert(fReduce.params.length == 2)

  val iterationCount: ArithExpr with SimplifiedExpr = loopVar.range.numVals
  var shouldUnroll = false

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = {
    fMap.visit_pp(prePost)
    fReduce.visit_pp(prePost)
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    throw new NotImplementedError()
  }
}
