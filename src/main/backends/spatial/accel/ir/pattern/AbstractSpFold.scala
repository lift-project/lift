package backends.spatial.accel.ir.pattern

import ir.ast.{IRNode, Lambda, Pattern}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{ArithExpr, SimplifiedExpr, Var}

abstract class AbstractSpFold(val fMap: Lambda,
                              val fReduce: Lambda,
                              val mapLoopVar: Var,
                              val reduceLoopVar: Var,
                              val iterSize: ArithExpr,
                              val stride: ArithExpr,
                              val factor: ArithExpr) extends Pattern(arity = 2) {
  assert(fMap.params.length == 1)
  assert(fReduce.params.length == 2)

  val iterationCount: ArithExpr with SimplifiedExpr = mapLoopVar.range.numVals // == reduceLoopVar.range.numVals
  var shouldUnroll = false

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = {
    fMap.visit_pp(prePost)
    fReduce.visit_pp(prePost)
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    throw new NotImplementedError()
  }
}
