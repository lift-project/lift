package backends.spatial.accel.ir.pattern

import ir.ast.{IRNode, Lambda1, Lambda2, Pattern}
import ir.interpreter.Interpreter.ValueMap
import ir.{Memory, Type, UnallocatedMemory, UndefType}
import lift.arithmetic.{ArithExpr, SimplifiedExpr, Var}

abstract class AbstractSpFold(val fMap: Lambda1,
                              val fReduce: Lambda2,
                              var mapLoopVar: Var,
                              var reduceLoopVar: Var,
                              val chunkSize: ArithExpr,
                              val stride: ArithExpr,
                              val factor: ArithExpr) extends Pattern(arity = 2) {

  // Since this is a macro pattern (Reduce and Map), the node needs to have more
  // information than usual nodes, such as fMapT below.
  var fFlatMapT: Type = UndefType // The type of the Map lambda
  var fMapMem: Memory = UnallocatedMemory // The implicit memory of the Map

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
