package backends.spatial.accel.ir.pattern

import ir.{ArrayType, TupleType, Type, TypeChecker, TypeException}
import ir.ast.{Expr, FPattern, IRNode, Lambda, Lambda1, Lambda2, Pattern, fun}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{ArithExpr, PosVar, Var}

/**
 * Sequential MapAccum pattern.
 *
 * The mapAccum pattern has the following high-level semantics:
 *   <code>MapAccum(f)( x, [y,,1,,, ..., y,,n,,] )
 *           = ( z, [w,,1,,, ..., w,,n,,] )</code>
 *
 * The mapAccum pattern has the following type:
 *  `MapAccum(f) : a -> [b]_n -> (a, [c]_n)`
 * where `f: a -> b -> (a, c)`

 * MapAccum is similar to scan; the difference is that scan produces the same new value for state and intermediate
 * output and returns all outputs, while mapAccum produces one value for state and another for intermediate
 * output and returns both the final state and all outputs.
 *
 * One use case is when we want to acquire the final state. Scan can be applied so that on each state it
 * produces a tuple of two values where one value is used as state and another -- as the output value; however,
 * the result would be all intermediate states and all output. If we are only interested in the last state,
 * that would waste memory.
 *
 * @param f       The lambda for producing the tuple (state, output)
 * @param loopVar The iterator variable
 */
case class MapAccumSeq(f: Lambda, var loopVar: Var) extends Pattern(arity = 2) with FPattern with Sequential {
  override def eval(valueMap: ValueMap, args: Any*): Any = {
    throw new NotImplementedError()
  }
  var shouldUnroll = false

  // MapAccumSeq::(a -> b -> (a, c)) -> a -> [b]_n -> (a, [c]_n)
  override def checkType(argType: Type, setType: Boolean): Type = {
    argType match {
      case TupleType(initT, argAt @ ArrayType(elemT)) =>
        f.params(0).t = initT // initial elem type => a
        f.params(1).t = elemT // array element type => b

        val bodyType = TypeChecker.check(f.body, setType)

        bodyType match {
          case TupleType(stateType, outType) =>
            if (stateType != initT)
              throw TypeException(s"MapAccumSeq operator returns $stateType for state instead of the expected $initT")

            TupleType(stateType, argAt.replacedElemT(outType)) // generate (a, [c]_n) from [b]_n

          case _ => throw new TypeException(bodyType, "TupleType(_, _)", f)
        }

      case _ => throw new TypeException(argType, "TupleType(_, ArrayType(_, _))", this)
    }
  }

  def iterationCount: ArithExpr = loopVar.range.numVals

  override def copy(f: Lambda): MapAccumSeq = MapAccumSeq(f, PosVar("i"))

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)
}

object MapAccumSeq {
  def apply(f: Lambda2, init: Expr): Lambda1 = fun(x => MapAccumSeq(f, PosVar("i"))(init, x))
}