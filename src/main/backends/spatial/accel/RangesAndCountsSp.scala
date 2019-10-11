package backends.spatial.accel

import _root_.ir.ast._
import _root_.ir.Type
import backends.spatial.accel.ir.pattern.{AbstractSpFold, MapSeq, SpForeach}
import lift.arithmetic.{ArithExpr, ContinuousRange, Cst, Var}

object RangesAndCountsSp {
  /**
   * Add ranges to the iteration variables of the calls to loop-based patterns such as Map and Reduce and if
   * possible, determine the number of iterations of the corresponding loops.
   *
   * @param lambda Input lambda.
   * @param valueMap The map from variables to lengths.
   */
  def apply(lambda: Lambda, valueMap: scala.collection.Map[ArithExpr, ArithExpr]): Unit = {
    new RangesAndCountsSp(valueMap)(lambda.body)
  }
}

private class RangesAndCountsSp(valueMap: scala.collection.Map[ArithExpr, ArithExpr]) {
  private def apply(expr: Expr): Unit = {
    expr match {
      case call: FunCall =>
        call.args.foreach(apply)
        call.f match {
          case sf: SpForeach          => setRangeSpForeach(sf, call)
          case m: MapSeq              => setRangeMapSeq(m, call)
          case asf: AbstractSpFold    => setRangeAbstrSpFold(asf, call)

          case f: FPattern            => apply(f.f.body)
          case l: Lambda              => apply(l.body)
          case Zip(_) | Tuple(_)      => call.args.foreach(apply)
          case _ =>
        }
      case _ =>
    }
  }

  private def setRangeSpForeach(sf: SpForeach, call: FunCall): Unit = {
    sf.loopVar = Var(sf.loopVar.name, ContinuousRange(Cst(0), Type.getLength(call.args.head.t)))
    apply(sf.f.body)
  }

  private def setRangeMapSeq(m: MapSeq, call: FunCall): Unit = {
    m.loopVar = Var(m.loopVar.name, ContinuousRange(Cst(0), Type.getLength(call.args.head.t)))
    apply(m.f.body)
  }

  private def setRangeAbstrSpFold(asf: AbstractSpFold, call: FunCall): Unit = {
    asf.mapLoopVar = Var(asf.mapLoopVar.name, ContinuousRange(Cst(0), Type.getLength(call.args(1).t)))
    apply(asf.fMap.body)

    asf.reduceLoopVar = Var(asf.reduceLoopVar.name, ContinuousRange(Cst(0), Type.getLength(call.args(1).t)))
    apply(asf.fReduce.body)
  }
}