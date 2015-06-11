package opencl.generator

import arithmetic._
import ir._
import opencl.generator.OpenCLGenerator.Kernel
import opencl.ir._

import scala.collection.immutable

object RangesAndCounts {
  def apply(f: Lambda, localSizes: Array[ArithExpr], globalSizes: Array[ArithExpr],
            valueMap: immutable.Map[ArithExpr, ArithExpr]): Unit = {
    new RangesAndCounts(localSizes, globalSizes, valueMap)(f.body)
  }
}

class RangesAndCounts(localSizes: Array[ArithExpr], globalSizes: Array[ArithExpr],
                      valueMap: immutable.Map[ArithExpr, ArithExpr]) {
  def apply(expr: Expr): Unit = {
    expr match {
      case call: MapCall =>
        call.f match {
          case _: MapWrg => setRangeMapWrg(call)
          case _: MapGlb => setRangeMapGlb(call)
          case _: MapLcl => setRangeMapLcl(call)
          case _: MapWarp => setRangeMapWarp(call)
          case _: MapLane => setRangeMapLane(call)
          case _: MapSeq => setRangeMapSeq(call)
          case _ =>
        }
        call.f match {
          case _: Map =>
          case _ =>
            apply(call.f.f.body)
        }
      case call: ReduceCall =>
        call.f match {
          case _: ReduceSeq => setRangeReduceSeq(call)
          case _: ReduceHost => setRangeReduceSeq(call)
        }
        evaluateReduceRange(call)
        apply(call.arg0)
        apply(call.f.f.body)
      case call: IterateCall =>
        setRangeIterate(call)
        evaluateIterateRange(call)
        apply(call.f.f.body)
      case call: FunCall => call.f match {
        case cf: CompFunDef => cf.funs.foreach( (l:Lambda) => apply(l.body) )
        case f: FPattern => apply(f.f.body)
        case l: Lambda => apply(l.body)
        case Zip(_) | Tuple(_) => call.args.foreach(apply)
        case _ =>
      }
      case _ =>
    }
  }

  private def setRangeMapWrg(call: MapCall): Unit = {
    val m = call.f.asInstanceOf[MapWrg]
    val dim: Int = m.dim
    val start: get_group_id = new get_group_id(dim)
    val length: ArithExpr = Type.getLength(call.arg.t)
    val step: ArithExpr = new get_num_groups(m.dim)

    val gSize = globalSizes(dim)
    val lSize = localSizes(dim)

    gSize match {
      case Cst(c) =>
        val numGroups = gSize /^ lSize
        val lengthSubst = ArithExpr.substitute(length, valueMap)
        start.range = ContinuousRange(0, numGroups)
        call.loopVar.range = RangeAdd(start, lengthSubst, numGroups)
        evaluateMapRange(call)
      case ? =>
      case _ => throw new IllegalArgumentException("Invalid global size type")
    }

    call.loopVar.range = RangeAdd(start, length, step)
  }

  private def setRangeMapGlb(call: MapCall): Unit = {
    val m = call.f.asInstanceOf[MapGlb]
    call.loopVar.range = RangeAdd(new get_global_id(m.dim), Type.getLength(call.arg.t), new get_global_size(m.dim))
    evaluateMapRange(call)
  }

  private def setRangeMapLcl(call: MapCall): Unit = {
    val m = call.f.asInstanceOf[MapLcl]
    val dim: Int = m.dim
    val start = new get_local_id(dim)
    val length = Type.getLength(call.arg.t)
    var step: ArithExpr = new get_local_size(dim)

    val size = localSizes(dim)
    if (size != ?) {
      step = size
      start.range = ContinuousRange(0, size)
    }

    call.loopVar.range = RangeAdd(start, length, step)
    evaluateMapRange(call)
  }

  private def setRangeMapWarp(call: MapCall): Unit = {
    call.loopVar.range = RangeAdd(new get_local_id(0) /^ OpenCL.warpSize,
      Type.getLength(call.arg.t),
      Cst(Kernel.workGroupSize) /^ OpenCL.warpSize)
    evaluateMapRange(call)
  }

  private def setRangeMapLane(call: MapCall): Unit = {
    call.loopVar.range = RangeAdd(new get_local_id(0) & (OpenCL.warpSize - Cst(1)), Type.getLength(call.arg.t), OpenCL.warpSize)
    evaluateMapRange(call)
  }

  private def setRangeMapSeq(call: MapCall): Unit = {
    call.loopVar.range = ContinuousRange(Cst(0), Type.getLength(call.arg.t))
    evaluateMapRange(call)
  }

  private def setRangeReduceSeq(call: ReduceCall): Unit = {
    val inT = call.arg1.t
    call.loopVar.range = RangeAdd(Cst(0), Type.getLength(inT), Cst(1))
  }

  private def setRangeIterate(call: IterateCall): Unit = {
    call.indexVar.range = ContinuousRange(Cst(0), call.f.n)
  }

  private def evaluateMapRange(call: MapCall): Unit = {
    call.iterationCount = evaluateRangeForCount(call.loopVar.range.asInstanceOf[RangeAdd])
  }

  private def evaluateReduceRange(call: ReduceCall): Unit = {
    call.iterationCount = evaluateRangeForCount(call.loopVar.range.asInstanceOf[RangeAdd])
  }

  private def evaluateIterateRange(call: IterateCall): Unit = {
    call.iterationCount = evaluateRangeForCount(call.indexVar.range.asInstanceOf[RangeAdd])
  }

  private def evaluateRangeForCount(range: RangeAdd): ArithExpr = {
    val init = ExprSimplifier(range.start)
    val cond = ExprSimplifier(range.stop)
    val update = ExprSimplifier(range.step)

    // eval expression. if successful return true and the value, otherwise return false
    def evalExpr = (e: ArithExpr) => {try { (true, e.evalAtMax())} catch { case _ : Throwable => (false, 0) } }
    def evalExprMinMax = (e: ArithExpr) => {try { (true, e.evalAtMin(), e.evalAtMax())} catch { case _ : Throwable => (false, 0, 0) } }

    // try to directly evaluate
    val (initIsEvaluated, initMinEvaluated, initMaxEvaluated) = evalExprMinMax(init)
    val (condIsEvaluated, condEvaluated) = evalExpr(cond)
    val (updateIsEvaluated, updateEvaluated) = evalExpr(update)

    if (initIsEvaluated && condIsEvaluated) {
      if (condEvaluated <= initMinEvaluated) {
        // nothing to do
        return 0
      }
    }

    if (initIsEvaluated && condIsEvaluated && updateIsEvaluated) {
      assert (condEvaluated > initMinEvaluated)
      if (initMinEvaluated == initMaxEvaluated) // Sequential loop
        return ((condEvaluated - initMinEvaluated).toDouble / updateEvaluated).ceil.toInt

      else if (initMaxEvaluated - initMinEvaluated == updateEvaluated) // Parallel loop

        if ((condEvaluated - initMinEvaluated) % updateEvaluated == 0)
          return ((condEvaluated - initMinEvaluated).toDouble / updateEvaluated).ceil.toInt

    }

    if (condIsEvaluated && updateIsEvaluated)
      if (condEvaluated <= updateEvaluated) {
        // one or less iteration
        return Cst(1) / ?
      }

    ?
  }
}
