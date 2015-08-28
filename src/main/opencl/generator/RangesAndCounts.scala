package opencl.generator

import apart.arithmetic._
import ir._
import ir.ast._
import opencl.generator.OpenCLGenerator.Kernel
import opencl.ir.pattern._

import scala.collection.immutable

object RangesAndCounts {
  /**
   * Add ranges to the iteration variables of Map, Reduce and Iterate calls and if
   * possible, determine the number of iterations of the corresponding loops.
   *
   * @param lambda Input lambda.
   * @param localSizes Array containing the local sizes of the ND-Range.
   * @param globalSizes Array containing the global sizes of the ND-Range.
   * @param valueMap The map from variables to lengths.
   */
  def apply(lambda: Lambda, localSizes: Array[ArithExpr], globalSizes: Array[ArithExpr],
            valueMap: immutable.Map[ArithExpr, ArithExpr]): Unit = {
    new RangesAndCounts(localSizes, globalSizes, valueMap)(lambda.body)
  }
}

private class RangesAndCounts(localSizes: Array[ArithExpr], globalSizes: Array[ArithExpr],
                              valueMap: immutable.Map[ArithExpr, ArithExpr]) {
  private def apply(expr: Expr): Unit = {
    expr match {
      case call: FunCall =>
        call.args.foreach(apply)
        call.f match {
          case m: AbstractMap =>
            m match {
              case m: MapWrg => setRangeMapWrg(m, call)
              case m: MapGlb => setRangeMapGlb(m, call)
              case m: MapLcl => setRangeMapLcl(m, call)
              case m: MapWarp => setRangeMapWarp(m, call)
              case m: MapLane => setRangeMapLane(m, call)
              case m: MapSeq => setRangeMapSeq(m, call)
              case _ =>
            }
            m match {
              case _: Map =>
              case _ => apply(m.f.body)
            }

          case r: AbstractPartRed =>
            r match {
              case r: ReduceSeq => setRangeReduceSeq(r, call)
            }
            evaluateReduceRange(r)
            apply(r.f.body)

          case i: Iterate =>
            setRangeIterate(i)
            evaluateIterateRange(i)
            apply(i.f.body)

          case f: FPattern => apply(f.f.body)
          case l: Lambda => apply(l.body)
          case Zip(_) | Tuple(_) => call.args.foreach(apply)
          case _ =>
        }
      case _ =>
    }
  }

  private def setRangeMapWrg(m: MapWrg, call: FunCall): Unit = {
    val dim: Int = m.dim
    val start: get_group_id = new get_group_id(dim)
    val length: ArithExpr = Type.getLength(call.args.head.t)
    val step: ArithExpr = new get_num_groups(m.dim)

    val gSize = globalSizes(dim)
    val lSize = localSizes(dim)

    gSize match {
      case Cst(c) =>
        val numGroups = gSize /^ lSize
        val lengthSubst = ArithExpr.substitute(length, valueMap)
        start.range = ContinuousRange(0, numGroups)
        m.loopVar.range = RangeAdd(start, lengthSubst, numGroups)
        evaluateMapRange(m)
      case x if x.getClass == ?.getClass =>
      case x => throw new IllegalArgumentException(s"Invalid global size type: $x (${x.getClass})")
    }

    m.loopVar.range = RangeAdd(start, length, step)
  }

  private def setRangeMapGlb(m: MapGlb, call: FunCall): Unit = {
    m.loopVar.range = RangeAdd(new get_global_id(m.dim),
      Type.getLength(call.args.head.t),
      new get_global_size(m.dim))
    evaluateMapRange(m)
  }

  private def setRangeMapLcl(m: MapLcl, call: FunCall): Unit = {
    val dim: Int = m.dim
    val start = new get_local_id(dim)
    val length = Type.getLength(call.args.head.t)
    var step: ArithExpr = new get_local_size(dim)

    val size = localSizes(dim)
    if (size != ?) {
      step = size
      start.range = ContinuousRange(0, size)
    }

    m.loopVar.range = RangeAdd(start, length, step)
    evaluateMapRange(m)
  }

  private def setRangeMapWarp(m: MapWarp, call: FunCall): Unit = {
    m.loopVar.range = RangeAdd(new get_local_id(0) /^ OpenCL.warpSize,
      Type.getLength(call.args.head.t),
      Cst(Kernel.workGroupSize) /^ OpenCL.warpSize)
    evaluateMapRange(m)
  }

  private def setRangeMapLane(m: MapLane, call: FunCall): Unit = {
    m.loopVar.range = RangeAdd(new get_local_id(0) % OpenCL.warpSize,
      Type.getLength(call.args.head.t), OpenCL.warpSize)
    evaluateMapRange(m)
  }

  private def setRangeMapSeq(m: MapSeq, call: FunCall): Unit = {
    m.loopVar.range = ContinuousRange(Cst(0), Type.getLength(call.args.head.t))
    evaluateMapRange(m)
  }

  private def setRangeReduceSeq(r: AbstractReduce, call: FunCall): Unit = {
    val inT = call.args(1).t
    r.loopVar.range = RangeAdd(Cst(0), Type.getLength(inT), Cst(1))
  }

  private def setRangeIterate(i: Iterate): Unit = {
    i.indexVar.range = ContinuousRange(Cst(0), i.n)
  }

  private def evaluateMapRange(m: AbstractMap): Unit = {
    m.iterationCount =
      evaluateRangeForCount(m.loopVar.range.asInstanceOf[RangeAdd])
  }

  private def evaluateReduceRange(r: AbstractPartRed): Unit = {
    r.iterationCount =
      evaluateRangeForCount(r.loopVar.range.asInstanceOf[RangeAdd])
  }

  private def evaluateIterateRange(i: Iterate): Unit = {
    i.iterationCount =
      evaluateRangeForCount(i.indexVar.range.asInstanceOf[RangeAdd])
  }

  private def evaluateRangeForCount(range: RangeAdd): ArithExpr = {
    val init = range.start
    val cond = range.stop
    val update = range.step

    // eval expression. if successful return true and the value, otherwise return false
    def evalExpr = (e: ArithExpr) => {try { (true, e.atMax.eval)} catch { case _ : Throwable => (false, 0) } }
    def evalExprMinMax = (e: ArithExpr) => {try { (true, e.atMin.eval, e.atMax.eval)} catch { case _ : Throwable => (false, 0, 0) } }

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
