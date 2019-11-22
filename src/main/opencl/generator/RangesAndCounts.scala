package opencl.generator

import lift.arithmetic._
import ir._
import ir.ast._
import opencl.ir.pattern._

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
  def apply(lambda: Lambda, localSizes: NDRange, globalSizes: NDRange,
            valueMap: scala.collection.Map[ArithExpr, ArithExpr]): Unit = {
    new RangesAndCounts(localSizes, globalSizes, valueMap)(lambda.body)
  }
}

private class RangesAndCounts(localSizes: NDRange, globalSizes: NDRange,
                              valueMap: scala.collection.Map[ArithExpr, ArithExpr]) {
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
              case m: MapAtomLcl => setRangeMapAtomLcl(m, call)
              case m: MapAtomWrg => setRangeMapAtomWrg(m, call)
              case m: MapWarp => setRangeMapWarp(m, call)
              case m: MapLane => setRangeMapLane(m, call)
              case m: MapSeq => setRangeMapSeq(m, call)
              case _ =>
            }
            m match {
              case _: Map =>
              case _ => apply(m.f.body)
            }

          case f: FilterSeq =>
            setRangeFilterSeq(f, call)
            apply(f.f.body)

          case iss: InsertionSortSeq =>
            setRangeInsertionSort(iss, call)
            apply(iss.f.body)

          case scan: ScanSeq => {
            setRangeScanSeq(scan, call)
            apply(scan.f.body)
          }

          case r: AbstractPartRed =>
            r match {
              case r : ReduceSeq => setRangeReduceSeq(r, call)
              case r : ReduceWhileSeq => setRangeReduceSeq(r, call) 
            }
            apply(r.f.body)
          case sp: MapSeqSlide => setRangeMapSeqSlide(sp, call)
            apply(sp.f.body)

          case i: Iterate =>
            setRangeIterate(i)
            evaluateIterateRange(i)
            apply(i.f.body)


          case f: FPattern => apply(f.f.body)
          case l: Lambda => apply(l.body)
          case _ =>
        }
      case _ =>
    }
  }

  private def setRangeMapWrg(m: MapWrg, call: FunCall): Unit = {
    val dim: Int = m.dim

    var start =  get_group_id(dim)
    val stop: ArithExpr = ArithExpr.substitute(Type.getLength(call.args.head.t), valueMap)
    var step : ArithExpr = get_num_groups(m.dim)

    val gSize = globalSizes(dim)
    val lSize = localSizes(dim)

    gSize match {
      case x if x.getClass == ?.getClass =>
      case c =>
        val numGroups = gSize /^ lSize
        start = get_group_id(dim,ContinuousRange(0, numGroups))
        step = numGroups
    }

    m.loopVar = Var(m.loopVar.name, RangeAdd(start, stop, step))
  }

  private def setRangeMapGlb(m: MapGlb, call: FunCall): Unit = {
    val dim = m.dim

    var start : ArithExpr = get_global_id(dim)
    var length = Type.getLength(call.args.head.t)
    var step: ArithExpr = get_global_size(dim)

    val size = globalSizes(dim)
    if (size != ?) {
      step = size
      length = ArithExpr.substitute(length, valueMap)
      start = get_global_id(dim,ContinuousRange(0, size))
    }

    m.loopVar = Var(m.loopVar.name, RangeAdd(start, length, step))
  }

  private def setRangeMapLcl(m: MapLcl, call: FunCall): Unit = {
    val dim: Int = m.dim
    var start = get_local_id(dim)
    val length = Type.getLength(call.args.head.t)
    var step: ArithExpr = get_local_size(dim)

    val size = localSizes(dim)
    if (size != ?) {
      step = size
      start = get_local_id(dim,ContinuousRange(0, size))
    }

    m.loopVar = Var(m.loopVar.name,RangeAdd(start, length, step))

  }

  private def setRangeMapAtomLcl(m: MapAtomLcl, call: FunCall) : Unit = {
    // note, this implementation of set range is purposefully simple
    // we assume that the actual look is _not_ implemented as a simple
    // loop, but using a separate "task pointer", atomically incremented
    // and a while loop to repeat the incrementation until the range is reached
    // under this model, the range/start/end/step of the loop variable
    // is correct, even though it would not be possible to generate a 
    // "normal" loop from it.
    val start = Cst(0)
    val length = Type.getLength(call.args.head.t)
    val step: ArithExpr = Cst(1)

    m.loopVar = Var(m.loopVar.name, RangeAdd(start, length, step))
  }

  private def setRangeMapAtomWrg(m: MapAtomWrg, call: FunCall) : Unit = {
    // note, this implementation of set range is purposefully simple
    // we assume that the actual look is _not_ implemented as a simple
    // loop, but using a separate "task pointer", atomically incremented
    // and a while loop to repeat the incrementation until the range is reached
    // under this model, the range/start/end/step of the loop variable
    // is correct, even though it would not be possible to generate a 
    // "normal" loop from it.
    val start = Cst(0)
    val length = Type.getLength(call.args.head.t)
    val step: ArithExpr = Cst(1)

    m.loopVar = Var(m.loopVar.name, RangeAdd(start, length, step))
  }


  private def setRangeMapWarp(m: MapWarp, call: FunCall): Unit = {
    m.loopVar = Var(m.loopVar.name, range = RangeAdd(get_local_id(0) /^ OpenCL.warpSize,
      Type.getLength(call.args.head.t),
      localSizes(0) /^ OpenCL.warpSize))
  }

  private def setRangeMapLane(m: MapLane, call: FunCall): Unit = {
    m.loopVar = Var(m.loopVar.name, RangeAdd(get_local_id(0) % OpenCL.warpSize,
      Type.getLength(call.args.head.t), OpenCL.warpSize))
  }

  private def setRangeMapSeq(m: MapSeq, call: FunCall): Unit = {
    m.loopVar = Var(m.loopVar.name, ContinuousRange(Cst(0), Type.getLength(call.args.head.t)))
  }
  
  private def setRangeInsertionSort(iss: InsertionSortSeq, call: FunCall): Unit = {
    iss.loopRead = Var(
      iss.loopRead.name,
      ContinuousRange(Cst(0), Type.getLength(call.args.head.t))
    )
    iss.loopWrite = Var(
      iss.loopWrite.name,
      ContinuousRange(Cst(0), Type.getLength(call.args.head.t))
    )
  }

  private def setRangeFilterSeq(f: FilterSeq, call: FunCall): Unit = {
    f.loopRead = Var(f.loopRead.name, ContinuousRange(Cst(0), Type.getLength(call.args.head.t)))
    f.loopWrite = Var(f.loopWrite.name, ContinuousRange(Cst(0), Type.getLength(call.args.head.t)))
  }
  
  private def setRangeReduceSeq(r: AbstractReduce, call: FunCall): Unit = {
    val inT = call.args(1).t
    r.loopVar = Var(r.loopVar.name, RangeAdd(Cst(0), Type.getLength(inT), Cst(1)))
  }

  private def setRangeMapSeqSlide(sp: MapSeqSlide, call: FunCall): Unit = {
    val reuse = sp.size - sp.step
    sp.loopVar = Var(sp.loopVar.name, ContinuousRange(Cst(0), (Type.getLength(call.args.head.t) - reuse) / sp.step))
  }


  private def setRangeIterate(i: Iterate): Unit = {
    i.indexVar = Var(i.indexVar.name,range = ContinuousRange(Cst(0), i.n))
  }

  private def evaluateIterateRange(i: Iterate): Unit = {
    i.iterationCount =
      evaluateRangeForCount(i.indexVar.range.asInstanceOf[RangeAdd])
  }

  private def setRangeScanSeq(scan:ScanSeq, call: FunCall): Unit = {
    scan.loopVar = Var(scan.loopVar.name, ContinuousRange(Cst(0), Type.getLength(call.args(1).t)))
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
