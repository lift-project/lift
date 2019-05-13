package cbackends.common.loop_var_inference

import ir.Type
import ir.ast.{AbstractMap, AbstractReduce, FunCall, IRNode, Iterate, Lambda}
import lift.arithmetic.{ContinuousRange, Cst, Var}
import opencl.ir.pattern.{MapGlb, ScanSeq}


object LoopVarInference extends Function1[Lambda, Lambda] {
  def apply(lambda : Lambda): Lambda = {

    println("4. common infer loop var")

    //new RangeAndCountsHost(lambda).execute()
    lambda visitBy {
      case fc@FunCall(m: AbstractMap, _) if !m.isInstanceOf[MapGlb] =>
        m.loopVar = Var(m.loopVar.name, ContinuousRange(Cst(0), Type.getLength(fc.args.head.t)))
      case fc@FunCall(r: AbstractReduce, _*) =>
        //second argument is the array, the first one is init value, so use second arg for length information
        r.loopVar = Var(r.loopVar.name, ContinuousRange(Cst(0), Type.getLength(fc.args(1).t)))
      case fc@FunCall(s: ScanSeq, _*) =>
        s.loopVar = Var(s.loopVar.name, ContinuousRange(Cst(0), Type.getLength(fc.args(1).t)))
      case fc@FunCall(i: Iterate, _*) =>
        i.loopVar = Var(i.loopVar.name, ContinuousRange(Cst(0), i.n))
      case _ =>
        ()
    }

    lambda
  }
}

