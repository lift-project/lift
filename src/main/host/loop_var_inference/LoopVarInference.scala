package host.loop_var_inference

import ir.Type
import ir.ast.{AbstractMap, AbstractReduce, FunCall, IRNode, Lambda}
import lift.arithmetic.{ContinuousRange, Cst, Var}


object LoopVarInference {
  def apply(lambda : Lambda): Unit = {
    //new RangeAndCountsHost(lambda).execute()
    lambda visitBy {
      case fc@FunCall(m: AbstractMap, _) =>
        m.loopVar = Var(m.loopVar.name, ContinuousRange(Cst(0), Type.getLength(fc.args.head.t)))
      case fc@FunCall(r: AbstractReduce, _*) =>
        //second argument is the array, the first one is init value, so use second arg for length information
        r.loopVar = Var(r.loopVar.name, ContinuousRange(Cst(0), Type.getLength(fc.args(1).t)))
      case _ => ()
    }
  }
}

/*
class RangeAndCountsHost(lambda: Lambda) {
  def execute(): Unit = {
    lambda.visit(range_and_count_transfomation)
  }

  val range_and_count_transfomation = {
    node: IRNode =>
      node match {
        case fc@FunCall(m: AbstractMap, _) =>
          m.loopVar = Var(m.loopVar.name, ContinuousRange(Cst(0), Type.getLength(fc.args.head.t)))
          /* if(m.isInstanceOf[MapGPE]){
            assert(m.f.body.isInstanceOf[FunCall])
            assert(m.f.body.asInstanceOf[FunCall].f.isInstanceOf[TMKernel])
            m.f.body.asInstanceOf[FunCall].f.asInstanceOf[TMKernel].loopVar = m.loopVar
          } */
        case fc@FunCall(r: AbstractReduce, _*) =>
          r.loopVar = Var(r.loopVar.name, ContinuousRange(Cst(0), Type.getLength(fc.args.head.t)))
        /*case fc@FunCall(s: ScanHost, _*) =>
          s.loopVar = Var(s.loopVar.name, ContinuousRange(Cst(1), Type.getLength(fc.args(1).t)))
        case fc@FunCall(s: MapScanHost, _*) =>
          s.loopVar = Var(s.loopVar.name, ContinuousRange(Cst(1), Type.getLength(fc.args(1).t))) */
        case _ => ()
      }
  }
}
*/
