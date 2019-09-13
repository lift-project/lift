package backends.c.sdh.loop_var_inference

import backends.c.sdh.sdh_ir.{MapGPE, TMKernel}
import ir.Type
import ir.ast.{AbstractMap, AbstractReduce, FunCall, IRNode, Lambda}
import lift.arithmetic.{ContinuousRange, Cst, Var}


object LoopVarInference  extends Function1[Lambda, Lambda] {
  def apply(lambda : Lambda) : Lambda = {

    println("4. sdh infer loop var")

    lambda visitBy {
      case fc@FunCall(m: AbstractMap, _) =>
        if(m.isInstanceOf[MapGPE]){
          assert(m.f.body.isInstanceOf[FunCall])
          assert(m.f.body.asInstanceOf[FunCall].f.isInstanceOf[TMKernel])
          m.f.body.asInstanceOf[FunCall].f.asInstanceOf[TMKernel].loopVar = m.loopVar
        }
      case _ => ()
    }

    lambda
  }
}

