package generators

import test._
import test.ReduceSeq

object OpenCLGenerator {
  
  def generate(f: Fun) : String = {
    assert(f.inT != UndefType)
    assert(f.ouT != UndefType)
    
    val str = f match {
      case cf: CompFun => cf.funs.foldRight("")((inF, str) => str + generate(inF))
      // maps
      case m: MapWrg => MapWgrGenerator.generate(m)
      case m: MapLcl => "MapLcl(" + generate(m.f) + ")"
      // reduce
      case r: ReduceSeq => "ReduceSeq(" + generate(r.f) + ")"
      // user functions
      case NullFun => "userFunc"
      // utilities
      case _: oSplit => ""
      case _: oJoin => ""
      case _ => "__" + f.toString() + "__"
    }
    
    // "kernel void KERNEL(...) {\n" + str + "}\n"
    str
  }

}