package generators

import test._
import test.ReduceSeq

package object generators {
  type AccessFunction = (Expr) => Expr
}

object OpenCLGenerator {
  
  type AccessFunction = generators.AccessFunction
  
  def generateKernel(f: Fun) : String = {
    generate(f, Array.empty[AccessFunction])
  }
  
  def generate(f: Fun, accessFunctions: Array[AccessFunction]) : String = {
    assert(f.inT != UndefType)
    assert(f.ouT != UndefType)
    
    f match {
      case cf: CompFun => cf.funs.foldRight("")((inF, str) => str + generate(inF, accessFunctions))
      // maps
      case m: MapWrg => MapWgrGenerator.generate(m, accessFunctions)
      case m: MapLcl => MapLclGenerator.generate(m, accessFunctions)
      // reduce
      case r: ReduceSeq => ReduceSeqGenerator.generate(r, accessFunctions)
      // user functions
      case NullFun => "userFunc"
      // utilities
      case _: oSplit => ""
      case _: oJoin => ""
      case _ => "__" + f.toString() + "__"
    }
  }

}