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
      case m: MapLcl => MapLclGenerator.generate(m)
      // reduce
      case r: ReduceSeq => ReduceSeqGenerator.generate(r)
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
  
  type AccessFunction = (String) => String
  
  private var accessFunctions = new scala.collection.mutable.Stack[(AccessFunction)]
  
  def foldNewToOld(z: String)(op: (String, AccessFunction) => String) : String = {
    accessFunctions.foldLeft(z)(op)
  }
  
  def foldOldToNew(z: String)(op: (String, AccessFunction) => String) : String = {
    accessFunctions.foldRight(z)( (accessFunc, str) => op(str, accessFunc))
  }
  
  def pushAccessFunction(func: AccessFunction): Unit = {
    accessFunctions.push(func)
  }
  
  def popAccessFunction(): Unit = {
    accessFunctions.pop()
  }

}