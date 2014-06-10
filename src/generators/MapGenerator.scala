package generators

import test._

object MapGenerator {
  
  def generate(m: AbstractMap, f: Fun, indexVar: Expr, init: Expr, update: Expr, accessFunctions: Array[generators.AccessFunction]) : String = {
	val (elemT, len) = m.inT.extractArrayType()
	val cond = len
	
	// multiply all lengths with the indexVariable ...
    val expr = elemT.length().foldLeft(indexVar)( _ * _ )
    val accessFun = (index: Expr) => { expr + index }
    
    val body = OpenCLGenerator.generate(f, accessFunctions :+ accessFun) + "\n"
    
    LoopGenerator.generate(indexVar, init, cond, update, body)
  }
	
}

object MapWgrGenerator {
  
  def generate(m: MapWrg, accessFunctions: Array[generators.AccessFunction]) : String = {
    val indexVar = Var("g_id")
    val init = Var("get_group_id(0)")
    val update = Var("get_num_groups(0)") // macro!?
      
    MapGenerator.generate(m, m.f, indexVar, init, update, accessFunctions) +
    "return;\n"
  }

}

object MapLclGenerator {
  
  def generate(m: MapLcl, accessFunctions: Array[generators.AccessFunction]) : String = {   
    val indexVar = Var("l_id")
    val init = Var("get_local_id(0)")
    val update = Var("get_local_size(0)") // macro!?
      
    MapGenerator.generate(m, m.f, indexVar, init, update, accessFunctions) +
    generateBarrier
  }
  
  def generateBarrier() : String = {
     // TODO: decide between local and global memory based on type information
    "barrier(CLK_LOCAL_MEM_FENCE && CLK_GLOBAL_MEM_FENCE)"
  }
  
}
