package generators

import ir._

object MapGenerator {
  
  def generate(m: AbstractMap, f: Fun, indexVar: Expr, range: RangeAdd, accessFunctions: Array[generators.AccessFunction]) : String = {
    val elemT = Type.getElemT(m.inT)
    
	// multiply all lengths with the indexVariable ...
    val expr = Type.length(elemT).foldLeft(indexVar)( _ * _ )
    val accessFun = (index: Expr) => { expr + index }
    
    val body = OpenCLGenerator.generate(f, accessFunctions :+ accessFun) + "\n"
    
    LoopGenerator.generate(indexVar, range, body)
  }
	
}

object MapWgrGenerator {
  
  def generate(m: MapWrg, accessFunctions: Array[generators.AccessFunction]) : String = {
    val len = Type.getLength(m.inT)
    val range = RangeAdd(Var("get_group_id(0)"), len, Var("get_num_groups(0)"))
    val indexVar = Var("g_id") // range
      
    MapGenerator.generate(m, m.f, indexVar, range, accessFunctions) +
    "return;\n"
  }

}

object MapLclGenerator {
  
  def generate(m: MapLcl, accessFunctions: Array[generators.AccessFunction]) : String = {
    val len = Type.getLength(m.inT)
    val range = RangeAdd(Var("get_local_id(0)"), len, Var("get_local_size(0)"))
    val indexVar = Var("l_id") // range
      
    MapGenerator.generate(m, m.f, indexVar, range, accessFunctions) +
    generateBarrier
  }
  
  def generateBarrier() : String = {
     // TODO: decide between local and global memory based on type information
    "barrier(CLK_LOCAL_MEM_FENCE && CLK_GLOBAL_MEM_FENCE)"
  }
  
}
