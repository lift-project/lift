package generators

import test._

object MapGenerator {
  
  def generate(m: AbstractMap, f: Fun, indexVar: String, init: String, update: String) : String = {
	val (elemT, len) = m.inT.extractArrayType()
	val cond = len
	
	// multiply all the sizes with the indexVariable ...
    val expr = elemT.sizes().foldLeft[Expr](Var(indexVar))( _ * _ )
    OpenCLGenerator.pushAccessFunction((index: String) => { expr + " + " + index })
    
    val body = OpenCLGenerator.generate(f) + "\n"
    
    OpenCLGenerator.popAccessFunction()
    
    LoopGenerator.generate(indexVar, init, cond, update, body)
  }
	
}

object MapWgrGenerator {
  
  def generate(m: MapWrg) : String = {
    val indexVar = "g_id"
    val init = "get_group_id(0)"
    val update = "get_num_groups(0)" // macro!?
      
    MapGenerator.generate(m, m.f, indexVar, init, update) +
    "return;\n"
  }

}

object MapLclGenerator {
  
  def generate(m: MapLcl) : String = {   
    val indexVar = "l_id"
    val init = "get_local_id(0)"
    val update = "get_local_size(0)" // macro!?
      
    MapGenerator.generate(m, m.f, indexVar, init, update) +
    generateBarrier
  }
  
  def generateBarrier() : String = {
     // TODO: decide between local and global memory based on type information
    "barrier(CLK_LOCAL_MEM_FENCE && CLK_GLOBAL_MEM_FENCE)"
  }
  
}


/*
  // necessary???
  def ifndefDefine(name: String, value: String) : String = {
    "#ifndef " + name + "\n" +
    "#define " + name + " (" + value + ")\n" +
    "#endif\n"
  }

  // necessary???
  def undef(name: String) : String = {
    "#undef " + name + "\n"
  }
*/