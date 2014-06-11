package generators

import ir._

object OpenCLGenerator {
  
  def generateKernel(f: Fun) : String = {
    // generate the body of the kernel
    val body = generate(f, Array.empty[AccessFunction])
    
    Kernel.prefix + "\n" + "kernel void KERNEL () {\n" + body + "}\n"
  }
  
  private type AccessFunction = (Expr) => Expr
  
  private object Kernel {
	  val prefix = new StringBuilder
  }
  
  private def generate(f: Fun, accessFunctions: Array[AccessFunction]) : String = {
    assert(f.inT != UndefType)
    assert(f.ouT != UndefType)
    
    f match {
      case cf: CompFun => cf.funs.foldRight("")((inF, str) => str + generate(inF, accessFunctions))
      // maps
      case m: MapWrg => generateMapWrg(m, accessFunctions)
      case m: MapLcl => generateMapLcl(m, accessFunctions)
      // reduce
      case r: ReduceSeq => generateReduceSeq(r, accessFunctions)
      // user functions
      case u : UserFun => {
        Kernel.prefix.append(u.body + "\n")
        u.name // return the name
        }
      // utilities
      case _: oSplit => ""
      case _: oJoin => ""
      case _ => "__" + f.toString() + "__"
    }
  }
  
  // === Maps ===
  // generic Map
  private def generateMap(m: AbstractMap, f: Fun, indexVar: Expr, range: RangeAdd, accessFunctions: Array[AccessFunction]) : String = {
    val elemT = Type.getElemT(m.inT)
    
	// multiply all lengths with the indexVariable ...
    val expr = Type.length(elemT).foldLeft(indexVar)( _ * _ )
    val accessFun = (index: Expr) => { expr + index }
    
    val body = generate(f, accessFunctions :+ accessFun) + "\n"
    
    generateLoop(indexVar, range, body)
  }
  
  // MapWrg
  private def generateMapWrg(m: MapWrg, accessFunctions: Array[AccessFunction]) : String = {
    val len = Type.getLength(m.inT)
    val range = RangeAdd(Var("get_group_id(0)"), len, Var("get_num_groups(0)"))
    val indexVar = Var("g_id") // range
      
    generateMap(m, m.f, indexVar, range, accessFunctions) +
    "return;\n"
  }
  
  // MapLcl
  private def generateMapLcl(m: MapLcl, accessFunctions: Array[AccessFunction]) : String = {
    val len = Type.getLength(m.inT)
    val range = RangeAdd(Var("get_local_id(0)"), len, Var("get_local_size(0)"))
    val indexVar = Var("l_id") // range
      
    generateMap(m, m.f, indexVar, range, accessFunctions) +
    generateBarrier
  }
  
  // === Reduce ===
  private def generateReduceSeq(r: ReduceSeq, accessFunctions: Array[AccessFunction]) : String = {
     val elemT = Type.getElemT(r.inT)
     val len = Type.getLength(r.inT)
     
     val fName = generate(r.f, accessFunctions) // kind of expecting a name here ...
     val typeName = "int" // r.f.inT (binary func...)
     val inputVarName = "input" // has to be passed down here ...
     val outputVarName = "output" // has to be allocated ...
     
     val init = typeName + " acc = " /*+ r.id*/ + ";\n" // TODO: fix reduction
     val loop = generateReductionLoop(fName, len, inputVarName, accessFunctions)
      	
     val outputAccessFun = (index: Expr) => { index / len } // add access function for the output
     // apply index function one after the other following the LIFO order ...
     val outputAccess = (accessFunctions :+ outputAccessFun).foldLeft[Expr](Cst(0))((index, accessFun) => { accessFun(index) })
    
     val writeBack = outputVarName + "[" + outputAccess + "] = acc;\n"
     
     "{ /* reduce_seq */\n" + init + loop + writeBack + "} /* reduce_seq */"
  }
   
  private def generateReductionLoop(fName: String, len: Expr, inputVarName: String, accessFunctions: Array[AccessFunction]) : String = {
    val range = RangeAdd(Cst(0), len, Cst(1))
    val indexVar = Var("i") // range
    
    // apply index function one after the other following the FIFO order ...
    val inputAccess = accessFunctions.foldRight[Expr](indexVar)((accessFun, index) => { accessFun(index) })
    
    val body = "  acc = " + fName + "(acc, " + inputVarName + "[" + inputAccess + "]);\n"
    
    generateLoop(indexVar, range, body)
  }
  

  // === Utilities ===
  
  private def generateLoop(indexVar: Expr, range: RangeAdd, body: String) : String = {
    // TODO: Do "analysis" range and don't create loop if not necessary ...
    
    "for (int " + indexVar + " = " + range.start + "; " + indexVar + " < " + range.stop + "; " +
    indexVar + " += " + range.step + ") {\n" + body + "}\n";
  }
  
    private def generateBarrier() : String = {
     // TODO: decide between local and global memory based on type information
    "barrier(CLK_LOCAL_MEM_FENCE && CLK_GLOBAL_MEM_FENCE)"
  }
    
}