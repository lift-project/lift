package opencl.generator

import generator.Generator
import ir._
import opencl.ir._
import scala.collection.mutable.ArrayBuffer

object OpenCLGenerator extends Generator {
  
  def generate(f: Fun) : String = {
    generateKernel(f)
  }
  
  private type AccessFunction = (Expr) => Expr
  
  private def generateKernel(f: Fun) : String = {
    // generate the body of the kernel
    val body = generate(f, Array.empty[AccessFunction])
    
    val parameterString = Kernel.parameters.reduce( _ + ", \n" + _)
    
    Kernel.prefix + "\n" + "kernel void KERNEL (" + parameterString +") {\n" + body + "}\n"
  }
  
  private object Kernel {
	  val prefix = new StringBuilder
	  var parameters = new ArrayBuffer[String]
  }
  
  private def generate(f: Fun, accessFunctions: Array[AccessFunction]) : String = {
    assert(f.ouT != UndefType)
    
    f match {
      case cf: CompFun => cf.funs.foldRight("")((inF, str) => str + generate(inF, accessFunctions))
      // maps
      case m: MapWrg => generateMapWrg(m, accessFunctions)
      case m: MapLcl => generateMapLcl(m, accessFunctions)
      // reduce
      case r: ReduceSeq => generateReduceSeq(r, accessFunctions)
      // user functions
      case u : UserFun => generateUserFun(u)
      // utilities
      case _: Split => ""
      case _: Join => ""
      case i: Input => generateInput(i)
      case _ => "__" + f.toString() + "__"
    }
  }
  
  // === Maps ===
  // generic Map
  private def generateMap(m: AbstractMap, f: Fun, indexVar: Expr, range: RangeAdd,
                          accessFunctions: Array[AccessFunction]) : String = {
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
     val typeName = print(r.f.ouT)
     
     // input
     val inputVarName = r.memory.head.variable
     // apply index function one after the other following the FIFO order ...
     val generateInputAccess = (i : Expr) => {
         inputVarName + "[" +
           accessFunctions.foldRight[Expr](i)((accessFun, index) => { accessFun(index) }) +
         "]" }
       
     // output
     val outputVar = r.memory.last.variable
     val outputAccessFun = (index: Expr) => { index / len } // add access function for the output
     // apply index function one after the other following the LIFO order ...
     val generateOutputAccess = (i : Expr ) => {
    	 outputVar + "[" +
    	   (accessFunctions :+ outputAccessFun).foldLeft[Expr](i)((index, accessFun) => { accessFun(index) }) +
         "]" }
     
     // genetate: int acc = input[0]
     val init = typeName + " acc = " + generateInputAccess(Cst(0)) + ";\n"
     
     // generate loop from 1 .. length
     val range = RangeAdd(Cst(1), len, Cst(1))
     val indexVar = Var("i") // range
     val body = "  acc = " + fName + "(acc, " + generateInputAccess(indexVar) + ");\n"
     val loop = generateLoop(indexVar, range, body)
    
     // generate output[0] = acc
     val writeBack = generateOutputAccess(Cst(0)) + " = acc;\n"
     
     "{ /* reduce_seq */\n" + init + loop + writeBack + "} /* reduce_seq */"
  }
  
  // === UserFun ===
  
  private def generateUserFun(uF: UserFun) : String = {
    Kernel.prefix.append(uF.body + "\n")
    uF.name // return the name
  }
  
  // === Inut ===
  private def generateInput(input: Input) : String = {
    Kernel.parameters.append(printAsParameterDecl(input))
    ""
  }
  

  // === Utilities ===
  
  private def generateLoop(indexVar: Expr, range: RangeAdd, body: String) : String = {
    // TODO: Do "analysis" range and don't create loop if not necessary ...
    
    "for (int " + indexVar + " = " + range.start + "; " + indexVar + " < " + range.stop + "; " +
    indexVar + " += " + range.step + ") {\n" + body + "}\n";
  }
  
  private def generateBarrier() : String = {
    // TODO: decide between local and global memory based on address space information
    "barrier(CLK_LOCAL_MEM_FENCE && CLK_GLOBAL_MEM_FENCE)"
  }
  
  // === printing methods ===
  private def printAsParameterDecl(input: Input) : String = {
    val t = input.expectedOutT 
    t match {
      case TupleType(_) => throw new Exception // TODO: handle this ..., create multiple variables
      case _ => "global " + print(t) + " " + input.variable.name
    }
  }
  
  private def print(t: Type) : String = {
    t match {
      case ArrayType(elemT, _) => print(elemT) + "*"
      case VectorType(elemT, len) => print(elemT) + len
      case ScalarType(name, _) => name
      case TupleType(_) => throw new Exception // don't know how to print a tuple in opencl ...
    }
  }
  
}