package opencl.generator

import generator.Generator
import ir._
import opencl.ir._
import scala.collection.mutable.ArrayBuffer

object OpenCLGenerator extends Generator {
  
  def generate(f: Fun) : String = {
    generateKernel(f)
  }
  
  def allocateMemory(f: Fun) : Unit = {
    val memory = OpenCLMemory.allocate(f)
    memory.foreach( (mem) => {
      val oclmem = OpenCLMemory.asOpenCLMemory(mem)
      Kernel.parameters.append(printAsParameterDecl(oclmem))
      } )
  }
  
  private type AccessFunction = (Expr) => Expr
  
  private def generateKernel(f: Fun) : String = {
    // generate the body of the kernel
    val body = generate(f, Array.empty[AccessFunction])
    
    assert(!Kernel.parameters.isEmpty)
    val parameterString = Kernel.parameters.reduce( _ + ", " + _)
    
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
      case _: Input => ""
      case _ => "__" + f.toString() + "__"
    }
  }
  
  // === Maps ===
  // generic Map
  private def generateMap(m: AbstractMap, f: Fun, loopVar: Expr, range: RangeAdd,
                          accessFunctions: Array[AccessFunction]) : String = {
    val elemT = Type.getElemT(m.inT)
    
	// multiply all lengths with the indexVariable ...
    val sizes = Type.length(elemT).foldLeft(loopVar)( _ * _ )
    val accessFun = (index: Expr) => { sizes + index }
    
    val body = generate(f, accessFunctions :+ accessFun) + "\n"
    
    generateLoop(loopVar, range, body)
  }
  
  // MapWrg
  private def generateMapWrg(m: MapWrg, accessFunctions: Array[AccessFunction]) : String = {
    val len = Type.getLength(m.inT)
    val range = RangeAdd(Var("get_group_id(0)"), len, Var("get_num_groups(0)"))
    val loopVar = Var("g_id") // range
      
    generateMap(m, m.f, loopVar, range, accessFunctions) +
    "return;\n"
  }
  
  // MapLcl
  private def generateMapLcl(m: MapLcl, accessFunctions: Array[AccessFunction]) : String = {
    val len = Type.getLength(m.inT)
    val range = RangeAdd(Var("get_local_id(0)"), len, Var("get_local_size(0)"))
    val loopVar = Var("l_id") // range
      
    generateMap(m, m.f, loopVar, range, accessFunctions) +
    generateBarrier(m.memory.last)
  }
  
  // MapSeq
  private def generateMapSeq(m: MapSeq, accessFunctions: Array[AccessFunction]) : String = {
    val fun = generate(m.f, accessFunctions)
    val len = Type.getLength(m.inT)
    
    val inputVar = m.memory.head.variable
    val outputVar = m.memory.last.variable
    
    val range = RangeAdd(Cst(0), len, Cst(1))
    val indexVar = Var("i") // , range)
    								
    val body = access(outputVar, accessFunctions, indexVar) =:= apply(fun, access(inputVar, accessFunctions, indexVar))
    								
    val loop = generateLoop(indexVar, range, body)
    
    "{ /* map_seq */\n" + loop + "} /* map_seq */"
  }
  
  // === Reduce ===
  private def generateReduceSeq(r: ReduceSeq, accessFunctions: Array[AccessFunction]) : String = {
     val fun = generate(r.f, accessFunctions) // kind of expecting a name here ...
     val len = Type.getLength(r.inT)
     
     val inputVar = r.memory.head.variable
     val outputVar = r.memory.last.variable
     val outputAccessFun = (index: Expr) => { index / len } // access function for the output
     
     // 1. genetate: int acc = input[0]
     val acc = Var("acc")
     val init = privateVar(acc, r.f.ouT, access(inputVar, accessFunctions, Cst(0)))
     
     // 2. generate loop from 1 .. length
     val range = RangeAdd(Cst(1), len, Cst(1))
     val indexVar = Var("i") // , range)
     val body = "  " + acc =:= apply(fun, acc, access(inputVar, accessFunctions, indexVar)) 
     val loop = generateLoop(indexVar, range, body)
    
     // 3. generate output[0] = acc
     val writeBack = access(outputVar, outputAccessFun +: accessFunctions, Cst(0)) =:= acc
     
     "{ /* reduce_seq */\n" + init + loop + writeBack + "} /* reduce_seq */"
  }
  
  // === UserFun ===
  
  private def generateUserFun(uF: UserFun) : String = {
    Kernel.prefix.append(uF.body + "\n")
    uF.name // return the name
  }
  

  // === Utilities ===
  
  private def generateLoop(indexVar: Expr, range: RangeAdd, body: String) : String = {
    // TODO: Do "analysis" range and don't create loop if not necessary ...
    
    "for (int " + indexVar + " = " + range.start + "; " + indexVar + " < " + range.stop + "; " +
    indexVar + " += " + range.step + ") {\n" + body + "}\n";
  }
  
  private def generateBarrier(mem : Memory) : String = {
    mem match {
      case m : OpenCLMemory => generateBarrier(m)
      case _ => ""
    }
  }
  
  private def generateBarrier(mem : OpenCLMemory) : String = {
    if (mem.addressSpace == GlobalMemory) {
      return "barrier(CLK_GLOBAL_MEM_FENCE)"
    }
    if (mem.addressSpace == LocalMemory) {
      return "barrier(CLK_LOCAL_MEM_FENCE)"
    }
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
  
  private def printAsParameterDecl(mem: OpenCLMemory) : String = {
    mem.addressSpace + " " + print(mem.t) + " " + mem.variable.name
  }
  
  private def print(t: Type) : String = {
    t match {
      case ArrayType(elemT, _) => print(elemT) + "*"
      case VectorType(elemT, len) => print(elemT) + len
      case ScalarType(name, _) => name
      case TupleType(_) => throw new Exception // don't know how to print a tuple in opencl ...
    }
  }
  
  // helper functions generating the actual OpenCL code
  private implicit class Operators(v: Any) {
    def =:=(rhs: Any) : String = { this + " = " + rhs + ";\n" }
    override def toString() : String = v.toString
  }
  
  private def apply(fun: Any, arg: Any*) : String = {
    fun + "(" + arg.reduce( _ + ", " + _) + ")"
  }
  
  private def privateVar(variable: Var, t: Type, init: Any) : String = {
    print(t) + " " + variable =:= init
  }
  
  private def access(variable : Var, accessFunctions :Array[AccessFunction], index : Expr) : String = {
    variable + "[" + accessFunctions.foldRight[Expr](index)((aF, i) => { aF(i) }) + "]"
  }
  
}