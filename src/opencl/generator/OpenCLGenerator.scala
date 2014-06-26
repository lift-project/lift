package opencl.generator

import generator.Generator
import ir._
import opencl.ir._
import scala.collection.mutable.ArrayBuffer

class NotPrintableExpression(msg: String) extends Exception(msg)

object Debug {
  def apply() = { true }
}

object OpenCLGenerator extends Generator {

  def compile(f: Fun) : String = {
    // pass 1
    Type.check(f)

    if (Debug()) {
      println("Types:")
      Fun.visit(f, (f: Fun) => {
        println(f.ouT + " <- " + f.inT + " | " + f)
      }, (f: Fun) => {})
    }

    // pass 2
    allocateMemory(f)

    if (Debug()) {
      println("Memory:")
      Fun.visit(f, (f: Fun) => {
        if (f.memory.nonEmpty) {
          println(f.memory.last + " <- " + f.memory.head + " | " + f)
        }
      }, (f: Fun) => {})
    }

    // pass 3
    generate(f)
  }

  def generate(f: Fun) : String =  {
    generate(f, 128)
  }
  
  def generate(f: Fun, workGroupSize: Int) : String = {
    generateKernel(f, workGroupSize)
  }
  
  def allocateMemory(f: Fun) : Unit = {
    Kernel.memory = OpenCLMemory.allocate(f)
  }

  private type AccessFunction = (Expr) => Expr
  
  private def generateKernel(f: Fun, workGroupSize: Int) : String = {
    Kernel.workGroupSize = workGroupSize

    // generate the body of the kernel
    val body = generate(f, Array.empty[AccessFunction])

    // generate string from the parameters
    val parameterString = Kernel.memory.map( m =>
      printAsParameterDecl(OpenCLMemory.asOpenCLMemory(m))
    ).reduce(separateByComma)

    // array of all unique vars
    val vars = Kernel.memory.map( m => Var.getVars(m.size) ).filter(_.nonEmpty).flatten.distinct

    // generate string for the sizes
    val sizesString = vars.map( (v) => { "int " + v.name }).reduce(separateByComma)

    val separator = if (sizesString.nonEmpty) { ", " } else { "" }

    Kernel.prefix + "\n" + "kernel void KERNEL(" + parameterString + separator + sizesString + ") {\n" + body + "}\n"
  }
  
  private object Kernel {
	  val prefix = new StringBuilder
    var memory = new Array[Memory](0)
    var workGroupSize = 128
  }
  
  private def generate(f: Fun, accessFunctions: Array[AccessFunction]) : String = {
    assert(f.ouT != UndefType)
    
    f match {
      case cf: CompFun => cf.funs.foldRight("")((inF, str) => str + generate(inF, accessFunctions))
      // maps
      case m: MapWrg => generateMapWrg(m, accessFunctions)
      case m: MapLcl => generateMapLcl(m, accessFunctions)
      case m: MapSeq => generateMapSeq(m, accessFunctions)
      // reduce
      case r: ReduceSeq => generateReduceSeq(r, accessFunctions)
      // iterate
      case i: Iterate => generateIterate(i, accessFunctions)
      // user functions
      case u : UserFun => generateUserFun(u)
      // utilities
      case _: Split => ""
      case _: Join => ""
      case f: toGlobal => generate(f.f, accessFunctions)
      case f: toLocal => generate(f.f, accessFunctions)
      case _: Input => ""
      case _ => "__" + f.toString() + "__"
    }
  }
  
  // === Maps ===
  // generic Map
  private def generateMap(m: AbstractMap, f: Fun, loopVar: Var, range: RangeAdd,
                          accessFunctions: Array[AccessFunction]) : String = {
    val elemT = Type.getElemT(m.inT)
    
	  // multiply all lengths with the indexVariable ...
    val length = Type.length(elemT).foldLeft[Expr](loopVar)( _ * _ )
    val accessFun = (index: Expr) => { length + index }
    
    val body = generate(f, accessFunctions :+ accessFun)
    
    generateLoop(loopVar, range, body)
  }
  
  // MapWrg
  private def generateMapWrg(m: MapWrg, accessFunctions: Array[AccessFunction]) : String = {
    val length = Type.getLength(m.inT)
    val range = RangeAdd(Var("get_group_id(0)", ContinousRange(Cst(0), ?)),
                         length,
                         Var("get_num_groups(0)", ContinousRange(Cst(0), ?)))
    val loopVar = Var("g_id", range)
      
    generateMap(m, m.f, loopVar, range, accessFunctions) +
    "return;\n"
  }
  
  // MapLcl
  private def generateMapLcl(m: MapLcl, accessFunctions: Array[AccessFunction]) : String = {
    val length = Type.getLength(m.inT)
    val wgSize = Cst(Kernel.workGroupSize) // Var("get_local_size(0)")
    val range = RangeAdd(Var("get_local_id(0)", ContinousRange(Cst(0), wgSize)),
                         length, wgSize)
    val loopVar = Var("l_id", range)
      
    generateMap(m, m.f, loopVar, range, accessFunctions) +
    generateBarrier(m.memory.last) + "\n"
  }
  
  // MapSeq
  private def generateMapSeq(m: MapSeq, accessFunctions: Array[AccessFunction]) : String = {
    val fun = generate(m.f, accessFunctions)
    val length = Type.getLength(m.inT)
    
    val inputVar = m.memory.head.variable
    val outputVar = m.memory.last.variable
    
    val range = ContinousRange(Cst(0), length)
    val indexVar = Var("i", range)

    val body = access(outputVar, accessFunctions, indexVar) =:= apply(fun, access(inputVar, accessFunctions, indexVar))
    								
    val loop = generateLoop(indexVar, range, body)
    
    "{ /* map_seq */\n" + loop + "} /* map_seq */\n"
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
     val indexVar = Var("i", range)
     val body = "  " + acc =:= apply(fun, acc, access(inputVar, accessFunctions, indexVar)) 
     val loop = generateLoop(indexVar, range, body)
    
     // 3. generate output[0] = acc
     val writeBack = access(outputVar, outputAccessFun +: accessFunctions, Cst(0)) =:= acc
     
     "{ /* reduce_seq */\n" + init + loop + writeBack + "} /* reduce_seq */\n"
  }

  // === Iterate ===
  private def generateIterate(i: Iterate, accessFunctions: Array[AccessFunction]) : String = {
    val body = generate(i.f, accessFunctions)

    /*
    val length = Type.getLength(m.inT)

    val inputVar = m.memory.head.variable
    val outputVar = m.memory.last.variable

    val range = ContinousRange(Cst(0), length)
    val indexVar = Var("i", range)

    val body = access(outputVar, accessFunctions, indexVar) =:= apply(fun, access(inputVar, accessFunctions, indexVar))

    val loop = generateLoop(indexVar, range, body)

    "{ /* map_seq */\n" + loop + "} /* map_seq */\n"

    {
    int l0_size = 128 * 1;
    #pragma unroll 1
    for (int i = 0; i < 7; i += 1) {
      #ifndef L_SIZE
      #define L_SIZE (l0_size / 2)
      #endif
      {
      int l_id = get_local_id(0);
      if (l_id < (L_SIZE)) {
        { /* reduce_seq */
        int acc = 0;
        for (int i = 0; i < 2; i += 1) {
          acc = sumUp(acc, l0[((l_id * 2 * 1) + i)]);
        }
        l1[((l_id * 2 * 1 / (2)) + 0)] = acc;
        } /* red_seq end */
      }
      }
      #undef L_SIZE
      barrier(CLK_LOCAL_MEM_FENCE);
      l0_size = l0_size/(2);
      local int* tmp = l0;
      l0 = l1;
      l1 = tmp;
    }
    }
    */

    "ITERATE"
  }
  
  // === UserFun ===
  
  private def generateUserFun(uF: UserFun) : String = {
    Kernel.prefix.append(uF.body + "\n")
    uF.name // return the name
  }
  

  // === Utilities ===
  
  private def generateLoop(indexVar: Var, range: RangeAdd, body: String) : String = {
    val init = Expr.simplify(range.start)
    val cond = Expr.simplify(range.stop)
    val update = Expr.simplify(range.step)

    // eval expression. if sucessfull return true and the value, otherwise return false
    def evalExpr = (e: Expr) => { try { (true, e.eval()) } catch { case _ : Throwable => (false, 0) } }

    // try to directly evaluate
    val (initIsEvaluated, initEvaluated) = evalExpr(init)
    val (condIsEvaluated, condEvaluated) = evalExpr(cond)
    val (updateIsEvaluated, updateEvaluated) = evalExpr(update)

    // if all three can be evaluated ...
    if (initIsEvaluated && condIsEvaluated && updateIsEvaluated) {
      // .. and the condition is less or equal than init + update then exactly one iteration is necessary
      if (condEvaluated <= ( initEvaluated + updateEvaluated ) ) {
        return "{\nint " + indexVar + " = " + init + ";\n" + body + "}\n"
      }
    }

    // if condition and update can be evaluated ...
    if (condIsEvaluated && updateIsEvaluated) {
      // ... and the condition is less than the update then at most one iteration is necessary
      if (condEvaluated <= updateEvaluated) {
        return "{\nint " + indexVar + " = " + init + ";\n" +
               "if (" + indexVar + " < (" + cond + ")) {\n" + body + "}\n}\n"
      }
    }

    // as the default print of the default loop
    "for (int " + indexVar + " = " + init  + "; " +
      indexVar + " < " + cond  + "; " +
      indexVar + " += " + update + ") {\n" + body + "}\n";
  }
  
  private def generateBarrier(mem : Memory) : String = {
    mem match {
      case m : OpenCLMemory => generateBarrier(m)
      case _ => ""
    }
  }
  
  private def generateBarrier(mem : OpenCLMemory) : String = {
    if (mem.addressSpace == GlobalMemory) {
      return "barrier(CLK_GLOBAL_MEM_FENCE);"
    }
    if (mem.addressSpace == LocalMemory) {
      return "barrier(CLK_LOCAL_MEM_FENCE);"
    }
    "barrier(CLK_LOCAL_MEM_FENCE && CLK_GLOBAL_MEM_FENCE);"
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

  private def separateByComma(lhs: Any, rhs: Any) = {
    lhs + ", " + rhs
  }
  
  private def print(t: Type) : String = {
    t match {
      case ArrayType(elemT, _) => print(elemT) + "*"
      case VectorType(elemT, len) => print(elemT) + len
      case ScalarType(name, _) => name
      case TupleType(_) => throw new Exception // don't know how to print a tuple in opencl ...
    }
  }

  private def print(e: Expr) : String = {
    val me = if(Debug()) { e } else { Expr.simplify(e) }
    me match {
      case Cst(c) => c.toString
      case Pow(b, ex) => "pow(" + print(b) + ", " + print(ex) + ")"
      case Prod(es) => "(" + es.foldLeft("1")( (s: String, e: Expr) => {
        s + (e match {
          case Pow(b, Cst(-1)) => " / (" + print(b) + ")"
          case _ => " * " + print(e)
        })
      } ).drop(4) /* drop "1 * " */ + ")"
      case Sum(es) => "(" + es.map(print).reduce( _ + " + " + _  ) + ")"
      case Var(n, _) => n
      case ? => throw new NotPrintableExpression("?")
    }
  }
  
  // helper functions to generate the actual OpenCL code
  private implicit class Operators(v: Any) {
    // generate assignment
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
    // TODO: if variable is in local memory do not apply last level of the accessfunctions
    variable + "[" + print(accessFunctions.foldRight[Expr](index)((aF, i) => { aF(i) })) + "]"
  }
  
}