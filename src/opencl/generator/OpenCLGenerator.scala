package opencl.generator

import generator.Generator
import ir._
import opencl.ir._

class NotPrintableExpression(msg: String) extends Exception(msg)
class NotI(msg: String) extends Exception(msg)

// hacky class to store function name
case class OclFunction(name: String, param: Int) extends Expr {def toOCLString = name+"("+param+")"}

class get_global_id(param: Int) extends OclFunction("get_global_id", param)
class get_local_id(param: Int) extends OclFunction("get_local_id", param)
class get_group_id(param: Int) extends OclFunction("get_group_id", param)
class get_num_groups(param: Int) extends OclFunction("get_num_groups", param)


object Debug {
  def apply() = { true }
}

object OpenCLGenerator extends Generator {

  var oclPrinter: OpenCLPrinter = null

  // compiler a type-checked function into an OpenCL kernel
  def generate(f: Fun) : String = {

    assert (f.inT != UndefType)

    // pass 1
    //Type.check(f)

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
      OpenCLMemory.getAllocatedMemory(f).map(m => println(m))
    }

    oclPrinter = new OpenCLPrinter

    // pass 3: find and generate user functions
    generateUserFunction(f)

    // pass 4: generate the kernel
    generateKernel(f)

    // return the code generated
    oclPrinter.code
  }

  def generateUserFunction(f: Fun) {
    val userFuns = Fun.visit(Set[UserFun]())(f, (f,set) => f match {
      case uf :UserFun => set + uf
      case _ => set
    })
    userFuns.foreach(uf => {
      oclPrinter.print(uf.body)
      oclPrinter.println()
    })
  }




  def allocateMemory(f: Fun) : Unit = {
    OpenCLMemory.alloc(f)
    Kernel.memory = OpenCLMemory.getAllocatedMemory(f)
  }

  private class AccessFunction(/*val ptr: Var, */val f: (Expr) => Expr, val scope: String) {
    def apply(e: Expr): Expr = f(e)
  }

  private object AccessFunction {
    def apply(f: (Expr) => Expr, scope: String) = new AccessFunction(f, scope)
  }

  private class MapAccessFunction(val loopVar: Var, var chunkSize: Expr, val mapName: String) extends AccessFunction( (i:Expr) => (loopVar * chunkSize) + i, mapName )

  private object MapAccessFunction {
    def apply(loopVar: Var, initialChunkSize: Expr, mapName: String) =
      new MapAccessFunction(loopVar, initialChunkSize, mapName)

    def asMapAccessFunction(a: AccessFunction): MapAccessFunction = {
      a match {
        case ma: MapAccessFunction => ma
        case _ => throw new IllegalArgumentException
      }
    }
  }


    private def generateKernel(f: Fun, workGroupSize: Int = 128) {

    Kernel.workGroupSize = workGroupSize

    oclPrinter.print("kernel void KERNEL(")
    oclPrinter.printAsParameterDecl(Kernel.memory.map(m => OpenCLMemory.asOpenCLMemory(m)))

    // array of all unique vars (like N, iterSize, etc. )
    val allVars = Kernel.memory.map( m => Var.getVars(m.size) ).filter(_.nonEmpty).flatten.distinct
    // partition into iteration variables and all others variables
    val (iterateVars, vars) = allVars.partition(_.name == Iterate.varName)

    val varMap = iterateVars.map( v => (v, Expr.asCst(v.range.max)) ).toMap

    Kernel.memory.map( mem => {
      val m = OpenCLMemory.asOpenCLMemory(mem)
      println("Allocate " + Expr.substitute(m.size, varMap.toMap) + " bytes for variable " + m.variable + " in " +
              m.addressSpace + " memory")
    })

    // generate string for the constants (N, ...)
    val constantsString = vars.map( (v) => { "int " + oclPrinter.toOpenCL(v) }).reduce(oclPrinter.separateByComma)
    if (constantsString.nonEmpty)
        oclPrinter.print(", " + constantsString)

    oclPrinter.print(")")

    // generate the body of the kernel
    oclPrinter.openCB()
    generate(f, Array.empty[AccessFunction], Array.empty[AccessFunction])
    oclPrinter.closeCB()
  }
  
  object Kernel {
    var memory = Array.empty[OpenCLMemory]
    var workGroupSize = 128
  }
  
  private def generate(f: Fun, inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    assert(f.ouT != UndefType)
    
    f match {
      case cf: CompFun => cf.funs.reverseMap(inF => generate(inF, inputAccess, outputAccess))
      // maps
      case m: MapWrg => generateMapWrg(m, inputAccess, outputAccess)
      case m: MapLcl => generateMapLcl(m, inputAccess, outputAccess)
      case m: MapSeq => generateMapSeq(m, inputAccess, outputAccess)
      // reduce
      case r: ReduceSeq => generateReduceSeq(r, inputAccess, outputAccess)
      case r: ReduceHost => generateReduceSeq(r, inputAccess, outputAccess)
      // iterate
      case i: Iterate => generateIterate(i, inputAccess, outputAccess)
      // user functions
      case u : UserFun => oclPrinter.generateFunCall(u)
      // utilities
      case _: Split =>
      case _: Join =>
      case f: toGlobal => generate(f.f, inputAccess, outputAccess)
      case f: toLocal => generate(f.f, inputAccess, outputAccess)
      case _: Input =>
      case _ => oclPrinter.print("__" + f.toString + "__")
    }
  }
  
  // === Maps ===
  // generic Map
  private def generateMap(m: AbstractMap, f: Fun, loopVar: Var, range: RangeAdd,
                          inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction],
                          mapName: String) {
    val inChunkSize = Type.length(m.inT).reduce[Expr]( _ * _ )
    val inInnerChunkSize = Type.length(Type.getElemT(m.inT)).reduce[Expr]( _ * _ )
    val inAccessFun = MapAccessFunction(loopVar, inInnerChunkSize, mapName)
    val updatedInputAccess = updateMapAccessFunction(inputAccess, inChunkSize) :+ inAccessFun

    val outChunkSize = Type.length(m.ouT).reduce[Expr]( _ * _ )
    val outInnerChunkSize = Type.length(Type.getElemT(m.ouT)).reduce[Expr]( _ * _ )
    val outAccessFun = MapAccessFunction(loopVar, outInnerChunkSize, mapName)
    val updatedOutputAccess = updateMapAccessFunction(outputAccess, outChunkSize) :+ outAccessFun

    oclPrinter.generateLoop(loopVar, range, () => generate(f, updatedInputAccess, updatedOutputAccess))
  }
  
  // MapWrg
  private def generateMapWrg(m: MapWrg,
                             inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    val length = Type.getLength(m.inT)
    val range = RangeAdd(new get_group_id(0),
                         length,
                         new get_num_groups(0))
    val loopVar = Var("g_id", range)
      
    generateMap(m, m.f, loopVar, range, inputAccess, outputAccess, "MapWrg")
    oclPrinter.println("return;")
  }
  
  // MapLcl
  private def generateMapLcl(m: MapLcl,
                             inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    val length = Type.getLength(m.inT)
    val wgSize = Cst(Kernel.workGroupSize)
    val range = RangeAdd(new get_local_id(0),
                         length, wgSize)
    val loopVar = Var("l_id", range)
      
    generateMap(m, m.f, loopVar, range, inputAccess, outputAccess, "MapLcl")
    oclPrinter.generateBarrier(m.outM)
  }
  
  // MapSeq
  private def generateMapSeq(m: MapSeq,
                             inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    val length = Type.getLength(m.inT)
    
    val inputMem = m.inM
    val outputMem = m.outM

    val range = ContinousRange(Cst(0), length)
    val indexVar = Var("i", range)

    oclPrinter.commln("map_seq")
    oclPrinter.generateLoop(indexVar, range, () => {
      // output[i] = f(input[i])
      oclPrinter.print(access(outputMem, outputAccess, indexVar) + " = ")
      m.f match {
        case uf : UserFun => oclPrinter.generateFunCall(uf, access(inputMem, inputAccess, indexVar))
        case _ => throw new NotImplementedError()
      }
      oclPrinter.println(";")
    })
    oclPrinter.commln("map_seq")

  }
  
  // === Reduce ===
  private def generateReduceSeq(r: AbstractReduce,
                                inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {

     val len = Type.getLength(r.inT)
     val inputMem = r.inM
     val outputMem = r.outM


     oclPrinter.openCB()
     oclPrinter.commln("reduce_seq")

     // 1. generate: int acc = input[0]
     val acc = Var("acc")
     oclPrinter.printVarDecl(opencl.ir.Int, acc, access(inputMem, inputAccess, 0))
     oclPrinter.println(";")

     // 2. generate loop from 1 .. length
     val range = RangeAdd(Cst(1), len, Cst(1))
     val indexVar = Var("i", range)
     oclPrinter.generateLoop(indexVar, range, () => {
       // 3. generate acc = fun(acc, input[i])
       oclPrinter.print(oclPrinter.toOpenCL(acc) + " = ")
       r.f match {
         case uf : UserFun => oclPrinter.generateFunCall(uf, oclPrinter.toOpenCL(acc), access(inputMem, inputAccess, indexVar))
         case _ => throw new NotImplementedError()
       }
       oclPrinter.println(";")
     })

     // 4. generate output[0] = acc
     oclPrinter.println(access(outputMem, outputAccess, Cst(0)) =:= oclPrinter.toOpenCL(acc))
     oclPrinter.commln("reduce_seq")
     oclPrinter.closeCB()
  }

  // === Iterate ===
  private def generateIterate(i: Iterate,
                              inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {

    val inputMem = OpenCLMemory.asOpenCLMemory(i.inM)
    val outputMem = OpenCLMemory.asOpenCLMemory(i.outM)
    val swapMem = OpenCLMemory.asOpenCLMemory(i.swapBuffer)

    assert (inputMem.addressSpace == outputMem.addressSpace)

    val innerInputLength = Type.getLength(i.f.inT)
    val innerOutputLength = Type.getLength(i.f.ouT)

    oclPrinter.openCB()

    // use the type var as the var holding the iterating size
    val curOutLen = TypeVar.getTypeVars(i.f.inT).head
    oclPrinter.printVarDecl(opencl.ir.Int, curOutLen, oclPrinter.toOpenCL(Type.getLength(i.inT)))
    oclPrinter.println(";")

    val range = ContinousRange(Cst(0), i.n)
    val indexVar = Var("i", range)

    // create new temporary input and output pointers
    val tin = Var("tin")
    val tout = Var("tout")

    val tinVStr = oclPrinter.toOpenCL(tin)
    val toutVStr = oclPrinter.toOpenCL(tout)
    val inVStr = oclPrinter.toOpenCL(inputMem.variable)
    val outVStr = oclPrinter.toOpenCL(outputMem.variable)
    val swapVStr = oclPrinter.toOpenCL(swapMem.variable)

    // ADDRSPC TYPE tin = in;
    oclPrinter.println(outputMem.addressSpace + " " + oclPrinter.toOpenCL(outputMem.t) + " " + tinVStr + " = " + inVStr+";")

    // ADDRSPC TYPE tin = (odd ? out : swap);
    oclPrinter.print(outputMem.addressSpace + " " + oclPrinter.toOpenCL(outputMem.t) + " " + toutVStr + " = ")
    oclPrinter.print("("+oclPrinter.toOpenCL(range.stop)+" & 1 != 0 ) ? ")
    oclPrinter.print(outVStr + " : " + swapVStr)
    oclPrinter.println(" ;")




    oclPrinter.println("#pragma unroll 1")
    oclPrinter.generateLoop(indexVar, range, () => {

      // modify the pointers to the memory before generating the body
      val oldInV = inputMem.variable
      val oldOutV = outputMem.variable
      inputMem.variable = tin
      outputMem.variable = tout

      // generate the body
      generate(i.f, inputAccess, outputAccess)

      // restore the pointers to memory
      inputMem.variable = oldInV
      outputMem.variable = oldOutV


      // tmp = tmp * outputLen / inputLen
      oclPrinter.println(oclPrinter.toOpenCL(curOutLen) + " = "+ oclPrinter.toOpenCL(ExprSimplifier.simplify(curOutLen * innerOutputLength / innerInputLength)) + ";")

      // tin = (tout == swap) ? swap : out
      oclPrinter.println(tinVStr + " = ( " + toutVStr+"=="+swapVStr+" ) ? "+ swapVStr +":"+ outVStr+";")
      // tout = (tout == swap) ? out : swap
      oclPrinter.println(toutVStr + " = ( " + toutVStr+"=="+swapVStr+" ) ? "+ outVStr +":"+ swapVStr+";")


      //generateSwap(i.swapBuffer, outputMem)
    } )

    //// make sure the output variable actually holds the computed data
    //oclPrinter.println(oclPrinter.toOpenCL(outputMem.variable)+" = "+oclPrinter.toOpenCL(i.swapBuffer.variable) + ";")
    oclPrinter.closeCB()


  }


 /* private def generateSwap(first: Memory, second: Memory) {

    //assert(first.t == second.t)

    val addrsSpace = OpenCLMemory.asOpenCLMemory(first).addressSpace
    val tmp = Var("tmp")

    // ADDRSPC TYPE tmp = first;
    oclPrinter.println(addrsSpace + " " + oclPrinter.toOpenCL(first.t) + " " + oclPrinter.toOpenCL(tmp) + " = " + oclPrinter.toOpenCL(first.variable)+";")
    // first = second;
    oclPrinter.println(oclPrinter.toOpenCL(first.variable) + " = " + oclPrinter.toOpenCL(second.variable)+";")
    // second = tmp;
    oclPrinter.println(oclPrinter.toOpenCL(second.variable) + " = " + oclPrinter.toOpenCL(tmp)+";")
  }*/



  // === Utilities ===

  private def updateMapAccessFunction(accessFunction: Array[AccessFunction], chunkSize: Expr): Array[AccessFunction] = {
    val i = accessFunction.lastIndexWhere(
      _ match {
        case _:MapAccessFunction => true
        case _ => false
    })
    if (i == -1) return accessFunction

    val aOld = MapAccessFunction.asMapAccessFunction(accessFunction(i))
    val aNew = MapAccessFunction(aOld.loopVar, chunkSize, aOld.mapName)
    accessFunction.updated(i,aNew)
  }
  

  

  
  // helper functions to generate the actual OpenCL code
  private implicit class Operators(v: Any) {
    // generate assignment
    def =:=(rhs: Any) : String = { this + " = " + rhs + ";\n" }

    override def toString : String = v.toString
  }
  
  private def apply(fun: Any, arg: Any*) : String = {
    fun + "(" + arg.reduce( _ + ", " + _) + ")"
  }
  
  private def privateVar(variable: Any, t: Type, init: Any) : String = {
    print(t) + " " + variable =:= init
  }

 /* private def privateVar(variable: Var, t: Type, addressSpace: OpenCLAddressSpace, init: Var): String = {
    addressSpace + " " + print(t) + " " + oclPrinter.toOpenCL(variable) =:= init
  }*/
  
  private def access(memory: Memory, accessFunctions: Array[AccessFunction], index: Expr): String = {
    val oclMem = OpenCLMemory.asOpenCLMemory(memory)
    oclMem.addressSpace match {
      case GlobalMemory =>
        oclPrinter.toOpenCL(oclMem.variable) + "[" + oclPrinter.toOpenCL(accessFunctions.foldRight[Expr](index)((aF, i) => { aF(i) })) + "]"

      case LocalMemory =>
        // access function from the kernel or MapWrg scope should not affect local
        val localAccessFunctions = accessFunctions.filter((a) => {
          (a.scope != "MapWrg") && (a.scope != "Kernel")
        })
        oclPrinter.toOpenCL(oclMem.variable) + "[" + oclPrinter.toOpenCL(localAccessFunctions.foldRight[Expr](index)((aF, i) => { aF(i) })) + "]"

      case _ => throw new NotImplementedError()
    }
  }



}