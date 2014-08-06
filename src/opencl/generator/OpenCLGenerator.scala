package opencl.generator

import Function.tupled
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

object OpenCL{
  val warpSize = 32
}

object OpenCLGenerator extends Generator {

  var oclPrinter: OpenCLPrinter = null

  // compiler a type-checked function into an OpenCL kernel
  def generate(f: Fun) : String = {

    assert (f.inT != UndefType)

    if (Debug()) {
      println("Types:")
      Fun.visit(f, (f: Fun) => {
        println(f + "\n    " + f.ouT + " <- " + f.inT + "\n")
      }, (f: Fun) => {})
    }

    // pass 1
    allocateMemory(f)

    if (Debug()) {
      println("Memory:")
      TypedOpenCLMemory.getAllocatedMemory(f).map(m => println(m))
    }

    oclPrinter = new OpenCLPrinter

    // pass 2: find and generate user functions
    generateUserFunction(f)

    oclPrinter.println()

    // pass 3: generate the kernel
    generateKernel(f)

    // return the code generated
    oclPrinter.code
  }

  /** Traversals f and print all user functions using oclPrinter */
  def generateUserFunction(f: Fun) {
    val userFuns = Fun.visit(Set[UserFun]())(f, (f,set) => f match {
      case uf: UserFun => set + uf
      //case vec: Vectorize => set + UserFun.vectorize(vec.f.asInstanceOf[UserFun], vec.n)
      case _ => set
    })
    userFuns.foreach(uf => {
      oclPrinter.print(oclPrinter.toOpenCL(uf))
      oclPrinter.println()
    })
  }

  def allocateMemory(f: Fun) : Unit = {
    OpenCLMemory.alloc(f)
    Kernel.memory = TypedOpenCLMemory.getAllocatedMemory(f)
  }

  private class AccessFunction(val f: (Expr) => Expr, val scope: String) {
    def apply(e: Expr): Expr = f(e)
  }

  private object AccessFunction {
    def apply(f: (Expr) => Expr, scope: String) = new AccessFunction(f, scope)

    def id(scope: String) = new AccessFunction( (e:Expr) => e, scope)
  }

  private class MapAccessFunction(val loopVar: Var, val chunkSize: Expr, val mapName: String)
    extends AccessFunction( (i:Expr) => (loopVar * chunkSize) + i, mapName )

  private object MapAccessFunction {
    def apply(loopVar: Var, initialChunkSize: Expr, mapName: String) =
      new MapAccessFunction(loopVar, initialChunkSize, mapName)

    def multWithChunkSize(ma: MapAccessFunction, chunkSize: Expr): MapAccessFunction = {
      MapAccessFunction(ma.loopVar, ma.chunkSize * chunkSize, ma.mapName)
    }

    def asMapAccessFunction(a: AccessFunction): MapAccessFunction = {
      a match {
        case ma: MapAccessFunction => ma
        case _ => throw new IllegalArgumentException
      }
    }
  }


  private def generateKernel(f: Fun, workGroupSize: Int = 128) {

    Kernel.workGroupSize = workGroupSize

    // generate kernel function signature
    oclPrinter.print("kernel void KERNEL(")

    oclPrinter.printAsParameterDecl(Kernel.memory)

    // array of all unique vars (like N, iterSize, etc. )
    val allVars = Kernel.memory.map( mem => Var.getVars(mem.mem.size) ).filter(_.nonEmpty).flatten.distinct
    // partition into iteration variables and all others variables
    val (iterateVars, vars) = allVars.partition(_.name == Iterate.varName)

    // generate string for the constants (N, ...)
    if (vars.nonEmpty) {
      val constantsString = vars.map((v) => {
        "int " + oclPrinter.toOpenCL(v)
      }).reduce(oclPrinter.separateByComma)
      oclPrinter.print(", " + constantsString)
    }

    oclPrinter.print(") ")

    // print out allocated memory sizes
    val varMap = iterateVars.map( v => (v, Expr.asCst(v.range.max)) ).toMap
    Kernel.memory.map( mem => {
      val m = mem.mem
      if (Debug()) {
        println("Allocate " + Expr.substitute(m.size, varMap.toMap) + " bytes for variable " + m.variable + " in " +
          m.addressSpace + " memory")
      }
    })

    // generate the body of the kernel
    oclPrinter.openCB()
    generate(f, Array.empty[AccessFunction], Array.empty[AccessFunction])
    oclPrinter.closeCB()
  }
  
  object Kernel {
    var memory = Array.empty[TypedOpenCLMemory]
    var workGroupSize = 128
  }
  
  private def generate(f: Fun, inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    assert(f.ouT != UndefType)

    //val inputAccess = updateInputAccessFunction(oldInputAccess, f.inT)
    
    f match {
      // sequential composition of functions. Allow to pass access functions horizontally.
      // TODO: maybe generalize this (output access function, multiple access functions, pass other information ...)
      // go from right to left, as the data flows ...
      case cf: CompFun => cf.funs.foldRight[Option[AccessFunction]](None)(
        (innerF: Fun, af: Option[AccessFunction]) => innerF match {
          // pass newly created access function to the next function in line
          case r : ReorderStride => Some(createReorderStrideAccessFunction(r, inputAccess.last.scope))

          case _ => {
            if (af.isDefined) {
              generate(innerF, inputAccess :+ af.get, outputAccess)
            } else {
              generate(innerF, inputAccess, outputAccess)
            }
            None // af is consumed and not passed to the next function in line
          }
        })
      // case cf: CompFun => cf.funs.reverseMap(inF => generate(inF, inputAccess, outputAccess))
      // maps
      case m: MapWrg => generateMapWrg(m, inputAccess, outputAccess)
      case m: MapLcl => generateMapLcl(m, inputAccess, outputAccess)
      case m: MapWarp => generateMapWarp(m, inputAccess, outputAccess)
      case m: MapLane => generateMapLane(m, inputAccess, outputAccess)
      case m: MapSeq => generateMapSeq(m, inputAccess, outputAccess)
      // reduce
      case r: ReduceSeq => generateReduceSeq(r, inputAccess, outputAccess)
      case r: ReduceHost => generateReduceSeq(r, inputAccess, outputAccess)
      // iterate
      case i: Iterate => generateIterate(i, inputAccess, outputAccess)
      // reorder
      //case r : ReorderStride => generateReorderStride(r, inputAccess, outputAccess)
      // user functions
      case u : UserFun => oclPrinter.generateFunCall(u)
      // utilities
      case f: toGlobal => generate(f.f, inputAccess, outputAccess)
      case f: toLocal => generate(f.f, inputAccess, outputAccess)
      case l: Lambda => generate(l.f, inputAccess, outputAccess)
      case _: asVector =>
      case _: asScalar =>
      case _: Split =>
      case _: Join =>
      case _: Input =>
      case _: Param =>
      case _: Zip =>
      case _ => oclPrinter.print("__" + f.toString + "__")
    }
  }
  
  // === Maps ===
  // generic Map
  private def generateMap(m: AbstractMap, f: Fun, loopVar: Var, range: RangeAdd,
                          inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction],
                          mapName: String) {
    val inAccessFun = MapAccessFunction(loopVar, Cst(1), mapName)
    val updatedInputAccess = updateAccessFunction(inputAccess, Type.length(m.inT).head) :+ inAccessFun

    // output
    val outAccessFun = MapAccessFunction(loopVar, Cst(1), mapName)
    val updatedOutputAccess = updateAccessFunction(outputAccess, Type.length(m.ouT).head) :+ outAccessFun

    oclPrinter.generateLoop(loopVar, range, () => generate(m.f, updatedInputAccess, updatedOutputAccess))
  }
  
  // MapWrg
  private def generateMapWrg(m: MapWrg,
                             inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    val range = RangeAdd(new get_group_id(0), Type.getLength(m.inT), new get_num_groups(0))
    val loopVar = Var("g_id", range)

    generateMap(m, m.f, loopVar, range, inputAccess, outputAccess, "MapWrg")
    oclPrinter.println("return;")
  }
  
  // MapLcl
  private def generateMapLcl(m: MapLcl,
                             inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    val range = RangeAdd(new get_local_id(0), Type.getLength(m.inT), Cst(Kernel.workGroupSize))
    val loopVar = Var("l_id", range)

    generateMap(m, m.f, loopVar, range, inputAccess, outputAccess, "MapLcl")
    oclPrinter.generateBarrier(m.outM)
  }

  // MapWarp
  private def generateMapWarp(m: MapWarp,
                              inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    val range = RangeAdd(new get_local_id(0) / OpenCL.warpSize,
                         Type.getLength(m.inT),
                         Cst(Kernel.workGroupSize) / OpenCL.warpSize)
    val loopVar = Var("warp_id", range)

    generateMap(m, m.f, loopVar, range, inputAccess, outputAccess, "MapWarp")
    oclPrinter.generateBarrier(m.outM)
  }

  // MapLane
  private def generateMapLane(m: MapLane,
                              inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    val range = RangeAdd(new get_local_id(0) & (OpenCL.warpSize - Cst(1)), Type.getLength(m.inT), OpenCL.warpSize)
    val loopVar = Var("lane_id", range)

    generateMap(m, m.f, loopVar, range, inputAccess, outputAccess, "MapLane")
  }
  
  // MapSeq
  private def generateMapSeq(m: MapSeq,
                             inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    val range = ContinousRange(Cst(0), Type.getLength(m.inT))
    val loopVar = Var("i", range)

    // input
    val inChunkSizes = Type.length(m.inT).reduce(_ * _) // this includes the vector size
    val updatedInputAccess = updateAccessFunction(inputAccess, inChunkSizes)

    // output
    val outChunkSizes = Type.length(m.ouT).reduce(_ * _)  // this includes the vector size
    val updatedOutputAccess = updateAccessFunction(outputAccess, outChunkSizes)

    oclPrinter.commln("map_seq")
    oclPrinter.generateLoop(loopVar, range, () => {
      // output[i] = f(input[i])
      oclPrinter.print(access(m.outM, m.f.ouT, updatedOutputAccess, loopVar) + " = ")
      oclPrinter.generateFunCall(m.f, access(m.inM, m.f.inT, updatedInputAccess, loopVar))
      oclPrinter.println(";")
    })
    oclPrinter.commln("map_seq")

  }
  
  // === Reduce ===
  private def generateReduceSeq(r: AbstractReduce,
                                inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
     // input
     val inChunkSizes = Type.length(r.inT).reduce(_ * _) // this includes the vector size
     val updatedInputAccess = updateAccessFunction(inputAccess, inChunkSizes)

     // output
     val outChunkSizes = Type.length(r.ouT).reduce(_ * _)  // this includes the vector size
     val updatedOutputAccess = updateAccessFunction(outputAccess, outChunkSizes)

     oclPrinter.openCB()
     oclPrinter.commln("reduce_seq")

     // 1. generate: int acc = 0
     val acc = Var("acc")
     val accType = r.init.expectedOutT
     oclPrinter.printVarDecl(accType, acc, r.init.value)
     oclPrinter.println(";")

     // 2. generate loop from 0 .. length
     val range = RangeAdd(Cst(0), Type.getLength(r.inT), Cst(1))
     val loopVar = Var("i", range)
     oclPrinter.generateLoop(loopVar, range, () => {
       // 3. generate acc = fun(acc, input[i])
       oclPrinter.print(oclPrinter.toOpenCL(acc) + " = ")
       val t = r.f.inT match { case tt: TupleType => tt.elemsT(1) } // type of the second argument
       oclPrinter.generateFunCall(r.f, oclPrinter.toOpenCL(acc), access(r.inM, t, updatedInputAccess, loopVar))
       oclPrinter.println(";")
     })

     // 4. generate output[0] = acc
     oclPrinter.println(access(r.outM, r.f.ouT, updatedOutputAccess, Cst(0)) =:= oclPrinter.toOpenCL(acc))
     oclPrinter.commln("reduce_seq")
     oclPrinter.closeCB()
  }

  // === Iterate ===
  private def generateIterate(i: Iterate,
                              inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) = {

    val inputMem = OpenCLMemory.asOpenCLMemory(i.inM)
    val outputMem = OpenCLMemory.asOpenCLMemory(i.outM)
    val swapMem = OpenCLMemory.asOpenCLMemory(i.swapBuffer)

    assert (inputMem.addressSpace == outputMem.addressSpace)

    val innerInputLength = Type.getLength(i.f.inT)
    val innerOutputLength = Type.getLength(i.f.ouT)

    oclPrinter.openCB()

    // use the type var as the var holding the iterating size if it exists
    if (TypeVar.getTypeVars(i.f.inT).size > 1) {
      println("size: " + TypeVar.getTypeVars(i.f.inT).size)
      TypeVar.getTypeVars(i.f.inT).map( (tv) => { println("tv: " + tv) })
      println("i.f.inT " + i.f.inT)
      throw new NotImplementedError()
    }
    val curOutLen =
      if (TypeVar.getTypeVars(i.f.inT).isEmpty)
        Var("curOutLen")
      else
        TypeVar.getTypeVars(i.f.inT).head
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
    oclPrinter.println(outputMem.addressSpace + " " + oclPrinter.toOpenCL(Type.devectorize(i.ouT)) + " " + tinVStr + " = " + inVStr+";")

    // ADDRSPC TYPE tin = (odd ? out : swap);
    oclPrinter.print(outputMem.addressSpace + " " + oclPrinter.toOpenCL(Type.devectorize(i.ouT)) + " " + toutVStr + " = ")
    oclPrinter.print("( ("+oclPrinter.toOpenCL(range.stop)+" & 1) != 0 ) ? ")
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
      oclPrinter.println(oclPrinter.toOpenCL(curOutLen) + " = " +
                         oclPrinter.toOpenCL(ExprSimplifier.simplify(curOutLen * innerOutputLength / innerInputLength))+
                         ";")

      // tin = (tout == swap) ? swap : out
      oclPrinter.println(tinVStr + " = ( " + toutVStr+"=="+swapVStr+" ) ? "+ swapVStr +":"+ outVStr+";")
      // tout = (tout == swap) ? out : swap
      oclPrinter.println(toutVStr + " = ( " + toutVStr+"=="+swapVStr+" ) ? "+ outVStr +":"+ swapVStr+";")
    } )

    oclPrinter.closeCB()
  }

  // === ReorderStride ===
  private def createReorderStrideAccessFunction(r: ReorderStride, scope: String) = {
    val s = Type.getLength(r.inT)
    val n = Type.getLength(Type.getElemT(r.inT))
    AccessFunction( (i:Expr) => { i / n + s * (i % n) } , scope)
  }

  // === Utilities ===

  private def updateAccessFunction(access: Array[AccessFunction], chunkSize: Expr) = {
    access.map( af => af match {
      case ma: MapAccessFunction => MapAccessFunction.multWithChunkSize(ma, chunkSize)
      case af: AccessFunction => af
    } )
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
  
  private def access(memory: Memory, t: Type, accessFunctions: Array[AccessFunction], index: Expr): String = {
    val oclMem = OpenCLMemory.asOpenCLMemory(memory)

    if (oclMem.isInstanceOf[OpenCLMemoryCollection]) {
      assert(t.isInstanceOf[TupleType])
      val tt = t.asInstanceOf[TupleType]

      val coll = oclMem.asInstanceOf[OpenCLMemoryCollection]

      assert(tt.elemsT.length == coll.subMemories.length)

      (coll.subMemories zip tt.elemsT).map( { case (m, t) => access(m, t, accessFunctions, index) } ).reduce(_ + ", " + _)
    } else {
      oclMem.addressSpace match {
        case GlobalMemory =>
          "*((global " + oclPrinter.toOpenCL(t) + "*)&" +
            oclPrinter.toOpenCL(oclMem.variable) +
            "[" + oclPrinter.toOpenCL(accessFunctions.foldRight[Expr](index * Type.getVectorSize(t))((aF, i) => {
            aF(i)
          })) + "])"

        case LocalMemory =>
          // access function from the kernel or MapWrg scope should not affect local
          val localAccessFunctions = accessFunctions.filter((a) => {
            (a.scope != "MapWrg") && (a.scope != "Kernel")
          })
          "*((local " + oclPrinter.toOpenCL(t) + "*)&" +
            oclPrinter.toOpenCL(oclMem.variable) +
            "[" + oclPrinter.toOpenCL(localAccessFunctions.foldRight[Expr](index * Type.getVectorSize(t))((aF, i) => {
            aF(i)
          })) + "])"

        case _ => throw new NotImplementedError()
      }
    }
  }



}