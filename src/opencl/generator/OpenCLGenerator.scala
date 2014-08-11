package opencl.generator

import generator.Generator
import ir._
import opencl.ir._

class NotPrintableExpression(msg: String) extends Exception(msg)
class NotI(msg: String) extends Exception(msg)

// hacky class to store function name
case class OclFunction(name: String, param: Int) extends ArithExpr {def toOCLString = name+"("+param+")"}

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
  def generate(f: Lambda) : String = {

    assert (f.body.inT != UndefType)

    if (Debug()) {
      println("Types:")
      Expr.visit(f.body, (f: Expr) => {
        println(f + "\n    " + f.outT + " <- " + f.inT + "\n")
      }, (f: Expr) => {})
    }

    // allocate the params and set the corresponding type
    f.params.map( (p) => {
      p.inM = OpenCLMemory.allocGlobalMemory(OpenCLMemory.getMaxSizeInBytes(p.outT))
      p.inT = p.outT
    })

    // pass 1
    allocateMemory(f.body)

    if (Debug()) {
      println("Memory:")
      Expr.visit(f.body, (f: Expr) => {
        println(f + "\n    " + f.outM.toString + " <- " + f.inM.toString + "\n")
      }, (f: Expr) => {})
    }

    if (Debug()) {
      println("Allocated Memory:")
      TypedOpenCLMemory.getAllocatedMemory(f.body).map(m => println(m.toString))
      println("")
    }

    oclPrinter = new OpenCLPrinter

    // pass 2: find and generate user functions
    generateUserFunction(f.body)

    oclPrinter.println()

    // pass 3: generate the kernel
    generateKernel(f)

    // return the code generated
    oclPrinter.code
  }

  /** Traversals f and print all user functions using oclPrinter */
  def generateUserFunction(expr: Expr) {
    val userFuns = Expr.visit(Set[UserFunDef]())(expr, (expr,set) =>
      expr match {
        case call: FunCall => call.f match {
            case uf: UserFunDef => set + uf
            //case vec: Vectorize => set + UserFun.vectorize(vec.f.asInstanceOf[UserFun], vec.n)
            case _ => set
          }
        case _ => set
      })
    userFuns.foreach(uf => {
      oclPrinter.print(oclPrinter.toOpenCL(uf))
      oclPrinter.println()
    })
  }

  def allocateMemory(f: Expr) : Unit = {
    OpenCLMemory.alloc(f)
    Kernel.memory = TypedOpenCLMemory.getAllocatedMemory(f)
  }

  private class AccessFunction(val f: (ArithExpr) => ArithExpr, val scope: String) {
    def apply(e: ArithExpr): ArithExpr = f(e)
  }

  private object AccessFunction {
    def apply(f: (ArithExpr) => ArithExpr, scope: String) = new AccessFunction(f, scope)

    def id(scope: String) = new AccessFunction( (e:ArithExpr) => e, scope)
  }

  private class MapAccessFunction(val loopVar: Var, val chunkSize: ArithExpr, val mapName: String)
    extends AccessFunction( (i:ArithExpr) => (loopVar * chunkSize) + i, mapName )

  private object MapAccessFunction {
    def apply(loopVar: Var, initialChunkSize: ArithExpr, mapName: String) =
      new MapAccessFunction(loopVar, initialChunkSize, mapName)

    def multWithChunkSize(ma: MapAccessFunction, chunkSize: ArithExpr): MapAccessFunction = {
      MapAccessFunction(ma.loopVar, ma.chunkSize * chunkSize, ma.mapName)
    }

    def asMapAccessFunction(a: AccessFunction): MapAccessFunction = {
      a match {
        case ma: MapAccessFunction => ma
        case _ => throw new IllegalArgumentException
      }
    }
  }


  private def generateKernel(f: Lambda, workGroupSize: Int = 128) {
    val expr = f.body

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
    val varMap = iterateVars.map( v => (v, ArithExpr.asCst(v.range.max)) ).toMap
    Kernel.memory.map( mem => {
      val m = mem.mem
      if (Debug()) {
        println("Allocate " + ArithExpr.substitute(m.size, varMap.toMap) + " bytes for variable " + m.variable + " in " +
          m.addressSpace + " memory")
      }
    })

    // generate the body of the kernel
    oclPrinter.openCB()
    generate(expr, Array.empty[AccessFunction], Array.empty[AccessFunction])
    oclPrinter.closeCB()
  }
  
  object Kernel {
    var memory = Array.empty[TypedOpenCLMemory]
    var workGroupSize = 128
  }
  
  private def generate(expr: Expr, inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    assert(expr.outT != UndefType)

    //val inputAccess = updateInputAccessFunction(oldInputAccess, f.inT)
    expr match {
      case call: FunCall => call.f match {
        // sequential composition of functions. Allow to pass access functions horizontally.
        // go from right to left, as the data flows ...
        case cf: CompFunDef => cf.funs.foldRight[Option[AccessFunction]](None)(
          (lambda: Lambda, af: Option[AccessFunction]) => lambda.body match {
            case call: FunCall => call.f match {
              // pass newly created access function to the next function in line
              case r : ReorderStride => Some(createReorderStrideAccessFunction(r, call, inputAccess.last.scope))

              case _ =>
                if (af.isDefined) {
                  generate(lambda.body, inputAccess :+ af.get, outputAccess)
                } else {
                  generate(lambda.body, inputAccess, outputAccess)
                }
                None // af is consumed and not passed to the next function in line
            }
          })
        // case cf: CompFun => cf.funs.reverseMap(inF => generate(inF, inputAccess, outputAccess))
        // maps
        case m: MapWrg => generateMapWrgCall(m, call, inputAccess, outputAccess)
        case m: MapLcl => generateMapLclCall(m, call, inputAccess, outputAccess)
        case m: MapWarp => generateMapWarpCall(m, call, inputAccess, outputAccess)
        case m: MapLane => generateMapLaneCall(m, call, inputAccess, outputAccess)
        case m: MapSeq => generateMapSeqCall(m, call, inputAccess, outputAccess)
        // reduce
        case r: ReduceSeq => generateReduceSeqCall(r, call, inputAccess, outputAccess)
        case r: ReduceHost => generateReduceSeqCall(r, call, inputAccess, outputAccess)
        // iterate
        case i: Iterate => generateIterateCall(i, call.asInstanceOf[IterateCall], inputAccess, outputAccess)
        // user functions
        case u : UserFunDef =>generateUserFunCall(u, call, inputAccess, outputAccess)
        // utilities
        case f: toGlobal => generate(f.f.body, inputAccess, outputAccess)
        case f: toLocal => generate(f.f.body, inputAccess, outputAccess)
        case l: Lambda => generate(l.body, inputAccess, outputAccess)
        case _: asVector =>
        case _: asScalar =>
        case _: Split =>
        case _: Join =>
        case _: Zip =>
        case _ => oclPrinter.print("__" + call.toString + "__")
      }
    }
  }

  // === Maps ===
  // generic Map
  private def generateMap(m: AbstractMap, c: FunCall, loopVar: Var, range: RangeAdd,
                          inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction],
                          mapName: String) {
    val inAccessFun = MapAccessFunction(loopVar, Cst(1), mapName)
    val updatedInputAccess = updateAccessFunction(inputAccess, Type.length(c.inT).head) :+ inAccessFun

    // output
    val outAccessFun = MapAccessFunction(loopVar, Cst(1), mapName)
    val updatedOutputAccess = updateAccessFunction(outputAccess, Type.length(c.outT).head) :+ outAccessFun

    oclPrinter.generateLoop(loopVar, range, () => generate(m.f.body, updatedInputAccess, updatedOutputAccess))
  }
  
  // MapWrg
  private def generateMapWrgCall(m: MapWrg, c: FunCall,
                                 inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    assert(c.f == m)

    val range = RangeAdd(new get_group_id(0), Type.getLength(c.inT), new get_num_groups(0))
    val loopVar = Var("g_id", range)

    generateMap(m, c, loopVar, range, inputAccess, outputAccess, "MapWrg")
    oclPrinter.println("return;")
  }
  
  // MapLcl
  private def generateMapLclCall(m: MapLcl, c: FunCall,
                                 inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    assert(c.f == m)

    val range = RangeAdd(new get_local_id(0), Type.getLength(c.inT), Cst(Kernel.workGroupSize))
    val loopVar = Var("l_id", range)

    generateMap(m, c, loopVar, range, inputAccess, outputAccess, "MapLcl")
    oclPrinter.generateBarrier(c.outM)
  }

  // MapWarp
  private def generateMapWarpCall(m: MapWarp, c: FunCall,
                                  inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    assert(c.f == m)

    val range = RangeAdd(new get_local_id(0) / OpenCL.warpSize,
                         Type.getLength(c.inT),
                         Cst(Kernel.workGroupSize) / OpenCL.warpSize)
    val loopVar = Var("warp_id", range)

    generateMap(m, c, loopVar, range, inputAccess, outputAccess, "MapWarp")
    oclPrinter.generateBarrier(c.outM)
  }

  // MapLane
  private def generateMapLaneCall(m: MapLane, c: FunCall,
                                  inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    assert(c.f == m)

    val range = RangeAdd(new get_local_id(0) & (OpenCL.warpSize - Cst(1)), Type.getLength(c.inT), OpenCL.warpSize)
    val loopVar = Var("lane_id", range)

    generateMap(m, c, loopVar, range, inputAccess, outputAccess, "MapLane")
  }
  
  // MapSeq
  private def generateMapSeqCall(m: MapSeq, c: FunCall,
                             inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    assert(c.f == m)

    val range = ContinousRange(Cst(0), Type.getLength(c.inT))
    val loopVar = Var("i", range)

    // input
    val inChunkSizes = Type.length(c.inT).reduce(_ * _) // this includes the vector size
    val updatedInputAccess = updateAccessFunction(inputAccess, inChunkSizes)

    // output
    val outChunkSizes = Type.length(c.outT).reduce(_ * _)  // this includes the vector size
    val updatedOutputAccess = updateAccessFunction(outputAccess, outChunkSizes)

    oclPrinter.commln("map_seq")
    oclPrinter.generateLoop(loopVar, range, () => {
      val innermostAccess = AccessFunction( (_) => loopVar * Type.getVectorSize(m.f.body.inT), "MapSeq")

      // output[i] = f(input[i])
      oclPrinter.print(access(c.outM, m.f.body.outT, updatedOutputAccess :+ innermostAccess) + " = ")

      generate(m.f.body, updatedInputAccess :+ innermostAccess, updatedOutputAccess)

      oclPrinter.println(";")
    })
    oclPrinter.commln("map_seq")

  }
  
  // === Reduce ===
  private def generateReduceSeqCall(r: AbstractReduce, c: FunCall,
                                    inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) {
    assert(c.f == r)

    val initT = c.inT match { case tt: TupleType => tt.elemsT(0) }
    val inT = c.inT match { case tt: TupleType => tt.elemsT(1) }

    // input
    val inChunkSizes = Type.length(inT).reduce(_ * _) // this includes the vector size
    val updatedInputAccess = updateAccessFunction(inputAccess, inChunkSizes)

    // output
    val outChunkSizes = Type.length(c.outT).reduce(_ * _)  // this includes the vector size
    val updatedOutputAccess = updateAccessFunction(outputAccess, outChunkSizes)

    oclPrinter.openCB()
    oclPrinter.commln("reduce_seq")

    // 1. generate: int acc = 0
    val accVar = c.inM match { case coll: OpenCLMemoryCollection => coll.subMemories(0).variable }
    val accType = initT //r.init.outT
    val accValue = c.args(0) match { case v: Value => v.value }
    oclPrinter.printVarDecl(accType, accVar, accValue)
    oclPrinter.println(";")

    // 2. generate loop from 0 .. length
    val range = RangeAdd(Cst(0), Type.getLength(inT), Cst(1))
    val loopVar = Var("i", range)
    oclPrinter.generateLoop(loopVar, range, () => {
      // 3. generate acc = fun(acc, input[i])
      oclPrinter.print(oclPrinter.toOpenCL(accVar) + " = ")

      val t = r.f.body.inT match { case tt: TupleType => tt.elemsT(1) } // type of the second argument
      val innermostInputAccess = AccessFunction( (_) => loopVar * Type.getVectorSize(t), "ReduceSeq")

      generate(r.f.body, updatedInputAccess :+ innermostInputAccess, updatedOutputAccess)

      // oclPrinter.generateFunCall(r.f.body, oclPrinter.toOpenCL(acc), access(c.inM, t, updatedInputAccess, loopVar))

      oclPrinter.println(";")
    })

     // 4. generate output[0] = acc
     oclPrinter.println(access(c.outM, r.f.body.outT, updatedOutputAccess) =:= oclPrinter.toOpenCL(accVar))
     oclPrinter.commln("reduce_seq")
     oclPrinter.closeCB()
  }

  // === Iterate ===
  private def generateIterateCall(i: Iterate, c: IterateCall,
                                  inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) = {

    assert(c.f == i)

    val inputMem = OpenCLMemory.asOpenCLMemory(c.inM)
    val outputMem = OpenCLMemory.asOpenCLMemory(c.outM)
    val swapMem = OpenCLMemory.asOpenCLMemory(c.swapBuffer)

    assert (inputMem.addressSpace == outputMem.addressSpace)

    val innerInputLength = Type.getLength(i.f.body.inT)
    val innerOutputLength = Type.getLength(i.f.body.outT)

    oclPrinter.openCB()

    // use the type var as the var holding the iterating size if it exists
    if (TypeVar.getTypeVars(i.f.body.inT).size > 1) {
      println("size: " + TypeVar.getTypeVars(i.f.body.inT).size)
      TypeVar.getTypeVars(i.f.body.inT).map( (tv) => { println("tv: " + tv) })
      println("i.f.inT " + i.f.body.inT)
      throw new NotImplementedError()
    }
    val curOutLen =
      if (TypeVar.getTypeVars(i.f.body.inT).isEmpty)
        Var("curOutLen")
      else
        TypeVar.getTypeVars(i.f.body.inT).head
    oclPrinter.printVarDecl(opencl.ir.Int, curOutLen, oclPrinter.toOpenCL(Type.getLength(c.inT)))
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
    oclPrinter.println(outputMem.addressSpace + " " + oclPrinter.toOpenCL(Type.devectorize(c.outT)) + " " + tinVStr + " = " + inVStr+";")

    // ADDRSPC TYPE tin = (odd ? out : swap);
    oclPrinter.print(outputMem.addressSpace + " " + oclPrinter.toOpenCL(Type.devectorize(c.outT)) + " " + toutVStr + " = ")
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
      generate(i.f.body, inputAccess, outputAccess)

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

  private def generateUserFunCall(u: UserFunDef, call: FunCall,
                                  inputAccess: Array[AccessFunction], outputAccess: Array[AccessFunction]) = {
    assert(call.f == u)

    oclPrinter.generateFunCall(call, access(call.inM, call.inT, inputAccess))
  }

  // === ReorderStride ===
  private def createReorderStrideAccessFunction(r: ReorderStride, call: FunCall, scope: String) = {
    assert (call.f == r)

    val s = Type.getLength(call.inT)
    val n = Type.getLength(Type.getElemT(call.inT))
    AccessFunction( (i:ArithExpr) => { i / n + s * (i % n) } , scope)
  }

  // === Utilities ===

  private def updateAccessFunction(access: Array[AccessFunction], chunkSize: ArithExpr) = {
    access.map( {
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

  private def access(memory: Memory, t: Type, accessFunctions: Array[AccessFunction]): String = {
    val oclMem = OpenCLMemory.asOpenCLMemory(memory)

    println( "mem is coll? " + oclMem.isInstanceOf[OpenCLMemoryCollection] + "; t " + t )

    oclMem match {
      case coll: OpenCLMemoryCollection =>
        t match {
          case tt: TupleType =>
            assert(tt.elemsT.length == coll.subMemories.length)

            (coll.subMemories zip tt.elemsT).map( { case (m, ty) => access(m, ty, accessFunctions) } ).reduce(_ + ", " + _)
        }

      case _ =>
        oclMem.addressSpace match {
          case GlobalMemory =>
            "*((global " + oclPrinter.toOpenCL(t) + "*)&" +
              oclPrinter.toOpenCL(oclMem.variable) +
              "[" + oclPrinter.toOpenCL(accessFunctions.foldRight[ArithExpr](Cst(0))((aF, i) => {
              aF(i)
            })) + "])"

          case LocalMemory =>
            // access function from the kernel or MapWrg scope should not affect local
            val localAccessFunctions = accessFunctions.filter((a) => {
              (a.scope != "MapWrg") && (a.scope != "Kernel")
            })
            "*((local " + oclPrinter.toOpenCL(t) + "*)&" +
              oclPrinter.toOpenCL(oclMem.variable) +
              "[" + oclPrinter.toOpenCL(localAccessFunctions.foldRight[ArithExpr](Cst(0))((aF, i) => {
              aF(i)
            })) + "])"

          case PrivateMemory =>
            oclPrinter.toOpenCL(oclMem.variable)

          case _ => throw new NotImplementedError()
        }
    }
  }



}