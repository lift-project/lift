package opencl.generator

import generator.Generator
import ir._
import opencl.ir._

class NotPrintableExpression(msg: String) extends Exception(msg)
class NotI(msg: String) extends Exception(msg)

// hacky class to store function name
class OclFunction(name: String, param: Int) extends ArithExprFunction {def toOCLString = name+"("+param+")"}

class get_global_id(param: Int) extends OclFunction("get_global_id", param)
class get_local_id(param: Int) extends OclFunction("get_local_id", param)
class get_group_id(param: Int) extends OclFunction("get_group_id", param)
class get_num_groups(param: Int) extends OclFunction("get_num_groups", param)
class get_global_size(param: Int) extends OclFunction("get_global_size", param)
class get_local_size(param: Int) extends OclFunction("get_local_size", param)


object Debug {
  def apply() = { true }
}

object OpenCL{
  val warpSize = 32
}

object OpenCLGenerator extends Generator {

  var oclPrinter: OpenCLPrinter = null

  // compiler a type-checked function into an OpenCL kernel
  def generate(f: Lambda): String = {

    assert(f.body.inT != UndefType)

    if (Debug()) {
      println("Types:")
      Expr.visit(f.body, (f: Expr) => {
        println(f + "\n    " + f.outT + " <- " + f.inT + "\n")
      }, (f: Expr) => {})
    }

    // allocate the params and set the corresponding type
    f.params.map((p) => {
      p.outT match {
        case _: ScalarType =>
          p.outM = OpenCLMemory.allocPrivateMemory(OpenCLMemory.getMaxSizeInBytes(p.outT))
        case _ =>
          p.outM = OpenCLMemory.allocGlobalMemory(OpenCLMemory.getMaxSizeInBytes(p.outT))
      }
      p.inAccess = IdAccessFunctions
      p.inT = p.outT
    })

    // pass 1
    allocateMemory(f)

    if (Debug()) {
      println("Memory:")
      Expr.visit(f.body, (f: Expr) => {
        println(f + "\n    " + f.outM.toString + " <- " + f.inM.toString + "\n")
      }, (f: Expr) => {})
    }

    if (Debug()) {
      println("Allocated Memory:")
      TypedOpenCLMemory.getAllocatedMemory(f.body, f.params).map(m => println(m.toString))
      println("")
    }

    AccessFunction.addAccessFunctions(f.body)

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
    val userFuns = Expr.visit(Set[UserFunDef]())(expr, (expr, set) =>
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

  def allocateMemory(f: Lambda): Unit = {
    OpenCLMemory.alloc(f.body)
    Kernel.memory = TypedOpenCLMemory.getAllocatedMemory(f.body, f.params)
  }


  private def generateKernel(f: Lambda, workGroupSize: Int = 128) {
    val expr = f.body

    Kernel.workGroupSize = workGroupSize

    // generate kernel function signature
    oclPrinter.print("kernel void KERNEL(")

    oclPrinter.printAsParameterDecl(Kernel.memory)

    // array of all unique vars (like N, iterSize, etc. )
    val allVars = Kernel.memory.map(mem => Var.getVars(mem.mem.size)).filter(_.nonEmpty).flatten.distinct
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
    val varMap = iterateVars.map(v => (v, ArithExpr.asCst(v.range.max))).toMap
    Kernel.memory.map(mem => {
      val m = mem.mem
      if (Debug()) {
        println("Allocated " + ArithExpr.substitute(m.size, varMap.toMap) + " bytes for variable " +
          oclPrinter.toOpenCL(m.variable) + " in " + m.addressSpace + " memory")
      }
    })

    // generate the body of the kernel
    oclPrinter.openCB()
    generate(expr)
    oclPrinter.closeCB()
  }

  object Kernel {
    var memory = Array.empty[TypedOpenCLMemory]
    var workGroupSize = 128
  }



  private def generate(expr: Expr) {
    assert(expr.outT != UndefType)

    expr match {
      case call: MapCall => call.f match {
        case _: MapWrg => generateMapWrgCall(call)
        case _: MapGlb => generateMapGlbCall(call)
        case _: MapLcl => generateMapLclCall(call)
        case _: MapWarp => generateMapWarpCall(call)
        case _: MapLane => generateMapLaneCall(call)
        case _: MapSeq => generateMapSeqCall(call)
      }
      case call: ReduceCall => call.f match {
        case _: ReduceSeq => generateReduceSeqCall(call)
        case _: ReduceHost => generateReduceSeqCall(call)
      }
      case call: IterateCall => generateIterateCall(call)
      case call: FunCall => call.f match {
        case cf: CompFunDef => cf.funs.reverseMap( (l:Lambda) => generate(l.body) )

        case u : UserFunDef =>generateUserFunCall(u, call)

        case f: toGlobal => generate(f.f.body)
        case f: toLocal => generate(f.f.body)
        case l: Lambda => generate(l.body)
        case _: ReorderStride =>
        case _: asVector =>
        case _: asScalar =>
        case _: Split =>
        case _: Join =>
        case _: Zip => call.args.map(generate)
        case _ => oclPrinter.print("__" + call.toString + "__")
      }
      case p: Param =>
    }
  }

  // === Maps ===
  
  // MapWrg
  private def generateMapWrgCall(call: MapCall): Unit = {
    val range = RangeAdd(new get_group_id(0), Type.getLength(call.inT), new get_num_groups(0))

    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
    oclPrinter.println("return;")
  }

  // MapGlb
  private def generateMapGlbCall(call: MapCall): Unit = {
    val range = RangeAdd(new get_global_id(0), Type.getLength(call.inT), new get_global_size(0))

    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
    oclPrinter.println("return;")
  }
  
  // MapLcl
  private def generateMapLclCall(call: MapCall) {
    val range = RangeAdd(new get_local_id(0), Type.getLength(call.inT), Cst(Kernel.workGroupSize))

    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
    oclPrinter.generateBarrier(call.outM)
  }

  // MapWarp
  private def generateMapWarpCall(call: MapCall) {
    val range = RangeAdd(new get_local_id(0) / OpenCL.warpSize,
                         Type.getLength(call.inT),
                         Cst(Kernel.workGroupSize) / OpenCL.warpSize)

    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
    oclPrinter.generateBarrier(call.outM)
  }

  // MapLane
  private def generateMapLaneCall(call: MapCall) {
    val range = RangeAdd(new get_local_id(0) & (OpenCL.warpSize - Cst(1)), Type.getLength(call.inT), OpenCL.warpSize)

    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
  }
  
  // MapSeq
  private def generateMapSeqCall(call: MapCall) {

    val range = ContinousRange(Cst(0), Type.getLength(call.inT))

    oclPrinter.commln("map_seq")
    oclPrinter.generateLoop(call.loopVar, range, () => {

      oclPrinter.print(access(call.outM, call.f.f.body.outT, call.outAccess) + " = ")

      generate(call.f.f.body)

      oclPrinter.println(";")
    })
    oclPrinter.commln("map_seq")
  }
  
  // === Reduce ===
  private def generateReduceSeqCall(call: ReduceCall) {

    val initT = call.inT match { case tt: TupleType => tt.elemsT(0) }
    val inT   = call.inT match { case tt: TupleType => tt.elemsT(1) }

    oclPrinter.openCB()
    oclPrinter.commln("reduce_seq")

    // 1. generate: int acc = 0
    val accVar = call.inM match { case coll: OpenCLMemoryCollection => coll.subMemories(0).variable }
    val accType = initT //r.init.outT
    val accValue = call.args(0) match { case v: Value => v.value }
    oclPrinter.printVarDecl(accType, accVar, accValue)
    oclPrinter.println(";")

    // 2. generate loop from 0 .. length
    val range = RangeAdd(Cst(0), Type.getLength(inT), Cst(1))
    oclPrinter.generateLoop(call.loopVar, range, () => {
      // 3. generate acc = fun(acc, input[i])
      oclPrinter.print(oclPrinter.toOpenCL(accVar) + " = ")

      generate(call.f.f.body)

      oclPrinter.println(";")
    })

    // 4. generate output[0] = acc
    oclPrinter.println(access(call.outM, call.f.f.body.outT, call.outAccess) =:= oclPrinter.toOpenCL(accVar))
    oclPrinter.commln("reduce_seq")
    oclPrinter.closeCB()
  }

  // === Iterate ===
  private def generateIterateCall(c: IterateCall) = {

    val inputMem = OpenCLMemory.asOpenCLMemory(c.inM)
    val outputMem = OpenCLMemory.asOpenCLMemory(c.outM)
    val swapMem = OpenCLMemory.asOpenCLMemory(c.swapBuffer)

    assert (inputMem.addressSpace == outputMem.addressSpace)

    val innerInputLength = Type.getLength(c.f.f.body.inT)
    val innerOutputLength = Type.getLength(c.f.f.body.outT)

    oclPrinter.openCB()

    // use the type var as the var holding the iterating size if it exists
    if (TypeVar.getTypeVars(c.f.f.body.inT).size > 1) {
      println("size: " + TypeVar.getTypeVars(c.f.f.body.inT).size)
      TypeVar.getTypeVars(c.f.f.body.inT).map( (tv) => { println("tv: " + tv) })
      println("i.f.inT " + c.f.f.body.inT)
      throw new NotImplementedError()
    }
    val curOutLen =
      if (TypeVar.getTypeVars(c.f.f.body.inT).isEmpty)
        Var("curOutLen")
      else
        TypeVar.getTypeVars(c.f.f.body.inT).head
    oclPrinter.printVarDecl(opencl.ir.Int, curOutLen, oclPrinter.toOpenCL(Type.getLength(c.inT)))
    oclPrinter.println(";")

    val range = ContinousRange(Cst(0), c.f.n)
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
      generate(c.f.f.body)

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

  private def generateUserFunCall(u: UserFunDef, call: FunCall) = {
    assert(call.f == u)

    oclPrinter.generateFunCall(call, access(call.inM, call.inT, call.inAccess))
  }

  // === Utilities ===
  
  // helper functions to generate the actual OpenCL code
  private implicit class Operators(v: Any) {
    // generate assignment
    def =:=(rhs: Any) : String = { this + " = " + rhs + ";\n" }

    override def toString : String = v.toString
  }
  
  private def apply(fun: Any, arg: Any*) : String = {
    fun + "(" + arg.reduce( _ + ", " + _) + ")"
  }

  private def access(memory: Memory, t: Type, accessFunctions: AccessFunctions): String = {
    val oclMem = OpenCLMemory.asOpenCLMemory(memory)

    oclMem match {
      case coll: OpenCLMemoryCollection =>
        t match {
          case tt: TupleType =>
            assert(tt.elemsT.length == coll.subMemories.length)

            accessFunctions match {
              case collAf: AccessFunctionsCollection =>
                assert(collAf.elems.length == coll.subMemories.length)
                ((coll.subMemories zip tt.elemsT) zip collAf.elems).map({
                    case ((m, ty), af) => access(m, ty, af)
                  } ).reduce(_ + ", " + _)
/*
              case af: AccessFunctions =>
                println("JUST ONE ACCESS FUNCTION FOR MULTIPLE MEMORY") // TODO: find out why this is necessary
                (coll.subMemories zip tt.elemsT).map({
                  case (m, ty) => access(m, ty, af)
                } ).reduce(_ + ", " + _)
*/
            }
        }

      case _ =>
        t match {
          case _ =>
            oclMem.addressSpace match {
              case GlobalMemory =>
                "*((global " + oclPrinter.toOpenCL(t) + "*)&" +
                  oclPrinter.toOpenCL(oclMem.variable) +
                  "[" + oclPrinter.toOpenCL(accessFunctions.afs.foldRight[ArithExpr](Cst(0))((aF, i) => {
                  aF(i)
                })) + "])"

              case LocalMemory =>
                // access function from the kernel or MapWrg scope should not affect local
                val localAccessFunctions = accessFunctions.afs.filter((a) => {
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



}