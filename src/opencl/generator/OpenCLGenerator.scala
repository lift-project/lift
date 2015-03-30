package opencl.generator

import generator.Generator
import ir._
import opencl.ir._

import scala.collection.immutable

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
  def apply() = { false }
}

object Verbose {
  var verbose = true
  def apply() = { verbose }
}

object OpenCL{
  val warpSize = 32
}

object OpenCLGenerator extends Generator {

  var oclPrinter: OpenCLPrinter = null
  var localSize: Array[ArithExpr] = Array(?, ?, ?)
  var globalSize: Array[ArithExpr] = Array(?, ?, ?)
  var valueMap: immutable.Map[ArithExpr, ArithExpr] = immutable.Map()

  private def printTypes(expr: Expr): Unit = {
    Expr.visit(expr, (e: Expr) => e match {
      case call: FunCall => println(e + "\n    " + e.t + " <- " + call.argsType + "\n")
      case _ => println(e + "\n    " + e.t + "\n")
    }, (e: Expr) => {})
  }

  private def printMemories(expr: Expr): Unit = {
    Expr.visit(expr, (e: Expr) => e match {
      case call: FunCall => println(e + "\n    " + e.mem.toString + " <- " + call.argsMemory.toString + "\n")
      case _ => println(e + "\n    " + e.mem.toString + "\n")
    }, (f: Expr) => {})
  }

  def generate(f: Lambda): String = {
    generate(f, Array(?, ?, ?))
  }

  def generate(f: Lambda, localSizes: Array[ArithExpr]): String = {
    generate(f, localSizes, Array(?, ?, ?), immutable.Map())
  }

  // Compile a type-checked function into an OpenCL kernel
  def generate(f: Lambda, localSize: Array[ArithExpr], globalSizes: Array[ArithExpr], valueMap: immutable.Map[ArithExpr, ArithExpr]): String = {
    this.localSize = localSize
    this.globalSize = globalSizes
    this.valueMap = valueMap

    assert(localSize.length == 3)
    assert(globalSize.length == 3)
    assert(f.body.t != UndefType)

    if (Verbose()) {
      println("Types:")
      printTypes(f.body)
    }

    oclPrinter = new OpenCLPrinter

    // allocate the params and set the corresponding type
    f.params.foreach((p) => {
      p.t match {
        case _: ScalarType =>
          p.mem = OpenCLMemory.allocPrivateMemory(OpenCLMemory.getMaxSizeInBytes(p.t))
        case _ =>
          p.mem = OpenCLMemory.allocGlobalMemory(OpenCLMemory.getMaxSizeInBytes(p.t))
      }
      p.view = InputView(p.t, oclPrinter.toOpenCL(p.mem.variable))
    })

    // pass 1
    allocateMemory(f)

    if (Verbose()) {
      println("Memory:")
      printMemories(f.body)
    }

    if (Verbose()) {
      println("Allocated Memory:")
      TypedOpenCLMemory.getAllocatedMemory(f.body, f.params).foreach(m => println(m.toString))
      println("")
    }

    InputView.visitAndBuildViews(f.body)

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
    Kernel.memory.foreach(mem => {
      val m = mem.mem
      if (Verbose()) {
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
    assert(expr.t != UndefType)

    expr match {
      case call: MapCall => call.f match {
        case _: MapWrg => generateMapWrgCall(call)
        case _: MapGlb => generateMapGlbCall(call)
        case _: MapLcl => generateMapLclCall(call)
        case _: MapWarp => generateMapWarpCall(call)
        case _: MapLane => generateMapLaneCall(call)
        case _: MapSeq => generateMapSeqCall(call)
        case _: Map =>
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
        case g: Gather => generate(g.f.body)
        case s: Scatter => generate(s.f.body)
        case _: ReorderStride =>
        case _: Transpose =>
        case _: TransposeW =>
        case _: Swap =>
        case _: asVector =>
        case _: asScalar =>
        case _: Split =>
        case _: SplitDim2 =>
        case _: Join =>
        case _: JoinDim2 =>
        case _: Zip => call.args.foreach(generate)
        case _: Unzip =>
        case _ => oclPrinter.print("__" + call.toString + "__")
      }
      case p: Param =>
    }
  }

  // === Maps ===
  
  // MapWrg
  private def generateMapWrgCall(call: MapCall): Unit = {
    val m = call.f.asInstanceOf[MapWrg]
    val dim: Int = m.dim
    val start: get_group_id = new get_group_id(dim)
    val length: ArithExpr = Type.getLength(call.arg.t)
    var step: ArithExpr = new get_num_groups(m.dim)

    val gSize = globalSize(dim)
    val lSize = localSize(dim)

    gSize match {
      case Cst(c) =>
        val numGroups = gSize / lSize
        val vars = Var.getVars(length)
        vars.foreach(v => {
          valueMap.get(v) match {
            case Some(value) => v.range = GoesToRange(value)
            case None =>
          }
        })
        step = numGroups
        start.range = ContinuousRange(0, numGroups)
      case ? =>
    }

    val range = RangeAdd(start, length, step)

    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
    // TODO: This assumes, that the MapWrg(0) is always the outermost and there is no need for synchronization inside.
    // TODO: Rethink and then redesign this!
    // if (m.dim == 0) {
    //  oclPrinter.println("return;")
    // }
  }

  // MapGlb
  private def generateMapGlbCall(call: MapCall): Unit = {
    val m = call.f.asInstanceOf[MapGlb]
    val range = RangeAdd(new get_global_id(m.dim), Type.getLength(call.arg.t), new get_global_size(m.dim))



    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
    // TODO: This assumes, that the MapGlb(0) is always the outermost and there is no need for synchronization inside.
    // TODO: Rethink and then redesign this!
    // if (m.dim == 0) {
    //  oclPrinter.println("return;")
    // }
  }
  
  // MapLcl
  private def generateMapLclCall(call: MapCall) {
    val m = call.f.asInstanceOf[MapLcl]
    val dim: Int = m.dim
    val start = new get_local_id(dim)
    val length = Type.getLength(call.arg.t)
    var step: ArithExpr = new get_local_size(dim)

    val size = localSize(dim)
    if (size != ?) {
      step = size
      start.range = ContinuousRange(0, size)
    }

    val range = RangeAdd(start, length, step)

    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
    // TODO: This assumes, that the MapLcl(0) is always the outermost and there is no need for synchronization inside.
    // TODO: Rethink and then redesign this!
    if (dim == 0) {
      oclPrinter.generateBarrier(call.mem)
    }
  }

  // MapWarp
  private def generateMapWarpCall(call: MapCall) {
    val range = RangeAdd(new get_local_id(0) / OpenCL.warpSize,
                         Type.getLength(call.arg.t),
                         Cst(Kernel.workGroupSize) / OpenCL.warpSize)

    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
    oclPrinter.generateBarrier(call.mem)
  }

  // MapLane
  private def generateMapLaneCall(call: MapCall) {
    val range = RangeAdd(new get_local_id(0) & (OpenCL.warpSize - Cst(1)), Type.getLength(call.arg.t), OpenCL.warpSize)

    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
  }
  
  // MapSeq
  private def generateMapSeqCall(call: MapCall) {

    val range = ContinuousRange(Cst(0), Type.getLength(call.arg.t))

    oclPrinter.commln("map_seq")
    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
    oclPrinter.commln("map_seq")
  }
  
  // === Reduce ===
  private def generateReduceSeqCall(call: ReduceCall) {

    oclPrinter.openCB()
    oclPrinter.commln("reduce_seq")

    // 1. generate: int acc = 0
    val accVar = call.arg0.mem.variable
    val accType = call.arg0.t
    val accValue = call.arg0 match { case v: Value => v.value }
    oclPrinter.printVarDecl(accType, accVar, accValue)
    oclPrinter.println(";")

    val inT = call.arg1.t
    val funCall = call.f.f.body match { case call: FunCall => call }

    // 2. generate loop from 0 .. length
    val range = RangeAdd(Cst(0), Type.getLength(inT), Cst(1))
    oclPrinter.generateLoop(call.loopVar, range, () => {
      // 3. generate acc = fun(acc, input[i])
      oclPrinter.print(access(call.arg0.mem, call.arg0.t, call.arg0.view) + " = ")

      // TODO: This assumes a UserFun to be nested here!
      oclPrinter.generateFunCall(funCall, access(funCall.argsMemory, funCall.argsType, funCall.args.map(_.view):_*))

      oclPrinter.println(";")
    })

    // 4. generate output[0] = acc
    oclPrinter.println(access(call.mem, call.f.f.body.t, funCall.view) =:= oclPrinter.toOpenCL(accVar))
    oclPrinter.commln("reduce_seq")
    oclPrinter.closeCB()
  }

  // === Iterate ===
  private def generateIterateCall(call: IterateCall) = {

    val inputMem = OpenCLMemory.asOpenCLMemory(call.arg.mem)
    val outputMem = OpenCLMemory.asOpenCLMemory(call.mem)
    val swapMem = OpenCLMemory.asOpenCLMemory(call.swapBuffer)

    assert (inputMem.addressSpace == outputMem.addressSpace)

    val funCall = call.f.f.body match { case call: FunCall => call }
    val innerInputLength = Type.getLength(funCall.argsType)
    val innerOutputLength = Type.getLength(funCall.t)

    oclPrinter.openCB()

    // use the type var as the var holding the iterating size if it exists
    if (TypeVar.getTypeVars(funCall.argsType).size > 1) {
      println("size: " + TypeVar.getTypeVars(funCall.argsType).size)
      TypeVar.getTypeVars(funCall.argsType).foreach( (tv) => { println("tv: " + tv) })
      println("i.f.inT " + funCall.argsType)
      throw new NotImplementedError()
    }
    val curOutLen =
      if (TypeVar.getTypeVars(funCall.argsType).isEmpty)
        Var("curOutLen")
      else
        TypeVar.getTypeVars(funCall.argsType).head
    oclPrinter.printVarDecl(opencl.ir.Int, curOutLen, oclPrinter.toOpenCL(Type.getLength(call.argsType)))
    oclPrinter.println(";")

    val range = ContinuousRange(Cst(0), call.f.n)
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
    oclPrinter.println(outputMem.addressSpace + " " + oclPrinter.toOpenCL(Type.devectorize(call.t)) + " " + tinVStr + " = " + inVStr+";")

    // ADDRSPC TYPE tin = (odd ? out : swap);
    oclPrinter.print(outputMem.addressSpace + " " + oclPrinter.toOpenCL(Type.devectorize(call.t)) + " " + toutVStr + " = ")
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

      // generate the function call in the body
      generate(funCall)

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

    oclPrinter.print(access(call.mem, call.t, call.view) + " = ")

    oclPrinter.generateFunCall(call, access(call.argsMemory, call.argsType, call.args.map(_.view):_*))

    oclPrinter.println(";")
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

  private def access(memory: Memory, t: Type, views: InputView*): String = {
    val oclMem = OpenCLMemory.asOpenCLMemory(memory)

    oclMem match {
      case coll: OpenCLMemoryCollection =>
        t match {
          case tt: TupleType =>
            assert(tt.elemsT.length == coll.subMemories.length)

            assert(views.length == coll.subMemories.length)
            ((coll.subMemories zip tt.elemsT) zip views).map({
              case ((m, ty), af) => access(m, ty, af)
            } ).reduce(_ + ", " + _)
        }

      case _ =>
        oclMem.addressSpace match {
          case GlobalMemory =>

            "*((global " + oclPrinter.toOpenCL(t) + "*)&" +
              oclPrinter.toOpenCL(oclMem.variable) +
              "[" + oclPrinter.toOpenCL(ViewPrinter.emit(views(0))) + "])"

          case LocalMemory =>

            "*((local " + oclPrinter.toOpenCL(t) + "*)&" +
              oclPrinter.toOpenCL(oclMem.variable) +
              "[" + oclPrinter.toOpenCL(ViewPrinter.emit(views(0))) + "])"

          case PrivateMemory =>
            oclPrinter.toOpenCL(oclMem.variable)

          case _ => throw new NotImplementedError()
        }
    }
  }



}