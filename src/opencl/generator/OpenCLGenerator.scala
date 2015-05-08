package opencl.generator

import arithmetic._
import generator.Generator
import ir._
import ir.view.{ViewPrinter, View}
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
  var debug = false
  def apply() = debug
  def apply(debug: Boolean) = { this.debug = debug }
}

object Verbose {
  var verbose = true
  def apply() = verbose
  def apply(verbose: Boolean) = { this.verbose = verbose }
}

object AllocateStatically{
  var allocateStatically = true
  def apply() = allocateStatically
  def apply(allocateStatically: Boolean) = { this.allocateStatically = allocateStatically }
}

object OpenCL{
  val warpSize = 32
}

object OpenCLGenerator extends Generator {

  var oclPrinter: OpenCLPrinter = null

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
  def generate(f: Lambda, localSize: Array[ArithExpr], globalSize: Array[ArithExpr], valueMap: immutable.Map[ArithExpr, ArithExpr]): String = {

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
      p.view = View(p.t, oclPrinter.toOpenCL(p.mem.variable))
    })

    // pass 1
    allocateMemory(f)

    RangesAndCounts(f, localSize, globalSize, valueMap)

    BarrierElimination(f)

    if (Verbose()) {
      println("Memory:")
      printMemories(f.body)
    }

    if (Verbose()) {
      println("Allocated Memory:")
      TypedOpenCLMemory.getAllocatedMemory(f.body, f.params).foreach(m => println(m.toString))
      println("")
    }

    View.visitAndBuildViews(f.body)

    // pass 2: find and generate user and group functions
    generateUserFunction(f.body)
    generateGroupFunction(f.body)

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

  /** Traverses f and print all group functions using oclPrinter */
  def generateGroupFunction(expr: Expr) {
    val groupFuns = Expr.visit(Set[Group]())(expr, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case group: Group => set + group
          case _ => set
        }
        case _ => set
      })
    groupFuns.foreach(group => {
      oclPrinter.print(oclPrinter.toOpenCL(group))
      oclPrinter.println()
    })
  }

  def allocateMemory(f: Lambda): Unit = {
    OpenCLMemory.alloc(f.body)
    Kernel.memory = TypedOpenCLMemory.getAllocatedMemory(f.body, f.params)
  }

  private def isFixedSizeLocalMemory: (TypedOpenCLMemory) => Boolean = {
    mem => try {
      mem.mem.size.eval()
      mem.mem.addressSpace == LocalMemory
    } catch {
      case _: NotEvaluableException =>
        false
    }
  }

  object Kernel {
    var memory = Array.empty[TypedOpenCLMemory]
    var workGroupSize = 128
  }

  private def generateKernel(f: Lambda, workGroupSize: Int = 128) {
    val expr = f.body

    Kernel.workGroupSize = workGroupSize

    // generate kernel function signature
    oclPrinter.print("kernel void KERNEL(")

    val privateMems = TypedOpenCLMemory.
      getAllocatedMemory(f.body, f.params, includePrivate = true).diff(Kernel.memory)

    val (staticLocal, rest) =
      if (AllocateStatically())
        Kernel.memory.partition(isFixedSizeLocalMemory)
      else (Array.empty[TypedOpenCLMemory], Kernel.memory)

    Kernel.memory = rest

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
    // Print declarations for non parameter private memories and fixed size local memories
    staticLocal.foreach(oclPrinter.printVarDecl)
    privateMems.foreach(m => oclPrinter.printVarDecl(Type.getValueType(m.t), m.mem.variable))
    generate(expr)
    oclPrinter.closeCB()
  }



  private def generate(expr: Expr) {
    assert(expr.t != UndefType)

    expr match {
      case f: FunCall => f.args.foreach(generate)
      case _ =>
    }

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
//      case call: ConcreteHeadCall => generateConcreteHeadCall(call)
//      case call: HeadCall => generateHeadCall(call)
//      case call: TailCall => generateTailCall(call)

      case call: FunCall => call.f match {
        case cf: CompFunDef => cf.funs.reverseMap( (l:Lambda) => generate(l.body) )

        case u : UserFunDef =>generateUserFunCall(u, call)

        case fp: FPattern => generate(fp.f.body)
        case l: Lambda => generate(l.body)
        case b : Barrier => if (b.valid) oclPrinter.generateBarrier(call.mem)
        case Unzip() | ReorderStride(_) | Transpose() | TransposeW() | asVector(_) | asScalar() |
             Split(_) | Join() | Group(_,_,_) | Zip(_) | Tuple(_) | Filter() =>
//        case _: VTail =>
        case Head() | Tail() =>
        case _ => oclPrinter.print("__" + call.toString + "__")
      }
      case v: Value => generateValue(v)
      case p: Param =>
    }
  }

  // === Maps ===

  // MapWrg
  private def generateMapWrgCall(call: MapCall): Unit = {

    oclPrinter.generateLoop(call.loopVar, () => generate(call.f.f.body), call.iterationCount)
    // TODO: This assumes, that the MapWrg(0) is always the outermost and there is no need for synchronization inside.
    // TODO: Rethink and then redesign this!
    // if (m.dim == 0) {
    //  oclPrinter.println("return;")
    // }
  }

  // MapGlb
  private def generateMapGlbCall(call: MapCall): Unit = {

    oclPrinter.generateLoop(call.loopVar, () => generate(call.f.f.body), call.iterationCount)
    // TODO: This assumes, that the MapGlb(0) is always the outermost and there is no need for synchronization inside.
    // TODO: Rethink and then redesign this!
    // if (m.dim == 0) {
    //  oclPrinter.println("return;")
    // }
  }

  // MapLcl
  private def generateMapLclCall(call: MapCall) {
    oclPrinter.generateLoop(call.loopVar, () => generate(call.f.f.body), call.iterationCount)
  }

  // MapWarp
  private def generateMapWarpCall(call: MapCall) {
    oclPrinter.generateLoop(call.loopVar, () => generate(call.f.f.body), call.iterationCount)
    oclPrinter.generateBarrier(call.mem)
  }

  // MapLane
  private def generateMapLaneCall(call: MapCall) {
    oclPrinter.generateLoop(call.loopVar, () => generate(call.f.f.body), call.iterationCount)
  }

  // MapSeq
  private def generateMapSeqCall(call: MapCall) {
    oclPrinter.commln("map_seq")
    oclPrinter.generateLoop(call.loopVar, () => generate(call.f.f.body), call.iterationCount)
    oclPrinter.commln("map_seq")
  }

  // MapMatrix
//  private def generateMapMatrixCall(call: MapCall) {
//    val m = call.f.asInstanceOf[MapMatrix]
//    val range = RangeAdd(new get_global_id(m.dim), Type.getLength(call.arg.t), new get_global_size(m.dim))
//
//    oclPrinter.generateLoop(call.loopVar, range, () => generate(call.f.f.body))
//    // TODO: This assumes, that the MapGlb(0) is always the outermost and there is no need for synchronization inside.
//    // TODO: Rethink and then redesign this!
//    if (m.dim == 0) {
//      oclPrinter.println("return;")
//    }
//  }
  
  // === Reduce ===
  private def generateReduceSeqCall(call: ReduceCall) {

    oclPrinter.openCB()
    oclPrinter.commln("reduce_seq")

    oclPrinter.generateLoop(call.loopVar, () => generate(call.f.f.body), call.iterationCount)
    //get local copies of our accumulation variables from our ReduceCall
    //print an OpenCL/C declaration for our variable

    oclPrinter.commln("reduce_seq")
    oclPrinter.closeCB()
  }

  private def generateValue(v: Value): Unit = {
    val temp = Var("")
    oclPrinter.printVarDecl(Type.getValueType(v.t), temp, v.value)
    oclPrinter.println(oclPrinter.toOpenCL(v.mem.variable) + " = " + oclPrinter.toOpenCL(temp) + ";")
  }

//    // === ConcreteHead ===
//
//    private def generateConcreteHeadCall(call: ConcreteHeadCall) {
//      val outET = call.t match { case aT: ArrayType => aT.elemT }
//      val argET = call.arg.t match { case aT: ArrayType => aT.elemT }
//
//      oclPrinter.commln("head")
//      // want to generate: output[0] = input[0]
//      oclPrinter.println(access(call.mem, outET, call.view) =:= access(call.arg.mem, argET, call.arg.view))
//      oclPrinter.commln("head")
//    }

//  // === Head ===
//
//  private def generateHeadCall(call: HeadCall) {
//    val outET = call.t match { case aT: ArrayType => aT.elemT }
//    val argET = call.arg.t match { case aT: ArrayType => aT.elemT }
////    oclPrinter.openCB()
//    oclPrinter.commln("head")
//    // want to generate: output[0] = input[0]
//    oclPrinter.println(access(call.mem, outET, call.view) =:= access(call.arg.mem, argET, call.arg.view))
//    oclPrinter.commln("head")
////    oclPrinter.closeCB()
//  }
//
//  // === Tail ===
//
//  private def generateTailCall(call: TailCall) {
////    val outET = call.t match { case aT: ArrayType => aT.elemT }
////    val argET = call.arg.t match { case aT: ArrayType => aT.elemT }
//////    oclPrinter.openCB()
////    oclPrinter.commln("tail")
////    val range = ContinuousRange(Cst(0), Type.getLength(call.t))
////
////    val av = call.view match { case av: ArrayView => av }
////
////    oclPrinter.generateLoop(call.loopVar, range, () =>
////      oclPrinter.println(access(call.mem, outET, av.access(call.loopVar)) =:= access(call.arg.mem, argET, call.arg.view))
////    )
////
////    // want to generate: output[0] = input[0]
////    oclPrinter.commln("tail")
//////    oclPrinter.closeCB()
//  }

  // === Iterate ===
  private def generateIterateCall(call: IterateCall) = {

    val inputMem = OpenCLMemory.asOpenCLMemory(call.arg.mem)
    val outputMem = OpenCLMemory.asOpenCLMemory(call.mem)
    val swapMem = OpenCLMemory.asOpenCLMemory(call.swapBuffer)

    println(inputMem.addressSpace)
    println(outputMem.addressSpace)

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

    // create new temporary input and output pointers
    val tin = Var("tin")
    val tout = Var("tout")

    val tinVStr = oclPrinter.toOpenCL(tin)
    val toutVStr = oclPrinter.toOpenCL(tout)
    val inVStr = oclPrinter.toOpenCL(inputMem.variable)
    val outVStr = oclPrinter.toOpenCL(outputMem.variable)
    val swapVStr = oclPrinter.toOpenCL(swapMem.variable)

    val range = call.indexVar.range.asInstanceOf[RangeAdd]

    // ADDRSPC TYPE tin = in;
    oclPrinter.println(outputMem.addressSpace + " " + oclPrinter.toOpenCL(Type.devectorize(call.t)) + " " + tinVStr + " = " + inVStr+";")

    // ADDRSPC TYPE tin = (odd ? out : swap);
    oclPrinter.print(outputMem.addressSpace + " " + oclPrinter.toOpenCL(Type.devectorize(call.t)) + " " + toutVStr + " = ")
    oclPrinter.print("( ("+oclPrinter.toOpenCL(range.stop)+" & 1) != 0 ) ? ")
    oclPrinter.print(outVStr + " : " + swapVStr)
    oclPrinter.println(" ;")

//    Removed the pragma temporarily as it was causing a (presumably) memory related bug on non NVIDIA and Intel CPU platforms
//    TODO: implement a platform dependent system for inserting the pragma when legal
//    oclPrinter.println("#pragma unroll 1")
    oclPrinter.generateLoop(call.indexVar,() => {

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

  private def access(memory: Memory, t: Type, views: View*): String = {
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