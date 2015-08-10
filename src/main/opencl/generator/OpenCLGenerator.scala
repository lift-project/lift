package opencl.generator

import java.io.{BufferedWriter, File, FileWriter}

import apart.arithmetic._
import arithmetic.TypeVar
import generator.Generator
import ir._
import ir.ast._
import ir.view.{View, ViewPrinter}
import opencl.generator.OpenCLAST.OclAstNode
import opencl.ir._
import opencl.ir.pattern._
import scala.collection.immutable

class NotPrintableExpression(msg: String) extends Exception(msg)
class NotI(msg: String) extends Exception(msg)

// hacky class to store function name
class OclFunction(name: String, param: Int) extends ArithExprFunction(name) {
  lazy val toOCLString = s"$name($param)"
}

class get_global_id(param: Int) extends OclFunction("get_global_id", param)
class get_local_id(param: Int) extends OclFunction("get_local_id", param)
class get_group_id(param: Int) extends OclFunction("get_group_id", param)
class get_num_groups(param: Int) extends OclFunction("get_num_groups", param)
class get_global_size(param: Int) extends OclFunction("get_global_size", param)
class get_local_size(param: Int) extends OclFunction("get_local_size", param)


object Debug {
  var debug = System.getenv("APART_DEBUG") != null
  def apply() = debug
  def apply(debug: Boolean) = { this.debug = debug }
}

object Verbose {
  var verbose = System.getenv("APART_VERBOSE") != null
  def apply() = verbose
  def apply(verbose: Boolean) = { this.verbose = verbose }
}

object AllocateLocalMemoryStatically {
  var allocateLocalMemoryStatically = true
  def apply() = allocateLocalMemoryStatically
  def apply(allocateStatically: Boolean) = { this.allocateLocalMemoryStatically = allocateStatically }
}

object OpenCL{
  val warpSize = 32
}

object OpenCLGenerator extends Generator {

  type NDRange = Array[ArithExpr]
  type ValueTable = immutable.Map[ArithExpr, ArithExpr]
  type SymbolTable = immutable.Map[Var, Type]

  var replacements: ValueTable = immutable.Map.empty
  var replacementsWithFuns: ValueTable = immutable.Map.empty
  var privateMems = Array[TypedOpenCLMemory]()

  var varDecls: SymbolTable = immutable.Map.empty

  // Root of the AST
  var ast = new OpenCLAST.Block(List.empty, true)

  // Current block
  var cur_block = ast


  private def printTypes(expr: Expr): Unit = {
    Expr.visit(expr, {
      case e@(call: FunCall) => println(e + "\n    " + e.t + " <- " + call.argsType + "\n")
      case e => println(e + "\n    " + e.t + "\n")
    }, (e: Expr) => {})
  }

  private def printMemories(expr: Expr): Unit = {
    Expr.visit(expr, {
      case e@(call: FunCall) => println(e + "\n    " + e.mem.toString + " <- " + call.argsMemory.toString + "\n")
      case e => println(e + "\n    " + e.mem.toString + "\n")
    }, (f: Expr) => {})
  }

  def generate(f: Lambda): String = {
    generate(f, Array(?, ?, ?))
  }

  def generate(f: Lambda, localSizes: NDRange): String = {
    generate(f, localSizes, Array(?, ?, ?), immutable.Map())
  }

  // Compile a type-checked function into an OpenCL kernel
  def generate(f: Lambda, localSize: NDRange, globalSize: NDRange,
               valueMap: immutable.Map[ArithExpr, ArithExpr]): String = {
    ast = new OpenCLAST.Block(List.empty, true)
    cur_block = ast

    assert(localSize.length == 3)
    assert(globalSize.length == 3)
    assert(f.body.t != UndefType)

    if (Verbose()) {
      println("Types:")
      printTypes(f.body)
    }

    // allocate the params and set the corresponding type
    f.params.foreach((p) => {
      p.t match {
        case _: ScalarType =>
          p.mem = OpenCLMemory.allocPrivateMemory(OpenCLMemory.getMaxSizeInBytes(p.t))
        case _ =>
          p.mem = OpenCLMemory.allocGlobalMemory(OpenCLMemory.getMaxSizeInBytes(p.t))
      }
      p.view = View(p.t, OpenCLCodeGen.toOpenCL(p.mem.variable))
    })

    RangesAndCounts(f, localSize, globalSize, valueMap)
    allocateMemory(f)
    BarrierElimination(f)

    if (Verbose()) {
      println("Memory:")
      printMemories(f.body)

      println("Allocated Memory:")
      TypedOpenCLMemory.getAllocatedMemory(f.body, f.params).foreach(m => println(m.toString))
      println("")
    }

    View.visitAndBuildViews(f.body)

    // pass 2: find and generate user and group functions
    generateUserFunction(f.body)
    generateGroupFunction(f.body)

    // pass 3: generate the kernel
    generateKernel(f)

    // return the code generated
    OpenCLCodeGen(ast)
  }

  /** Traversals f and print all user functions using oclPrinter */
  def generateUserFunction(expr: Expr): Unit = {
    val userFuns = Expr.visitWithState(Set[UserFun]())(expr, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case uf: UserFun => set + uf
          //case vec: Vectorize => set + UserFun.vectorize(vec.f.asInstanceOf[UserFun], vec.n)
          case _ => set
        }
        case _ => set
      })
    userFuns.foreach(uf => {
      uf.tupleTypes.foreach(tup => {
        cur_block += OpenCLAST.TypeDef(tup)
      })

      val block = OpenCLAST.Block()
      if(uf.tupleTypes.length == 1)
        block += OpenCLAST.TupleAlias(uf.tupleTypes.head, "Tuple")
      else uf.tupleTypes.zipWithIndex.foreach({ case (x, i) =>
        // TODO: think about this one ...
        block += OpenCLAST.TupleAlias(x, s"Tuple$i")
      })
      block += OpenCLAST.Inline(uf.body)

      cur_block += OpenCLAST.Function(
        name = uf.name,
        ret = uf.outT,
        params = (uf.inTs, uf.paramNames).zipped.map((t,n) => OpenCLAST.ParamDecl(n,t)).toList,
        body = block)
    })
  }

  /** Traverses f and print all group functions using oclPrinter */
  def generateGroupFunction(expr: Expr) {
    val groupFuns = Expr.visitWithState(Set[Group]())(expr, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case group: Group => set + group
          case _ => set
        }
        case _ => set
      })

    val lenVar = Var("length")
    val newIdx = Var("newIdx")

    groupFuns.foreach(group => {
      OpenCLAST.Function(
        name = s"groupComp${group.id}",
        ret = Int,
        params = List(
          OpenCLAST.ParamDecl("j", Int),
          OpenCLAST.ParamDecl("i", Int),
          OpenCLAST.ParamDecl(lenVar.toString, Int)
        ),
        body = OpenCLAST.Block(List(OpenCLAST.Inline(
          s"""
             |  // Compute new index
             |  int relIndices[] = {${group.relIndices.deep.mkString(", ")}};
             |  int $newIdx = j + relIndices[i];
             |
             |  // Boundary check
             |  if ($newIdx < 0) {
             |    return ${OpenCLCodeGen.toOpenCL(group.negOutOfBoundsF(newIdx, lenVar))};
             |  } else if ($newIdx >= $lenVar) {
             |    return ${OpenCLCodeGen.toOpenCL(group.posOutOfBoundsF(newIdx - lenVar + 1, lenVar))};
             |  } else {
             |    return $newIdx;
             |  }
         """.stripMargin
        ))))
    })
  }

  def allocateMemory(f: Lambda): Unit = {
    OpenCLMemory.alloc(f.body)
    Kernel.memory = TypedOpenCLMemory.getAllocatedMemory(f.body, f.params)
  }

  private def isFixedSizeLocalMemory: (TypedOpenCLMemory) => Boolean = {
    mem => try {
      mem.mem.size.eval
      mem.mem.addressSpace == LocalMemory
    } catch {
      case _: NotEvaluableException =>
        false
    }
  }

  object Kernel {
    var memory = Array.empty[TypedOpenCLMemory]
    var staticLocalMemory = Array.empty[TypedOpenCLMemory]
    var workGroupSize = 128
  }

  private def generateKernel(f: Lambda, workGroupSize: Int = 128): Unit = {
    val expr = f.body

    Kernel.workGroupSize = workGroupSize

    val valMems = Expr.visitWithState(Set[Memory]())(expr, (expr, set) =>
      expr match {
        case value: Value => set + value.mem
        case _ => set
      })

    val (typedValueMems, privateMems) = TypedOpenCLMemory.
      getAllocatedMemory(f.body, f.params, includePrivate = true).diff(Kernel.memory).
      partition(m => valMems.contains(m.mem))

    this.privateMems = privateMems

    // the base type is used for allocation of all variables ...
    this.varDecls = TypedOpenCLMemory.getAllocatedMemory(f.body, f.params, includePrivate = true)
                      .map(tm => (tm.mem.variable, Type.devectorize(tm.t))).toMap

    // ... besides the these variables which use the value types (i.e., possibly a vector type)
    this.varDecls = this.varDecls ++
                    typedValueMems.map(tm => (tm.mem.variable, tm.t)).toMap

    val partitioned =
      if (AllocateLocalMemoryStatically())
        Kernel.memory.partition(isFixedSizeLocalMemory)
      else
        (Array.empty[TypedOpenCLMemory], Kernel.memory)

     partitioned match {
      case (static, nonStatic) =>
        Kernel.memory = nonStatic
        Kernel.staticLocalMemory = static
    }

    f.params.foreach(_.mem.readOnly = true)

    // array of all unique vars (like N, iterSize, etc. )
    val allVars = Kernel.memory.map(mem => mem.mem.size.varList).filter(_.nonEmpty).flatten.distinct
    // partition into iteration variables and all others variables
    val (iterateVars, vars) = allVars.partition(_.name == Iterate.varName)

    // Create the actual kernel function
    val knode = OpenCLAST.Function(
      name = "KERNEL",
      ret = UndefType, // = void
      params =
        Kernel.memory.map(x =>
            OpenCLAST.ParamDecl(
              x.mem.variable.toString, Type.devectorize(x.t),
              const = x.mem.readOnly,
              addressSpace = x.mem.addressSpace
            )
        ).toList ++
        vars.map(x => OpenCLAST.ParamDecl(x.toString, Int)), // size parameters
      body = OpenCLAST.Block(List()),
      kernel = true)
    cur_block += knode
    cur_block = knode.body

    // print out allocated memory sizes
    val varMap = iterateVars.map(v => (v, ArithExpr.asCst(v.range.max))).toMap
    Kernel.memory.foreach(mem => {
      val m = mem.mem
      if (Verbose()) {
        println("Allocated " + ArithExpr.substitute(m.size, varMap.toMap) + " bytes for variable " +
          OpenCLCodeGen.toOpenCL(m.variable) + " in " + m.addressSpace + " memory")
      }
    })

    cur_block += OpenCLAST.Comment("Static local memory")
    Kernel.staticLocalMemory.foreach(x =>
      cur_block += OpenCLAST.VarDecl(x.mem.variable.toString, x.t,
        addressSpace = x.mem.addressSpace,
        length = (x.mem.size /^ Type.getSize(Type.getBaseType(x.t))).eval))
    cur_block += OpenCLAST.Comment("Typed Value memory")
    typedValueMems.foreach(x =>
      cur_block += OpenCLAST.VarDecl(x.mem.variable.toString, Type.getValueType(x.t),
        addressSpace = x.mem.addressSpace))
    cur_block += OpenCLAST.Comment("Private Memory")
    privateMems.foreach(x =>
      cur_block += OpenCLAST.VarDecl(x.mem.variable.toString, x.t,
        addressSpace = x.mem.addressSpace,
        length = (x.mem.size /^ Type.getSize(Type.getBaseType(x.t))).eval))

    generate(expr)
  }



  private def generate(expr: Expr): Unit = {
    assert(expr.t != UndefType)

    expr match {
      case f: FunCall => f.args.foreach(generate)
      case _ =>
    }

    expr match {
      case call: FunCall => call.f match {
        case m: MapWrg => generateMapWrgCall(m, call)
        case m: MapGlb => generateMapGlbCall(m, call)
        case m: MapLcl => generateMapLclCall(m, call)
        case m: MapWarp => generateMapWarpCall(m, call)
        case m: MapLane => generateMapLaneCall(m, call)
        case m: MapSeq => generateMapSeqCall(m, call)
        case _: Map =>

        case r: ReduceSeq => generateReduceSeqCall(r, call)

        case i: Iterate => generateIterateCall(i, call)

        case u : UserFun =>generateUserFunCall(u, call)

        case fp: FPattern => generate(fp.f.body)
        case l: Lambda => generate(l.body)
        case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
             Split(_) | Join() | Group(_,_,_) | Zip(_) | Tuple(_) | Filter() |
             Head() | Tail() | Scatter(_) | Gather(_) | Epsilon() | Get(_) =>

        //case _ => oclPrinter.print("__" + call.toString + "__")
      }
      case v: Value => generateValue(v)
      case p: Param =>
    }
  }

  // === Maps ===

  // MapWrg
  private def generateMapWrgCall(m : MapWrg, call: FunCall): Unit = {
    generateLoop(m.loopVar, () => generate(m.f.body), m.iterationCount)
    // TODO: This assumes, that the MapWrg(0) is always the outermost and there is no need for synchronization inside.
    // TODO: Rethink and then redesign this!
    // if (m.dim == 0) {
    //  oclPrinter.println("return;")
    // }
  }

  // MapGlb
  private def generateMapGlbCall(m: MapGlb, call: FunCall): Unit = {
    generateLoop(m.loopVar, () => generate(m.f.body), m.iterationCount)
    // TODO: This assumes, that the MapGlb(0) is always the outermost and there is no need for synchronization inside.
    // TODO: Rethink and then redesign this!
    // if (m.dim == 0) {
    //  oclPrinter.println("return;")
    // }
  }

  // MapLcl
  private def generateMapLclCall(m: MapLcl, call: FunCall): Unit = {
    generateLoop(m.loopVar, () => generate(m.f.body), m.iterationCount,
      (   OpenCLMemory.containsPrivateMemory(call.args.head.mem)
       && privateMems.exists(_.mem == call.args.head.mem)) || // Don't unroll just for value
        OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == PrivateMemory)

    if (m.emitBarrier)
      cur_block += OpenCLAST.Barrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // MapWarp
  private def generateMapWarpCall(m: MapWarp, call: FunCall): Unit = {
    generateLoop(m.loopVar, () => generate(m.f.body), m.iterationCount)
    call.mem match {
      case m : OpenCLMemory => cur_block += OpenCLAST.Barrier(m)
      case _ =>
    }
  }

  // MapLane
  private def generateMapLaneCall(m: MapLane, call: FunCall): Unit = {
    generateLoop(m.loopVar, () => generate(m.f.body), m.iterationCount)
  }

  // MapSeq
  private def generateMapSeqCall(m: MapSeq, call: FunCall): Unit = {
    val unroll: Boolean = (   OpenCLMemory.containsPrivateMemory(call.args.head.mem)
      && privateMems.exists(_.mem == call.args.head.mem)) || // Don't unroll just for value
      OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == PrivateMemory

    cur_block += OpenCLAST.Comment("map_seq")
    generateLoop(m.loopVar, () => generate(m.f.body), m.iterationCount, unroll)
    cur_block += OpenCLAST.Comment("end map_seq")
  }
  
  // === Reduce ===
  private def generateReduceSeqCall(r: AbstractReduce, call: FunCall): Unit = {
    val unroll: Boolean = OpenCLMemory.containsPrivateMemory(call.args(1).mem)

    /* ASTNODE */
    val newblock = OpenCLAST.Block(List.empty)
    val oldblock = cur_block
    cur_block = newblock

    cur_block += OpenCLAST.Comment("reduce_seq")
    generateLoop(r.loopVar, () => generate(r.f.body), r.iterationCount, unroll)
    cur_block += OpenCLAST.Comment("end reduce_seq")

    // restore block
    cur_block = oldblock
    cur_block += newblock
  }

  private def generateValue(v: Value): Unit = {
    val temp = Var("")
    cur_block += OpenCLAST.VarDecl(temp.toString, Type.getValueType(v.t),
      init = OpenCLAST.Inline(v.value))
    cur_block += OpenCLAST.Assignment(
      OpenCLAST.VarRef(v.mem.variable.toString),
      OpenCLAST.VarRef(temp.toString))
  }

  // === Iterate ===
  private def generateIterateCall(i: Iterate, call: FunCall): Unit = {

    val inputMem = OpenCLMemory.asOpenCLMemory(call.args.head.mem)
    val outputMem = OpenCLMemory.asOpenCLMemory(call.mem)
    val swapMem = OpenCLMemory.asOpenCLMemory(i.swapBuffer)

    assert (inputMem.addressSpace == outputMem.addressSpace)

    val funCall = i.f.body match { case call: FunCall => call }
    val innerInputLength = Type.getLength(funCall.argsType)
    val innerOutputLength = Type.getLength(funCall.t)

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
    cur_block += OpenCLAST.VarDecl(curOutLen.toString, Int, OpenCLAST.Expression(Type.getLength(call.argsType)))

    // create new temporary input and output pointers
    val tin = Var("tin")
    val tout = Var("tout")
    varDecls = varDecls.updated(tin, Type.devectorize(call.t))
    varDecls = varDecls.updated(tout, Type.devectorize(call.t))

    val tinVStr = OpenCLCodeGen.toOpenCL(tin)
    val toutVStr = OpenCLCodeGen.toOpenCL(tout)
    val inVStr = OpenCLCodeGen.toOpenCL(inputMem.variable)
    val outVStr = OpenCLCodeGen.toOpenCL(outputMem.variable)
    val swapVStr = OpenCLCodeGen.toOpenCL(swapMem.variable)

    val range = i.indexVar.range.asInstanceOf[RangeAdd]

    // ADDRSPC TYPE tin = in;
    cur_block += OpenCLAST.VarDecl(tin.toString, Type.devectorize(call.t), OpenCLAST.VarRef(inputMem.variable.toString), outputMem.addressSpace)

    // ADDRSPC TYPE tin = (odd ? out : swap);
    cur_block += OpenCLAST.VarDecl(toutVStr, Type.devectorize(call.t),
      init = OpenCLAST.Expression(((range.stop % 2) ne Cst(0)) ?? outputMem.variable !! swapMem.variable),
      addressSpace = outputMem.addressSpace)

//    Removed the pragma temporarily as it was causing a (presumably) memory related bug on non NVIDIA and Intel CPU platforms
//    TODO: implement a platform dependent system for inserting the pragma when legal

    generateLoop(i.indexVar, () => {

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

      val curOutLenRef = OpenCLAST.VarRef(curOutLen.toString)
      val tinVStrRef = OpenCLAST.VarRef(tinVStr.toString)
      val toutVStrRef = OpenCLAST.VarRef(toutVStr.toString)
      val swapVStrRef = OpenCLAST.VarRef(swapVStr.toString)

      // tmp = tmp * outputLen / inputLen
      cur_block += OpenCLAST.Assignment(curOutLenRef, OpenCLAST.Expression(curOutLen * innerOutputLength /^ innerInputLength))

      // tin = (tout == swap) ? swap : out
      cur_block += OpenCLAST.Assignment(tinVStrRef,
        OpenCLAST.Expression((tout eq swapMem.variable) ?? swapMem.variable !!  outputMem.variable))

      // tout = (tout == swap) ? out : swap
      cur_block += OpenCLAST.Assignment(toutVStrRef,
        OpenCLAST.Expression((tout eq swapMem.variable) ?? outputMem.variable !! swapMem.variable))
    }, i.iterationCount)
  }

  private def generateLoop(indexVar: Var, printBody: () => Unit,
                           iterationCount: ArithExpr = ?, unroll: Boolean = false): Unit = {
    val range = indexVar.range.asInstanceOf[RangeAdd]
    val step = range.step
    val init = range.start
    val cond = range.stop
    val update = range.step

    if (unroll && iterationCount.eval > 0) {
      cur_block += OpenCLAST.Comment("unroll")

        for (i <- 0 until iterationCount.eval) {
        replacements = replacements.updated(indexVar, i)
        if (range.min.isInstanceOf[OclFunction])
          replacementsWithFuns = replacementsWithFuns.updated(indexVar, range.min + step*i)
        else
          replacementsWithFuns = replacementsWithFuns.updated(indexVar, i)

        printBody()
      }
      cur_block += OpenCLAST.Comment("end unroll")
    } else /* the loop is not unrolled */{
      // Generate an for-loop
      val newblock = OpenCLAST.Block(List.empty)
      // add it to the current node:
      cur_block += OpenCLAST.Loop(indexVar, iterationCount, body = newblock)
      // swap to inner block
      val oldblock = cur_block
      cur_block = newblock
      printBody()
      // swap to parent block
      cur_block = oldblock
    }
  }

  private def generateUserFunCall(u: UserFun, call: FunCall): Unit = {
    assert(call.f == u)

    // Handle vector assignments for vector types
    val mem = OpenCLMemory.asOpenCLMemory(call.mem)
    cur_block += generateStoreNode(mem, call.t, call.view,
      () => generateFunCall(call, generateLoadNodes(call.args: _*)))
  }

  def generateFunCall(expr: Expr, args: List[OpenCLAST.OclAstNode]): OpenCLAST.OclAstNode = {
    expr match {
      case call: FunCall => call.f match {
        case uf: UserFun =>
          OpenCLAST.FunctionCall(uf.name, args)

        //case vf: Vectorize => generateFunCall(UserFun.vectorize(vf.f.asInstanceOf[UserFun], vf.n), args:_*)
        case l: Lambda => generateFunCall(l.body, args)

        case _ => throw new NotImplementedError()
      }
      case _ => throw new NotImplementedError()
    }
  }

  /**
   * Generate a simple or vector store.
   * This function emits a store[n] if the LHS is an array of scala types or an assignment otherwise.
   */
  private def generateStoreNode(mem: OpenCLMemory, t: Type, view: View,
                                valueGenerator: () => OpenCLAST.OclAstNode) : OpenCLAST.OclAstNode = {
    val originalType = varDecls(mem.variable)
    if (Type.isEqual(Type.getValueType(originalType), Type.getValueType(t))) {
      OpenCLAST.Assignment(
        to = simpleAccessNode(mem.variable, mem.addressSpace, t, view),
        value = valueGenerator()
      )
    } else {
      (originalType, t) match {
        case (at: ArrayType, vt: VectorType) => at.elemT match {
          // originally a scalar type, but now a vector type => vstore
          case st: ScalarType if Type.isEqual(st, vt.scalarT) =>
            OpenCLAST.Store(
              OpenCLAST.VarRef(mem.variable.toString), vt,
              value = valueGenerator(),
              offset = OpenCLAST.Expression(ArithExpr.substitute(ViewPrinter.emit(view), replacementsWithFuns) / vt.len))

          // originally an array, but now a vector type => vstore
          case at: ArrayType  if Type.isEqual(at.elemT, vt.scalarT)  =>
            OpenCLAST.Store(
              OpenCLAST.VarRef(mem.variable.toString), vt,
              value = valueGenerator(),
              offset = OpenCLAST.Expression(ArithExpr.substitute(ViewPrinter.emit(view), replacementsWithFuns) / vt.len))
        }
      }
    }
  }

  private def generateLoadNodes(args: Expr*): List[OpenCLAST.OclAstNode] = {
    args.map(generateLoadNode).toList
  }

  private def generateLoadNode(arg: Expr): OpenCLAST.OclAstNode = {
    val mem = OpenCLMemory.asOpenCLMemory(arg.mem)
    generateLoadNode(mem.variable, mem.addressSpace, arg.t, arg.view)
  }

  private def generateLoadNode(v: Var, addressSpace: OpenCLAddressSpace, t: Type, view: View)
    : OpenCLAST.OclAstNode =
  {
    val originalType = varDecls(v)
    if (Type.isEqual(Type.getValueType(originalType), Type.getValueType(t))) {
      simpleAccessNode(v, addressSpace, t, view)
    } else { // types do not match
      (originalType, t) match {
        case (at: ArrayType, vt: VectorType) => at.elemT match {
          // originally a scalar type, but now a vector type => vload
          case st: ScalarType if Type.isEqual(st, vt.scalarT) =>
            OpenCLAST.Load(
              OpenCLAST.VarRef(v.toString), vt,
              offset = OpenCLAST.Expression(ArithExpr.substitute(ViewPrinter.emit(view), replacementsWithFuns) / vt.len)
            )

          // originally an array, but now a vector type => vstore
          case at: ArrayType  if Type.isEqual(at.elemT, vt.scalarT)  =>
            OpenCLAST.Load(
              OpenCLAST.VarRef(v.toString), vt,
              offset = OpenCLAST.Expression(ArithExpr.substitute(ViewPrinter.emit(view), replacementsWithFuns) / vt.len)
            )
        }
        case (st:ScalarType, vt:VectorType)  if Type.isEqual(st, vt.scalarT) => {
          // create (float4) var
          OpenCLAST.Cast(OpenCLAST.VarRef(v.toString), st)
        }
      }
    }
  }

  private def simpleAccessNode(v: Var, addressSpace: OpenCLAddressSpace, t: Type, view: View)
    : OpenCLAST.OclAstNode =
  {
    val varname: String = OpenCLCodeGen.toOpenCL(v)

    addressSpace match {
      case LocalMemory | GlobalMemory =>
        // both types match => no vload necessary ...
        // generate: var[index]
        OpenCLAST.VarRef(varname,
          offset = OpenCLAST.Expression(ArithExpr.substitute(ViewPrinter.emit(view), replacementsWithFuns)))

      case PrivateMemory =>
        privateMems.find(m => m.mem.variable == v) match {
          case None => OpenCLAST.VarRef(varname)
          case Some(typedMemory) =>
            typedMemory.t match {
              // if the allocated memory was originally an index, compute the index and append it
              case _:ArrayType =>
                val index = ArithExpr.substitute(ViewPrinter.emit(view), replacements).eval
                OpenCLAST.VarRef(s"${varname}_" + OpenCLCodeGen.toOpenCL(index))
              case _ =>
                OpenCLAST.VarRef(varname)
            }
        }
    }
  }
}
