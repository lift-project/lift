package opencl.generator

import apart.arithmetic._
import arithmetic.TypeVar
import generator.Generator
import ir._
import ir.ast._
import ir.view.{View, ViewPrinter}
import opencl.generator.OpenCLAST._
import opencl.ir._
import opencl.ir.pattern._
import scala.collection.immutable

class NotPrintableExpression(msg: String) extends Exception(msg)
class NotI(msg: String) extends Exception(msg)

// hacky class to store function name
class OclFunction(name: String, param: Int) extends ArithExprFunction(name) {
  lazy val toOCLString = s"$name($param)"

  override lazy val digest: Int = HashSeed ^ range.digest() ^ name.hashCode ^ param

  override val HashSeed = 0x31111111
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
  // FIXME(tlutz) This should be a val
  var allocateLocalMemoryStatically = true
  def apply() = allocateLocalMemoryStatically
  def apply(allocateStatically: Boolean) = {
    this.allocateLocalMemoryStatically = allocateStatically
  }
}

object OpenCL {
  val warpSize = 32
}

object OpenCLGenerator extends Generator {
  type NDRange = Array[ArithExpr]

  def generate(f: Lambda): String = {
    generate(f, Array(?, ?, ?))
  }

  def generate(f: Lambda, localSizes: NDRange): String = {
    generate(f, localSizes, Array(?, ?, ?), immutable.Map())
  }

  // Compile a type-checked function into an OpenCL kernel
  def generate(f: Lambda, localSize: NDRange, globalSize: NDRange,
               valueMap: immutable.Map[ArithExpr, ArithExpr]): String = {
    (new OpenCLGenerator).generate(f, localSize, globalSize, valueMap)
  }

  def printTypes(expr: Expr): Unit = {
    Expr.visit(expr, {
      case e@(call: FunCall) => println(e + "\n    " +
        e.t + " <- " + call.argsType + "\n")
      case e => println(e + "\n    " + e.t + "\n")
    }, (e: Expr) => {})
  }

  def printTypes(lambda: Lambda): Unit = printTypes(lambda.body)

  def getMemories(f: Lambda): (Array[TypedOpenCLMemory],
                               Array[TypedOpenCLMemory]) = {
    val memories = TypedOpenCLMemory.get(f.body, f.params).toArray

    val numInputs = f.params.length

    val outputMem = memories.last

    if (memories.length > numInputs) {
      val temp = memories(numInputs)
      memories(numInputs) = outputMem
      memories(memories.length-1) = temp
    }

    if (AllocateLocalMemoryStatically())
      memories.partition(isFixedSizeLocalMemory)
    else
      (Array.empty[TypedOpenCLMemory], memories)
  }


  private[generator] def isFixedSizeLocalMemory: (TypedOpenCLMemory) => Boolean = {
    mem => try {
      mem.mem.size.eval
      mem.mem.addressSpace == LocalMemory
    } catch {
      case _: NotEvaluableException =>
        false
    }
  }
}


class OpenCLGenerator extends Generator {

  type NDRange = Array[ArithExpr]
  type ValueTable = immutable.Map[ArithExpr, ArithExpr]
  type SymbolTable = immutable.Map[Var, Type]

  private val openCLCodeGen = new OpenCLCodeGen

  private var replacements: ValueTable = immutable.Map.empty
  private var replacementsWithFuns: ValueTable = immutable.Map.empty
  private var privateMems = Array[TypedOpenCLMemory]()

  private var varDecls: SymbolTable = immutable.Map.empty

  private def printMemories(expr: Expr): Unit = {
    Expr.visit(expr, {
      case e@(call: FunCall) => println(e + "\n    " +
                                        e.mem.toString + " <- " +
                                        call.argsMemory.toString + "\n")
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
//    ast = new OpenCLAST.Block(List.empty, true)
//    cur_block = ast

    assert(localSize.length == 3)
    assert(globalSize.length == 3)
    assert(f.body.t != UndefType)

    if (Verbose()) {
      println("Types:")
      OpenCLGenerator.printTypes(f.body)
    }

    // allocate the params and set the corresponding type
    f.params.foreach((p) => {
      p.t match {
        case _: ScalarType =>
          p.mem = OpenCLMemory.allocPrivateMemory(
                    OpenCLMemory.getMaxSizeInBytes(p.t))
        case _ =>
          p.mem = OpenCLMemory.allocGlobalMemory(
                    OpenCLMemory.getMaxSizeInBytes(p.t))
      }
      p.view = View(p.t, openCLCodeGen.print(p.mem.variable))
    })

    RangesAndCounts(f, localSize, globalSize, valueMap)
    allocateMemory(f)
    BarrierElimination(f)
    CheckBarriersAndLoops(f)

    if (Verbose()) {
      println("Memory:")
      printMemories(f.body)

      println("Allocated Memory:")
      TypedOpenCLMemory.get(f.body, f.params).foreach(m => println(m))
      println()
    }

    View.visitAndBuildViews(f.body)

    val globalBlock = new OpenCLAST.Block(Vector.empty, global = true)

    // pass 2: find and generate user and group functions
    generateUserFunctions(f.body).foreach( globalBlock += _ )
    generateGroupFunctions(f.body).foreach( globalBlock += _ )

    // pass 3: generate the kernel
    globalBlock += generateKernel(f)

    // return the code generated
    openCLCodeGen(globalBlock)
  }

  /** Traversals f and print all user functions using oclPrinter */
  private def generateUserFunctions(expr: Expr): Seq[OclAstNode] = {
    var fs = Seq[OclAstNode]()

    val userFuns = Expr.visitWithState(Set[UserFun]())(expr, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case uf: UserFun => set + uf
          case vec: VectorizeUserFun => set + vec.vectorizedFunction
          case _ => set
        }
        case _ => set
      })

    userFuns.foreach(uf => {
      uf.tupleTypes.foreach(tup => {
        fs = fs :+ OpenCLAST.TypeDef(tup)
      })

      val block = OpenCLAST.Block()
      if(uf.tupleTypes.length == 1)
        block += OpenCLAST.TupleAlias(uf.tupleTypes.head, "Tuple")
      else uf.tupleTypes.zipWithIndex.foreach({ case (x, i) =>
        // TODO: think about this one ...
        block += OpenCLAST.TupleAlias(x, s"Tuple$i")
      })
      block += OpenCLAST.Inline(uf.body)

      fs = fs :+ OpenCLAST.Function(
        name = uf.name,
        ret = uf.outT,
        params = (uf.inTs, uf.paramNames).
                    zipped.map((t,n) => OpenCLAST.ParamDecl(n,t)).toList,
        body = block)
    })

    fs
  }

  /** Traverses f and print all group functions using oclPrinter */
  private def generateGroupFunctions(expr: Expr): Seq[OclAstNode] = {
    var fs = Seq[OclAstNode]()

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
      fs = fs :+ OpenCLAST.Function(
        name = s"groupComp${group.id}",
        ret = Int,
        params = List(
          OpenCLAST.ParamDecl("j", Int),
          OpenCLAST.ParamDecl("i", Int),
          OpenCLAST.ParamDecl(lenVar.toString, Int)
        ),
        body = OpenCLAST.Block(Vector(OpenCLAST.Inline(
          s"""
             |  // Compute new index
             |  int relIndices[] = {${group.relIndices.deep.mkString(", ")}};
             |  int $newIdx = j + relIndices[i];
             |
             |  // Boundary check
             |  if ($newIdx < 0) {
             |    return ${openCLCodeGen.print(
                              group.negOutOfBoundsF(newIdx, lenVar))};
             |  } else if ($newIdx >= $lenVar) {
             |    return ${openCLCodeGen.print(
                              group.posOutOfBoundsF(newIdx - lenVar + 1,
                                                    lenVar))};
             |  } else {
             |    return $newIdx;
             |  }
         """.stripMargin
        ))))
    })

    fs
  }

  def allocateMemory(f: Lambda): Unit = {
    OpenCLMemoryAllocator.alloc(f.body)
    Kernel.memory = TypedOpenCLMemory.get(f.body, f.params).toArray
  }

  private object Kernel {
    var memory = Array.empty[TypedOpenCLMemory]
    var staticLocalMemory = Array.empty[TypedOpenCLMemory]
  }

  private def generateKernel(f: Lambda): OclAstNode = {

    val valMems = Expr.visitWithState(Set[Memory]())(f.body, (expr, set) =>
      expr match {
        case value: Value => set + value.mem
        case _ => set
      })

    val typedMems =
      TypedOpenCLMemory.get(f.body, f.params, includePrivate = true).toArray

    val (typedValueMems, privateMems) =
      typedMems.diff(Kernel.memory).partition(m => valMems.contains(m.mem))

    this.privateMems = privateMems

    // the base type is used for allocation of all variables ...
    this.varDecls =
      typedMems.map(tm => (tm.mem.variable, Type.devectorize(tm.t))).toMap

    // ... besides the these variables which use the value types
    // (i.e., possibly a vector type)
    this.varDecls = this.varDecls ++
                    typedValueMems.map(tm => (tm.mem.variable, tm.t)).toMap


    val memories = OpenCLGenerator.getMemories(f)

    Kernel.memory = memories._2
    Kernel.staticLocalMemory = memories._1

    f.params.foreach(_.mem.readOnly = true)

    // array of all unique vars (like N, iterSize, etc. )
    val allVars = Kernel.memory.map(_.mem.size.varList)
                               .filter(_.nonEmpty).flatten.distinct
    // partition into iteration variables and all others variables
    val (iterateVars, vars) = allVars.partition(_.name == Iterate.varName)

    // Create the actual kernel function
    val kernel = OpenCLAST.Function(
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
      body = OpenCLAST.Block(Vector.empty),
      kernel = true)

    // print out allocated memory sizes
    val varMap = iterateVars.map(v => (v, ArithExpr.asCst(v.range.max))).toMap
    Kernel.memory.foreach(mem => {
      val m = mem.mem
      if (Verbose()) {
        println("Allocated " + ArithExpr.substitute(m.size, varMap.toMap) +
                " bytes for variable " + openCLCodeGen.print(m.variable) +
                " in " + m.addressSpace + " memory")
      }
    })

    kernel.body += OpenCLAST.Comment("Static local memory")
    Kernel.staticLocalMemory.foreach(x =>
      kernel.body +=
        OpenCLAST.VarDecl(x.mem.variable.toString, x.t,
          addressSpace = x.mem.addressSpace,
          length = (x.mem.size /^ Type.getSize(Type.getBaseType(x.t))).eval))

    kernel.body += OpenCLAST.Comment("Typed Value memory")
    typedValueMems.foreach(x =>
      kernel.body +=
        OpenCLAST.VarDecl(x.mem.variable.toString,
          Type.getValueType(x.t),
          addressSpace = x.mem.addressSpace))

    kernel.body += OpenCLAST.Comment("Private Memory")
    privateMems.foreach(x =>
      kernel.body +=
        OpenCLAST.VarDecl(x.mem.variable.toString, x.t,
          addressSpace = x.mem.addressSpace,
          length = (x.mem.size /^ Type.getSize(Type.getBaseType(x.t))).eval))

    generate(f.body, kernel.body)

    kernel
  }



  private def generate(expr: Expr, block: Block): Block = {
    assert(expr.t != UndefType)

    expr match {
      case f: FunCall => f.args.foreach(generate(_, block))
      case _ =>
    }

    expr match {
      case call: FunCall => call.f match {
        case m: MapWrg => generateMapWrgCall(m, call, block)
        case m: MapGlb => generateMapGlbCall(m, call, block)
        case m: MapLcl => generateMapLclCall(m, call, block)
        case m: MapWarp => generateMapWarpCall(m, call, block)
        case m: MapLane => generateMapLaneCall(m, call, block)
        case m: MapSeq => generateMapSeqCall(m, call, block)
        case _: Map => block

        case r: ReduceSeq => generateReduceSeqCall(r, call, block)

        case i: Iterate => generateIterateCall(i, call, block)

        case vec: VectorizeUserFun => generateUserFunCall(vec.vectorizedFunction, call, block)
        case u : UserFun => generateUserFunCall(u, call, block)

        case fp: FPattern => generate(fp.f.body, block)
        case l: Lambda => generate(l.body, block)
        case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
             Split(_) | Join() | Group(_,_,_) | Zip(_) | Tuple(_) | Filter() |
             Head() | Tail() | Scatter(_) | Gather(_) | Get(_) =>
                block

        //case _ => oclPrinter.print("__" + call.toString + "__")
      }
      case v: Value => generateValue(v, block)
      case p: Param => block
    }
  }

  // === Maps ===

  // MapWrg
  private def generateMapWrgCall(m : MapWrg,
                                 call: FunCall,
                                 block: Block): Block = {
    generateLoop(block, m.loopVar, (b) => generate(m.f.body, b),
                 m.iterationCount)
    // TODO: This assumes, that the MapWrg(0) is always the outermost and there
    // is no need for synchronization inside.
    // TODO: Rethink and then redesign this!
    // if (m.dim == 0) {
    //  oclPrinter.println("return;")
    // }
  }

  // MapGlb
  private def generateMapGlbCall(m: MapGlb,
                                 call: FunCall,
                                 block: Block): Block = {
    generateLoop(block, m.loopVar, (b) => generate(m.f.body, b),
                 m.iterationCount)
    // TODO: This assumes, that the MapGlb(0) is always the outermost and there
    // is no need for synchronization inside.
    // TODO: Rethink and then redesign this!
    // if (m.dim == 0) {
    //  oclPrinter.println("return;")
    // }
  }

  // MapLcl
  private def generateMapLclCall(m: MapLcl,
                                 call: FunCall,
                                 block: Block): Block = {
    generateLoop(block, m.loopVar, (b) => generate(m.f.body, b),
                 m.iterationCount,
      (   OpenCLMemory.containsPrivateMemory(call.args.head.mem)
       && privateMems.exists(_.mem == call.args.head.mem)) ||
        // Don't unroll just for value
        OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == PrivateMemory)

    if (m.emitBarrier)
      block += OpenCLAST.Barrier(call.mem.asInstanceOf[OpenCLMemory])

    block
  }

  // MapWarp
  private def generateMapWarpCall(m: MapWarp,
                                  call: FunCall,
                                  block: Block): Block = {
    generateLoop(block, m.loopVar, (b) => generate(m.f.body, b),
                 m.iterationCount)
    call.mem match {
      case m : OpenCLMemory => block += OpenCLAST.Barrier(m)
      case _ =>
    }

    block
  }

  // MapLane
  private def generateMapLaneCall(m: MapLane,
                                  call: FunCall,
                                  block: Block): Block = {
    generateLoop(block, m.loopVar, (b) => generate(m.f.body, b),
                 m.iterationCount)
  }

  // MapSeq
  private def generateMapSeqCall(m: MapSeq,
                                 call: FunCall,
                                 block: Block): Block = {
    val unroll: Boolean =
      (   OpenCLMemory.containsPrivateMemory(call.args.head.mem)
        && privateMems.exists(_.mem == call.args.head.mem)) ||
        // Don't unroll just for value
        OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == PrivateMemory

    block += OpenCLAST.Comment("map_seq")
    generateLoop(block, m.loopVar, (b) => generate(m.f.body, b),
                 m.iterationCount, unroll)
    block += OpenCLAST.Comment("end map_seq")

    block
  }
  
  // === Reduce ===
  private def generateReduceSeqCall(r: AbstractReduce,
                                    call: FunCall,
                                    block: Block): Block = {
    val unroll: Boolean = OpenCLMemory.containsPrivateMemory(call.args(1).mem)

    val nestedBlock = OpenCLAST.Block(Vector.empty)

    nestedBlock += OpenCLAST.Comment("reduce_seq")
    generateLoop(nestedBlock, r.loopVar, (b) => generate(r.f.body, b),
                 r.iterationCount, unroll)
    nestedBlock += OpenCLAST.Comment("end reduce_seq")

    block += nestedBlock

    block
  }

  private def generateValue(v: Value, block: Block): Block = {
    val temp = Var("")

    block += OpenCLAST.VarDecl(temp.toString, Type.getValueType(v.t),
      init = OpenCLAST.Inline(v.value))
    block += OpenCLAST.Assignment(
      OpenCLAST.VarRef(v.mem.variable.toString),
      OpenCLAST.VarRef(temp.toString))

    block
  }

  // === Iterate ===
  private def generateIterateCall(i: Iterate,
                                  call: FunCall,
                                  block: Block): Block = {

    val inputMem = OpenCLMemory.asOpenCLMemory(call.args.head.mem)
    val outputMem = OpenCLMemory.asOpenCLMemory(call.mem)
    val swapMem = OpenCLMemory.asOpenCLMemory(i.swapBuffer)

    assert (inputMem.addressSpace == outputMem.addressSpace)

    val funCall = i.f.body match { case call: FunCall => call }

    // use the type var as the var holding the iterating size if it exists
    if (TypeVar.getTypeVars(funCall.argsType).size > 1) {
      println("size: " + TypeVar.getTypeVars(funCall.argsType).size)
      TypeVar.getTypeVars(funCall.argsType).foreach( (tv) => {
        println("tv: " + tv)
      })
      println("i.f.inT " + funCall.argsType)
      throw new NotImplementedError()
    }

    val curOutLen =
      if (TypeVar.getTypeVars(funCall.argsType).isEmpty)
        Var("curOutLen")
      else
        TypeVar.getTypeVars(funCall.argsType).head

    block +=
      OpenCLAST.VarDecl(curOutLen.toString, Int,
                        OpenCLAST.Expression(Type.getLength(call.argsType)))

    // create new temporary input and output pointers
    val tin = Var("tin")
    val tout = Var("tout")
    varDecls = varDecls.updated(tin, Type.devectorize(call.t))
    varDecls = varDecls.updated(tout, Type.devectorize(call.t))

    // ADDRSPC TYPE tin = in;
    block += OpenCLAST.VarDecl(tin.toString, Type.devectorize(call.t),
                               OpenCLAST.VarRef(inputMem.variable.toString),
                               outputMem.addressSpace)

    val toutVStr = openCLCodeGen.print(tout)
    val range = i.indexVar.range.asInstanceOf[RangeAdd]

    // ADDRSPC TYPE tin = (odd ? out : swap);
    block += OpenCLAST.VarDecl(toutVStr, Type.devectorize(call.t),
      init = OpenCLAST.Expression(
        ((range.stop % 2) ne Cst(0)) ?? outputMem.variable !! swapMem.variable),
        addressSpace = outputMem.addressSpace)

    generateLoop(block, i.indexVar, (b) => {

      // modify the pointers to the memory before generating the body
      val oldInV = inputMem.variable
      val oldOutV = outputMem.variable
      inputMem.variable = tin
      outputMem.variable = tout

      // generate the function call in the body
      generate(funCall, b)

      // restore the pointers to memory
      inputMem.variable = oldInV
      outputMem.variable = oldOutV

      val curOutLenRef = OpenCLAST.VarRef(curOutLen.toString)

      val innerOutputLength = Type.getLength(funCall.t)
      val innerInputLength = Type.getLength(funCall.argsType)

      // tmp = tmp * outputLen / inputLen
      b += OpenCLAST.Assignment(curOutLenRef,
        OpenCLAST.Expression(curOutLen * innerOutputLength /^ innerInputLength))


      val tinVStrRef = OpenCLAST.VarRef(openCLCodeGen.print(tin))

      // tin = (tout == swap) ? swap : out
      b += OpenCLAST.Assignment(tinVStrRef,
        OpenCLAST.Expression((tout eq swapMem.variable) ??
                             swapMem.variable !!  outputMem.variable))


      val toutVStrRef = OpenCLAST.VarRef(toutVStr)

      // tout = (tout == swap) ? out : swap
      b += OpenCLAST.Assignment(toutVStrRef,
        OpenCLAST.Expression((tout eq swapMem.variable) ??
                             outputMem.variable !! swapMem.variable))

      b
    }, i.iterationCount)
  }

  private def generateLoop(block: Block,
                           indexVar: Var, printBody: (Block) => Block,
                           iterationCount: ArithExpr = ?,
                           unroll: Boolean = false): Block = {
    val range = indexVar.range.asInstanceOf[RangeAdd]
    val step = range.step

    if (unroll && iterationCount.eval > 0) {



      block += OpenCLAST.Comment("unroll")

      for (i <- 0 until iterationCount.eval) {
        replacements = replacements.updated(indexVar, i)
        if (range.min.isInstanceOf[OclFunction]) {
          replacementsWithFuns =
            replacementsWithFuns.updated(indexVar, range.min + step * i)
        } else {
          replacementsWithFuns =
            replacementsWithFuns.updated(indexVar, i)
        }

        printBody(block)
      }

      block += OpenCLAST.Comment("end unroll")
    } else /* the loop is not unrolled */ {
      // Generate an for-loop
      val innerBlock = OpenCLAST.Block(Vector.empty)
      // add it to the current node:
      block += OpenCLAST.Loop(indexVar, iterationCount, body = innerBlock)
      printBody(innerBlock)
    }

    block
  }

  private def generateUserFunCall(u: UserFun,
                                  call: FunCall,
                                  block: Block): Block = {
    // Handle vector assignments for vector types
    val mem = OpenCLMemory.asOpenCLMemory(call.mem)
    block += generateStoreNode(mem, call.t, call.view,
      generateFunCall(call, generateLoadNodes(call.args: _*)))

    block
  }

  private def generateFunCall(expr: Expr,
                      args: List[OclAstNode]): OclAstNode = {
    expr match {
      case call: FunCall => call.f match {
        case uf: UserFun =>
          OpenCLAST.FunctionCall(uf.name, args)

        case vf: VectorizeUserFun =>
          OpenCLAST.FunctionCall(vf.vectorizedFunction.name, args)
        case l: Lambda => generateFunCall(l.body, args)

        case _ => throw new NotImplementedError()
      }
      case _ => throw new NotImplementedError()
    }
  }

  /**
   * Generate a simple or vector store.
   * This function emits a store[n] if the LHS is an array of scala types or
   * an assignment otherwise.
   */
  private def generateStoreNode(mem: OpenCLMemory,
                                t: Type,
                                view: View,
                                value: OclAstNode): OclAstNode = {
    val originalType = varDecls(mem.variable)
    if (Type.isEqual(Type.getValueType(originalType), Type.getValueType(t))) {
      OpenCLAST.Assignment(
        to = simpleAccessNode(mem.variable, mem.addressSpace, t, view),
        value = value
      )
    } else {
      (originalType, t) match {
        case (at: ArrayType, vt: VectorType) => Type.getValueType(at) match {
          // originally a scalar type, but now a vector type => vstore
          case st: ScalarType if Type.isEqual(st, vt.scalarT) =>
            OpenCLAST.Store(
              OpenCLAST.VarRef(mem.variable.toString), vt,
              value = value,
              offset = OpenCLAST.Expression(
                          ArithExpr.substitute(ViewPrinter.emit(view),
                                               replacementsWithFuns) / vt.len))
        }
      }
    }
  }

  private def generateLoadNodes(args: Expr*): List[OclAstNode] = {
    args.map(generateLoadNode).toList
  }

  private def generateLoadNode(arg: Expr): OclAstNode = {
    val mem = OpenCLMemory.asOpenCLMemory(arg.mem)
    generateLoadNode(mem.variable, mem.addressSpace, arg.t, arg.view)
  }

  private def generateLoadNode(v: Var,
                               addressSpace: OpenCLAddressSpace,
                               t: Type,
                               view: View): OpenCLAST.OclAstNode = {
    val originalType = varDecls(v)
    if (Type.isEqual(Type.getValueType(originalType), Type.getValueType(t))) {
      simpleAccessNode(v, addressSpace, t, view)
    } else { // types do not match
      (originalType, t) match {
        case (at: ArrayType, vt: VectorType) => Type.getValueType(at) match {
          // originally a scalar type, but now a vector type => vload
          case st: ScalarType if Type.isEqual(st, vt.scalarT) =>
            OpenCLAST.Load(
              OpenCLAST.VarRef(v.toString), vt,
              offset = OpenCLAST.Expression(
                          ArithExpr.substitute(ViewPrinter.emit(view),
                                               replacementsWithFuns) / vt.len)
            )
        }
        case (st:ScalarType, vt:VectorType)  if Type.isEqual(st, vt.scalarT) =>
          // create (float4) var
          OpenCLAST.Cast(OpenCLAST.VarRef(v.toString), st)
      }
    }
  }

  private def simpleAccessNode(v: Var,
                               addressSpace: OpenCLAddressSpace,
                               t: Type,
                               view: View): OpenCLAST.OclAstNode = {
    val varname: String = openCLCodeGen.print(v)

    addressSpace match {
      case LocalMemory | GlobalMemory =>
        // both types match => no vload necessary ...
        // generate: var[index]
        OpenCLAST.VarRef(varname,
          offset = OpenCLAST.Expression(
                      ArithExpr.substitute(ViewPrinter.emit(view),
                                           replacementsWithFuns)))

      case PrivateMemory =>
        privateMems.find(m => m.mem.variable == v) match {
          case None => OpenCLAST.VarRef(varname)
          case Some(typedMemory) =>
            typedMemory.t match {
              // if the allocated memory was originally an index, compute the
              // index and append it
              case _:ArrayType =>
                val index = ArithExpr.substitute(ViewPrinter.emit(view),
                                                 replacements).eval

                OpenCLAST.VarRef(s"${varname}_" + openCLCodeGen.print(index))

              case _ =>
                OpenCLAST.VarRef(varname)
            }
        }
    }
  }
}
