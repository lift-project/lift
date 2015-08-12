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
import scala.collection.{mutable, immutable}

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
  def apply(allocateStatically: Boolean) = {
    this.allocateLocalMemoryStatically = allocateStatically
  }
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


  private def printTypes(expr: Expr): Unit = {
    Expr.visit(expr, {
      case e@(call: FunCall) => println(e + "\n    " +
                                        e.t + " <- " + call.argsType + "\n")
      case e => println(e + "\n    " + e.t + "\n")
    }, (e: Expr) => {})
  }

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
      printTypes(f.body)
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
      p.view = View(p.t, p.mem.variable.toString)
    })

    RangesAndCounts(f, localSize, globalSize, valueMap)
    allocateMemory(f)
    BarrierElimination(f)

    if (Verbose()) {
      println("Memory:")
      printMemories(f.body)

      println("Allocated Memory:")
      TypedOpenCLMemory.
        getAllocatedMemory(f.body, f.params).foreach(m => println(m.toString))
      println("")
    }

    View.visitAndBuildViews(f.body)

    val globalBlock = new OpenCLAST.Block(List.empty, global = true)

    // pass 2: find and generate user and group functions
    generateUserFunctions(f.body).foreach( globalBlock += _ )
    generateGroupFunctions(f.body).foreach( globalBlock += _ )

    // pass 3: generate the kernel
    globalBlock += generateKernel(f)

    // return the code generated
    OpenCLCodeGen(globalBlock)
  }

  /** Traversals f and print all user functions using oclPrinter */
  def generateUserFunctions(expr: Expr): Seq[OclAstNode] = {
    var fs = Seq[OclAstNode]()

    val userFuns = Expr.visitWithState(Set[UserFun]())(expr, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case uf: UserFun => set + uf
          //case vec: Vectorize =>
          //  set + UserFun.vectorize(vec.f.asInstanceOf[UserFun], vec.n)
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
      block += OpenCLAST.OpenCLCode(uf.body)

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
  def generateGroupFunctions(expr: Expr): Seq[OclAstNode] = {
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
        body = OpenCLAST.Block(List(OpenCLAST.OpenCLCode(
          s"""
             |  // Compute new index
             |  int relIndices[] = {${group.relIndices.deep.mkString(", ")}};
             |  int $newIdx = j + relIndices[i];
             |
             |  // Boundary check
             |  if ($newIdx < 0) {
             |    return ${OpenCLCodeGen.print(
                              group.negOutOfBoundsF(newIdx, lenVar))};
             |  } else if ($newIdx >= $lenVar) {
             |    return ${OpenCLCodeGen.print(
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

  private def generateKernel(f: Lambda,
                             workGroupSize: Int = 128): OclAstNode = {
    Kernel.workGroupSize = workGroupSize

    val valMems = Expr.visitWithState(Set[Memory]())(f.body, (expr, set) =>
      expr match {
        case value: Value => set + value.mem
        case _ => set
      })

    val (typedValueMems, privateMems) =
      TypedOpenCLMemory.
        getAllocatedMemory(f.body,
                           f.params,
                           includePrivate = true).diff(Kernel.memory).
          partition(m => valMems.contains(m.mem))

    this.privateMems = privateMems

    // the base type is used for allocation of all variables ...
    this.varDecls =
      TypedOpenCLMemory.
        getAllocatedMemory(f.body,
                           f.params,
                           includePrivate = true).
          map(tm => (tm.mem.variable, Type.devectorize(tm.t))).toMap

    // ... besides the these variables which use the value types
    // (i.e., possibly a vector type)
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
      body = OpenCLAST.Block(List()),
      kernel = true)

    // print out allocated memory sizes
    val varMap = iterateVars.map(v => (v, ArithExpr.asCst(v.range.max))).toMap
    Kernel.memory.foreach(mem => {
      val m = mem.mem
      if (Verbose()) {
        println("Allocated " + ArithExpr.substitute(m.size, varMap.toMap) +
                " bytes for variable " + m.variable.toString +
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

    findAndDeclareCommonSubterms(kernel.body)

    kernel
  }



  private def generate(expr: Expr, block: Block): Unit = {
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
        case _: Map =>

        case r: ReduceSeq => generateReduceSeqCall(r, call, block)

        case i: Iterate => generateIterateCall(i, call, block)

        case u : UserFun =>generateUserFunCall(u, call, block)

        case fp: FPattern => generate(fp.f.body, block)
        case l: Lambda => generate(l.body, block)
        case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
             Split(_) | Join() | Group(_,_,_) | Zip(_) | Tuple(_) | Filter() |
             Head() | Tail() | Scatter(_) | Gather(_) | Epsilon() | Get(_) =>

        //case _ => oclPrinter.print("__" + call.toString + "__")
      }
      case v: Value => generateValue(v, block)
      case p: Param =>
    }
  }

  // === Maps ===

  // MapWrg
  private def generateMapWrgCall(m : MapWrg,
                                 call: FunCall,
                                 block: Block): Unit = {
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
                                 block: Block): Unit = {
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
                                 block: Block): Unit = {
    generateLoop(block, m.loopVar, (b) => generate(m.f.body, b),
                 m.iterationCount,
      (   OpenCLMemory.containsPrivateMemory(call.args.head.mem)
       && privateMems.exists(_.mem == call.args.head.mem)) ||
        // Don't unroll just for value
        OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == PrivateMemory)

    if (m.emitBarrier)
      block += OpenCLAST.Barrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // MapWarp
  private def generateMapWarpCall(m: MapWarp,
                                  call: FunCall,
                                  block: Block): Unit = {
    generateLoop(block, m.loopVar, (b) => generate(m.f.body, b),
                 m.iterationCount)
    call.mem match {
      case m : OpenCLMemory => block += OpenCLAST.Barrier(m)
      case _ =>
    }
  }

  // MapLane
  private def generateMapLaneCall(m: MapLane,
                                  call: FunCall,
                                  block: Block): Unit = {
    generateLoop(block, m.loopVar, (b) => generate(m.f.body, b),
                 m.iterationCount)
  }

  // MapSeq
  private def generateMapSeqCall(m: MapSeq,
                                 call: FunCall,
                                 block: Block): Unit = {
    val unroll: Boolean =
      (   OpenCLMemory.containsPrivateMemory(call.args.head.mem)
        && privateMems.exists(_.mem == call.args.head.mem)) ||
        // Don't unroll just for value
        OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == PrivateMemory

    block += OpenCLAST.Comment("map_seq")
    generateLoop(block, m.loopVar, (b) => generate(m.f.body, b),
                 m.iterationCount, unroll)
    block += OpenCLAST.Comment("end map_seq")
  }
  
  // === Reduce ===
  private def generateReduceSeqCall(r: AbstractReduce,
                                    call: FunCall,
                                    block: Block): Unit = {
    val unroll: Boolean = OpenCLMemory.containsPrivateMemory(call.args(1).mem)

    val nestedBlock = OpenCLAST.Block(List.empty)

    nestedBlock += OpenCLAST.Comment("reduce_seq")
    generateLoop(nestedBlock, r.loopVar, (b) => generate(r.f.body, b),
                 r.iterationCount, unroll)
    nestedBlock += OpenCLAST.Comment("end reduce_seq")

    block += nestedBlock
  }

  private def generateValue(v: Value, block: Block): Unit = {
    val temp = Var("")

    block += OpenCLAST.VarDecl(temp.toString, Type.getValueType(v.t),
      init = OpenCLAST.OpenCLCode(v.value))
    block += OpenCLAST.Assignment(
      OpenCLAST.VarRef(v.mem.variable.toString),
      OpenCLAST.VarRef(temp.toString))
  }

  // === Iterate ===
  private def generateIterateCall(i: Iterate,
                                  call: FunCall,
                                  block: Block): Unit = {

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

    val toutVStr = tout.toString
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


      val tinVStrRef = OpenCLAST.VarRef(tin.toString)

      // tin = (tout == swap) ? swap : out
      b += OpenCLAST.Assignment(tinVStrRef,
        OpenCLAST.Expression((tout eq swapMem.variable) ??
                             swapMem.variable !!  outputMem.variable))


      val toutVStrRef = OpenCLAST.VarRef(toutVStr)

      // tout = (tout == swap) ? out : swap
      b += OpenCLAST.Assignment(toutVStrRef,
        OpenCLAST.Expression((tout eq swapMem.variable) ??
                             outputMem.variable !! swapMem.variable))
    }, i.iterationCount)
  }

  private def generateLoop(block: Block,
                           indexVar: Var, generate: (Block) => Unit,
                           iterationCount: ArithExpr = ?,
                           unroll: Boolean = false): Unit = {
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

        generate(block)
      }

      block += OpenCLAST.Comment("end unroll")
    } else /* the loop is not unrolled */ {
      // Generate an for-loop
      val innerBlock = OpenCLAST.Block(List.empty)
      // add it to the current node:
      block += OpenCLAST.Loop(indexVar, iterationCount, body = innerBlock)
      generate(innerBlock)
    }
  }

  private def generateUserFunCall(u: UserFun,
                                  call: FunCall,
                                  block: Block): Unit = {
    assert(call.f == u)

    val funCall = generateFunCall(call, generateLoadNodes(block, call.args: _*))

    val mem = OpenCLMemory.asOpenCLMemory(call.mem)
    block += generateStoreNode(block, mem, call.t, call.view, funCall)
  }

  def generateFunCall(expr: Expr,
                      args: List[OclAstNode]): OclAstNode = {
    expr match {
      case call: FunCall => call.f match {
        case uf: UserFun =>
          OpenCLAST.FunctionCall(uf.name, args)

        //case vf: Vectorize =>
        //  generateFunCall(UserFun.vectorize(vf.f.asInstanceOf[UserFun],
        //                                    vf.n), args:_*)
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
  private def generateStoreNode(block: Block,
                                mem: OpenCLMemory,
                                t: Type,
                                view: View,
                                value: OclAstNode): OclAstNode = {
    val originalType = varDecls(mem.variable)
    if (Type.isEqual(Type.getValueType(originalType), Type.getValueType(t))) {
      OpenCLAST.Assignment(
        to = simpleAccessNode(block, mem.variable, mem.addressSpace, t, view),
        value = value
      )
    } else {
      (originalType, t) match {
        case (at: ArrayType, vt: VectorType) => at.elemT match {
          // originally a scalar type, but now a vector type => vstore
          case st: ScalarType if Type.isEqual(st, vt.scalarT) =>
            OpenCLAST.Store(
              OpenCLAST.VarRef(mem.variable.toString), vt,
              value = value,
              offset = OpenCLAST.Expression(
                          ArithExpr.substitute(ViewPrinter.emit(view),
                                               replacementsWithFuns) / vt.len))

          // originally an array, but now a vector type => vstore
          case at: ArrayType  if Type.isEqual(at.elemT, vt.scalarT)  =>
            OpenCLAST.Store(
              OpenCLAST.VarRef(mem.variable.toString), vt,
              value = value,
              offset = OpenCLAST.Expression(
                          ArithExpr.substitute(ViewPrinter.emit(view),
                                               replacementsWithFuns) / vt.len))
          case _ => throw new NotImplementedError()
        }

        case _ => throw new NotImplementedError()
      }
    }
  }

  private def generateLoadNodes(block: Block, args: Expr*): List[OclAstNode] = {
    args.map(generateLoadNode(block, _)).toList
  }

  private def generateLoadNode(block: Block, arg: Expr): OclAstNode = {
    val mem = OpenCLMemory.asOpenCLMemory(arg.mem)
    generateLoadNode(block, mem.variable, mem.addressSpace, arg.t, arg.view)
  }

  private def generateLoadNode(block: Block,
                               v: Var,
                               addressSpace: OpenCLAddressSpace,
                               t: Type,
                               view: View): OpenCLAST.OclAstNode = {
    val originalType = varDecls(v)
    if (Type.isEqual(Type.getValueType(originalType), Type.getValueType(t))) {
      simpleAccessNode(block, v, addressSpace, t, view)
    } else { // types do not match
      (originalType, t) match {
        case (at: ArrayType, vt: VectorType) => at.elemT match {
          // originally a scalar type, but now a vector type => vload
          case st: ScalarType if Type.isEqual(st, vt.scalarT) =>
            OpenCLAST.Load(
              OpenCLAST.VarRef(v.toString), vt,
              offset = OpenCLAST.Expression(
                          ArithExpr.substitute(ViewPrinter.emit(view),
                                               replacementsWithFuns) / vt.len)
            )

          // originally an array, but now a vector type => vstore
          case at: ArrayType  if Type.isEqual(at.elemT, vt.scalarT)  =>
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

  private def simpleAccessNode(block: Block,
                               v: Var,
                               addressSpace: OpenCLAddressSpace,
                               t: Type,
                               view: View): OpenCLAST.OclAstNode = {
    val varname: String = v.toString

    addressSpace match {
      case LocalMemory | GlobalMemory =>
        // both types match => no vload necessary ...
        // generate: var[index]

        // index as ArithExpr
        val index = ArithExpr.substitute(ViewPrinter.emit(view),
                                         replacementsWithFuns)

        // create variable reference with index (i.e. access) to the freshly
        // created indexVar
        OpenCLAST.VarRef(name = varname,
                         index = OpenCLAST.Expression(index))

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

                OpenCLAST.VarRef(s"${varname}_" + OpenCLCodeGen.print(index))

              case _ =>
                OpenCLAST.VarRef(varname)
            }
        }
    }
  }

  private def findAndDeclareCommonSubterms(block: Block): Unit = {
    visitBlocks(block, process)

    def process(block: Block): Unit = {
      // get all the arithmetic expressions from this block
      var expressions = Seq[Expression]()
      visitExpressionsInBlock(block, e => { expressions = expressions :+ e })

      // map for counting how often subterms appear
      val counts = mutable.Map[ArithExpr, Int]()

      // count how many times a subterm appears in the expressions
      expressions.foreach(expr => {
        ArithExpr.visit(expr.content, subterm => {
          counts get subterm match {
            case None => counts put (subterm, 1)
            case Some(c) => counts put (subterm, c + 1)
          }
        })
      })

      // just choose the subterms which appear more than once and are not
      // instances of Var (i.e. are already variables)
      val subterms = counts.filter(_._2 > 1)
                           .filter(!_._1.isInstanceOf[Var])
                           .filter(!_._1.isInstanceOf[Cst])
                           .filter(!_._1.isInstanceOf[OclFunction])
                           .filter(_._1 match {
                              // don't choose pow(b, -1), as this might be
                              // printed as "/b" inside of a product
                              case Pow(_, Cst(-1)) => false
                              case _ => true
                            })

      // Pow(b, Cst(-1))

      val substitutions = mutable.Map[ArithExpr, ArithExpr]()

      val newVarDecls =
        // for every subterm p._1 ...
        subterms.map( p => {
          // ... create a new variable ...
          val newVar = Var("")
          // ... and prepare the substitution of the old expression with the
          //     new variable
          substitutions put (p._1, newVar)

          OpenCLAST.VarDecl(name = newVar.toString,
                            t = Int,
                            init = OpenCLAST.Expression(p._1),
                            addressSpace = PrivateMemory)
        })

      // update the Expression nodes to
      expressions.foreach(expr => {
        expr.content = ArithExpr.substitute(expr.content, substitutions.toMap)
      })

      // introduce new var decls at the beginning of the current block
      newVarDecls.foreach(_ :: block)
    }
  }
}
