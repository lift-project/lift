package opencl.generator

import arithmetic.TypeVar
import generator.Generator
import ir._
import ir.ast._
import ir.view._
import lift.arithmetic._
import opencl.generator.OpenCLAST._
import opencl.ir._
import opencl.ir.ast.OpenCLBuiltInFun
import opencl.ir.pattern._
import utils.Printer

import scala.collection.immutable

object OpenCLGenerator extends Generator {

  def generate(f: Lambda): String = {
    generate(f, NDRange(?, ?, ?))
  }

  def generate(f: Lambda, localSizes: NDRange): String = {
    generate(f, localSizes, NDRange(?, ?, ?), immutable.Map())
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
      case e                 => println(e + "\n    " + e.t + "\n")
    }, (_: Expr) => {})
  }

  def printTypes(lambda: Lambda): Unit = printTypes(lambda.body)

  /**
   * Get memory objects allocated for given Lambda
   *
   * @param f The lambda for which to get the memory objects
   * @return A tuple with the first component the statically allocated local memory objects and the second
   * component all remaining memory objects (i.e. dynamically allocated local and global memory objects)
   */
  def getMemories(f: Lambda): (Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory]) = {
    val (inputs, outputs, globalIntermediates, localIntermediates) = CollectTypedOpenCLMemory(f)

    if (AllocateLocalMemoryStatically()) {
      val (staticLocalIntermediates, dynamicLocalIntermediates) = localIntermediates.partition(isFixedSizeLocalMemory)
      (staticLocalIntermediates, inputs ++ outputs ++ globalIntermediates ++ dynamicLocalIntermediates)
    } else
      (Seq.empty[TypedOpenCLMemory], inputs ++ outputs ++ globalIntermediates ++ localIntermediates)
  }

  def getDifferentMemories(lambda: Lambda): (Seq[TypedOpenCLMemory], Seq[TypedOpenCLMemory], Predef.Map[Var, Type]) = {

    val valMems = Expr.visitWithState(Set[Memory]())(lambda.body, (expr, set) =>
      expr match {
        case value: Value => set + value.mem
        case _            => set
      })

    val typedMems = CollectTypedOpenCLMemory.asFlatSequence(lambda, includePrivate = true)

    val memory = CollectTypedOpenCLMemory.asFlatSequence(lambda)

    val (typedValueMems, privateMems) =
      typedMems.diff(memory).partition(m => valMems.contains(m.mem))

    // the base type is used for allocation of all variables ...
    var varDecls =
      typedMems.map(tm => {
        if (tm.mem.addressSpace == PrivateMemory || tm.t.isInstanceOf[VectorType]) {
          // do not de-vectorise for private memory
          // only de-vectorise arrays
          (tm.mem.variable, tm.t)
        } else {
          (tm.mem.variable, Type.devectorize(tm.t))
        }
      }).toMap

    // ... besides the these variables which use the value types
    // (i.e., possibly a vector type)
    varDecls = varDecls ++
      typedValueMems.map(tm => (tm.mem.variable, tm.t)).toMap

    (typedValueMems, privateMems, varDecls)
  }

  def getOriginalType(mem: OpenCLMemory,
                      varDecls: immutable.Map[Var, Type]): Type = {

    try {
      varDecls(mem.variable)
    } catch {
      case _: NoSuchElementException =>
        throw new VariableNotDeclaredError(s"Trying to generate access to variable " +
          s"${mem.variable} which was not previously declared.")
    }

  }

  def createFunctionDefinition(uf: UserFun): Function = {
    val block = OpenCLAST.Block()
    if (uf.tupleTypes.length == 1)
      block += OpenCLAST.TupleAlias(uf.tupleTypes.head, "Tuple")
    else uf.tupleTypes.zipWithIndex.foreach({ case (x, i) =>
      // TODO: think about this one ...
      block += OpenCLAST.TupleAlias(x, s"Tuple$i")
    })
    block += OpenCLAST.OpenCLCode(uf.body)

    OpenCLAST.Function(
      name = uf.name,
      ret = uf.outT,
      params = (uf.inTs, uf.paramNames).
        zipped.map((t, n) => OpenCLAST.ParamDecl(n, t)).toList,
      body = block)
  }

  private[generator] def isFixedSizeLocalMemory: (TypedOpenCLMemory) => Boolean = {
    mem =>
      try {
        mem.mem.size.evalLong
        mem.mem.addressSpace == LocalMemory
      } catch {
        case NotEvaluableException() => false
      }
  }
}

class OpenCLGenerator extends Generator {

  type ValueTable = immutable.Map[ArithExpr, ArithExpr]
  type SymbolTable = immutable.Map[Var, Type]


  private val openCLCodeGen = new OpenCLPrinter

  private var replacements: ValueTable = immutable.Map.empty
  private var replacementsWithFuns: ValueTable = immutable.Map.empty
  private var privateMems = Seq[TypedOpenCLMemory]()
  private var privateDecls = immutable.Map[Var, OpenCLAST.VarDecl]()

  private var varDecls: SymbolTable = immutable.Map.empty

  private var localSize: NDRange = _

  private def printMemories(expr: Expr): Unit = {
    Expr.visit(expr, {
      case e@(call: FunCall) =>
        println(e + "\n    " +
          e.mem.toString + " <- " +
          call.argsMemory.toString + "\n")
      case e                 => println(e + "\n    " + e.mem.toString + "\n")
    }, (_: Expr) => {})
  }

  def generate(f: Lambda): String = {
    generate(f, NDRange(?, ?, ?))
  }

  def generate(f: Lambda, localSizes: NDRange): String = {
    generate(f, localSizes, NDRange(?, ?, ?), immutable.Map())
  }

  def generate(f: Lambda, localSize: NDRange, globalSize: NDRange,
               valueMap: collection.Map[ArithExpr, ArithExpr]): String = {

    this.localSize = localSize

    if (f.body.t == UndefType)
      throw new OpenCLGeneratorException("Lambda has to be type-checked to generate code")

    InferOpenCLAddressSpace(f)
    RangesAndCounts(f, localSize, globalSize, valueMap)
    allocateMemory(f)

    ShouldUnroll(f)

    if (PerformBarrierElimination())
      BarrierElimination(f)

    checkLambdaIsLegal(f)

    if (Verbose()) {

      println("Types:")
      OpenCLGenerator.printTypes(f.body)

      println("Memory:")
      printMemories(f.body)

      println("Allocated Memory:")
      val (inputs, outputs, globalTmps, localTmps) = CollectTypedOpenCLMemory(f, includePrivate = true)
      println(" inputs:")
      inputs.foreach(println(_))
      println(" outputs:")
      outputs.foreach(println(_))
      println(" global tmps:")
      globalTmps.foreach(println(_))
      println(" local tmps:")
      localTmps.foreach(println(_))
      println()
    }

    View(f)

    val globalBlock = OpenCLAST.Block(Vector.empty, global = true)

    val containsDouble = Expr.visitWithState(false)(f.body, {
      case (expr, state) =>
        // A `Double` may be hidden in a TupleType. We need to visit the type
        // of each expression
        var found = false
        Type.visit(expr.t, t => if (t == Double) found = true, _ => ())
        found || state
    })

    if (containsDouble) {
      globalBlock += OpenCLExtension("cl_khr_fp64")
    }

    val tupleTypes = Expr.visitWithState(Set[TupleType]())(f.body, (expr, typeList) => {
      expr match {
        case FunCall(uf: UserFun, _*)           => typeList ++ uf.tupleTypes
        case FunCall(vec: VectorizeUserFun, _*) => typeList ++ vec.vectorizedFunction.tupleTypes
        case _                                  =>
          expr.t match {
            case t: TupleType if t.elemsT.forall(t => {
              var containsArray = false
              Type.visit(t, x => containsArray ||= x.isInstanceOf[ArrayType], _ => Unit)
              !containsArray
            })     => typeList + t
            case _ => typeList
          }
      }
    })

    tupleTypes.foreach(globalBlock += OpenCLAST.TypeDef(_))

    // pass 2: find and generate user and group functions
    generateUserFunctions(f.body).foreach(globalBlock += _)

    // pass 3: generate the
    globalBlock += generateKernel(f)

    // return the code generated
    openCLCodeGen(globalBlock)
  }

  // TODO: Gather(_)/Transpose() without read and Scatter(_)/TransposeW() without write
  private def checkLambdaIsLegal(lambda: Lambda): Unit = {
    CheckBarriersAndLoops(lambda)

    UpdateContext(lambda)

    Expr.visit(lambda.body, _ => Unit, {
      case call@FunCall(MapGlb(dim, _), _*) if call.context.inMapGlb(dim)       =>
        throw new IllegalKernel(s"Illegal nesting of $call inside MapGlb($dim)")
      case call@FunCall(MapWrg(dim, _), _*) if call.context.inMapWrg(dim)       =>
        throw new IllegalKernel(s"Illegal nesting of $call inside MapWrg($dim)")
      case call@FunCall(MapLcl(dim, _), _*) if call.context.inMapLcl(dim)       =>
        throw new IllegalKernel(s"Illegal nesting of $call inside MapLcl($dim)")
      case call@FunCall(MapLcl(dim, _), _*) if !call.context.inMapWrg(dim)      =>
        throw new IllegalKernel(s"Illegal use of $call without MapWrg($dim)")
      case call@FunCall(toLocal(_), _) if !call.context.inMapWrg.reduce(_ || _) =>
        throw new IllegalKernel(s"Illegal use of local memory, without using MapWrg $call")
      case call@FunCall(Map(Lambda(_, expr)), _*) if expr.isConcrete            =>
        throw new IllegalKernel(s"Illegal use of UserFun where it won't generate code in $call")
      case call@FunCall(Reduce(Lambda(_, expr)), _, _) if expr.isConcrete       =>
        throw new IllegalKernel(s"Illegal use of UserFun where it won't generate code in $call")
      case call@FunCall(PartRed(Lambda(_, expr)), _, _) if expr.isConcrete      =>
        throw new IllegalKernel(s"Illegal use of UserFun where it won't generate code in $call")
      case call@FunCall(Id(), _)                                                =>
        throw new IllegalKernel(s"Illegal use of Id where it won't generate a copy in $call")
      case _                                                                    =>
    })

    lambda.body.mem match {
      case m: OpenCLMemory if m.addressSpace != GlobalMemory =>
        throw new IllegalKernel("Final result must be stored in global memory")
      case _                                                 =>
    }
  }

  /** Traversals f and print all user functions using oclPrinter */
  private def generateUserFunctions(expr: Expr): Seq[Declaration] = {

    val userFuns = Expr.visitWithState(Set[UserFun]())(expr, (expr, set) =>
      expr match {
        case call: FunCall                      => call.f match {
          case _: OpenCLBuiltInFun   => set
          case uf: UserFun           => set + uf
          case vec: VectorizeUserFun => set + vec.vectorizedFunction
          case _                     => set
        }
        case ArrayFromUserFunGenerator(uf, _)   => set + uf
        case Array2DFromUserFunGenerator(uf, _) => set + uf
        case Array3DFromUserFunGenerator(uf, _) => set + uf
        case _                                  => set
      })

    userFuns.toSeq.map(OpenCLGenerator.createFunctionDefinition)
  }


  def allocateMemory(f: Lambda): Unit = {
    OpenCLMemoryAllocator(f)
    Kernel.memory = CollectTypedOpenCLMemory.asFlatSequence(f)
  }

  private object Kernel {
    var memory = Seq.empty[TypedOpenCLMemory]
    var staticLocalMemory = Seq.empty[TypedOpenCLMemory]
  }


  private def generateKernel(f: Lambda): Declaration = {

    val someMemories = OpenCLGenerator.getDifferentMemories(f)

    val typedValueMems = someMemories._1
    this.privateMems = someMemories._2
    this.varDecls = someMemories._3

    val memories = OpenCLGenerator.getMemories(f)

    Kernel.memory = memories._2
    Kernel.staticLocalMemory = memories._1

    f.params.foreach(_.mem.readOnly = true)

    // array of all unique vars (like N, iterSize, etc. )
    val allVars = Kernel.memory.map(_.mem.size.varList)
      .filter(_.nonEmpty).flatten.distinct
    // partition into iteration variables and all others variables
    val (iterateVars, vars) = allVars.partition(_.name == Iterate.varName)

    val attribute =
      if (localSize.forall(_.isEvaluable) &&
        f.body.contains({ case FunCall(MapWrg(_, _), _) => }))
        Some(RequiredWorkGroupSize(localSize))
      else None

    // Create the actual kernel function
    val kernel = OpenCLAST.Function(
      name = "KERNEL",
      ret = UndefType, // = void
      params =
        Kernel.memory.map(x =>
          OpenCLAST.ParamDecl(
            x.mem.variable.toString,
            x.t match { // Only de-vectorise arrays
              case vt: VectorType => vt
              case t              => Type.devectorize(t)
            },
            const = x.mem.readOnly,
            addressSpace = x.mem.addressSpace
          )
        ).toList ++
          // size parameters
          vars.sortBy(_.name).map(x => OpenCLAST.ParamDecl(x.toString, Int)),
      body = OpenCLAST.Block(Vector.empty),
      kernel = true, attribute)

    // print out allocated memory sizes
    val varMap = iterateVars.map(v => (v, ArithExpr.asCst(v.range.max))).toMap
    Kernel.memory.foreach(mem => {
      val m = mem.mem
      if (Verbose()) {
        println("Allocated " + ArithExpr.substitute(m.size, varMap.toMap) +
          " bytes for variable " + Printer.toString(m.variable) +
          " in " + m.addressSpace + " memory")
      }
    })

    kernel.body += OpenCLAST.Comment("Static local memory")
    Kernel.staticLocalMemory.foreach(x =>
      kernel.body +=
        OpenCLAST.VarDecl(x.mem.variable, x.t,
          addressSpace = x.mem.addressSpace,
          length = (x.mem.size /^ Type.getMaxAllocatedSize(Type.getBaseType(x.t))).eval))

    kernel.body += OpenCLAST.Comment("Typed Value memory")
    typedValueMems.foreach(x =>
      kernel.body +=
        OpenCLAST.VarDecl(x.mem.variable,
          Type.getValueType(x.t),
          addressSpace = x.mem.addressSpace))

    kernel.body += OpenCLAST.Comment("Private Memory")
    privateMems.foreach(x => {

      val length =
        (x.mem.size /^ Type.getMaxAllocatedSize(Type.getValueType(x.t))).enforceSimplification

      if (!length.isEvaluable)
        throw new IllegalKernel("Private memory length has to be" +
          s" evaluable, but found $length")

      val decl = OpenCLAST.VarDecl(x.mem.variable, x.t,
        addressSpace = x.mem.addressSpace,
        length = length.eval)

      privateDecls += x.mem.variable -> decl

      kernel.body += decl
    })

    generate(f.body, kernel.body)

    if (CSE())
      CommonSubexpressionElimination(kernel.body)

    kernel
  }


  private def generate(expr: Expr, block: Block): Unit = {
    assert(expr.t != UndefType)

    expr match {
      case f: FunCall => f.args.foreach(generate(_, block))
      case _          =>
    }

    expr match {
      case call: FunCall        => call.f match {
        case am: AbstractMap =>
          am match {
            case m: MapWrg     => generateMapWrgCall(m, call, block)
            case m: MapGlb     => generateMapGlbCall(m, call, block)
            case m: MapAtomWrg => generateMapAtomWrgCall(m, call, block)
            case m: MapLcl     => generateMapLclCall(m, call, block)
            case m: MapAtomLcl => generateMapAtomLclCall(m, call, block)
            case m: MapWarp    => generateMapWarpCall(m, call, block)
            case m: MapLane    => generateMapLaneCall(m, call, block)
            case m: MapSeq     => generateMapSeqCall(m, call, block)
            case _: Map        =>
          }
          // If the size of the input array is not known in the type, it is not
          // in the type of the output array either. Therefore, we have to copy
          // it from the input's header to the output's header.
          call.t match {
            case ArrayTypeWS(_, _) =>
            case _                 =>
              val inSizeView = call.args.head.view.size()
              // the output is accessed the same way as the input but the variable nam has to be replaced
              val outSizeView = inSizeView.replaced(call.args.head.mem.variable, call.mem.variable)
              (block: Block) += AssignmentExpression(
                to = accessNode(call.mem.variable, call.addressSpace, outSizeView),
                value = getArraySize(OpenCLMemory.asOpenCLMemory(call.args.head.mem), inSizeView)
              )
          }

        case f: FilterSeq => generateFilterSeqCall(f, call, block)

        case iss: InsertionSortSeq => generateInsertSortSeqCall(iss, call, block)

        case r: ReduceSeq      => generateReduceSeqCall(r, call, block)
        case r: ReduceWhileSeq => generateReduceWhileCall(r, call, block)

        case sp: MapSeqSlide => generateMapSeqSlideCall(sp, call, block)

        case bs: BSearch => generateBSearchCall(bs, call, block)
        case ls: LSearch => generateLSearchCall(ls, call, block)
        case _: Search   =>

        case scan: ScanSeq => generateScanSeqCall(scan, call, block)

        case i: Iterate => generateIterateCall(i, call, block)

        case vec: VectorizeUserFun => generateUserFunCall(vec.vectorizedFunction, call, block)
        case u: UserFun            => generateUserFunCall(u, call, block)

        case fp: FPattern                 => generate(fp.f.body, block)
        case l: Lambda                    => generate(l.body, block)
        case ua: UnsafeArrayAccess        => generateUnsafeArrayAccess(ua, call, block)
        case ca: CheckedArrayAccess       => generateCheckedArrayAccess(ca, call, block)
        case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
             Split(_) | Join() | Slide(_, _) | Zip(_) | Tuple(_) | Filter() |
             Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Pad(_, _, _) | PadConstant(_, _, _) |
             ArrayAccess(_) | PrintType(_,_) =>
        case _ => (block: Block) += OpenCLAST.Comment("__" + call.toString + "__")
      }
      case v: Value             => generateValue(v, block)
      case _: Param             =>
      case _: ArrayConstructors =>
    }
  }

  // === Maps ===

  // MapWrg
  private def generateMapWrgCall(m: MapWrg,
                                 call: FunCall,
                                 block: Block): Unit = {
    generateForLoop(block, call.args.head, m.loopVar, generate(m.f.body, _))
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
    generateForLoop(block, call.args.head, m.loopVar, generate(m.f.body, _))
    // TODO: This assumes, that the MapGlb(0) is always the outermost and there
    // is no need for synchronization inside.
    // TODO: Rethink and then redesign this!
    // if (m.dim == 0) {
    //  oclPrinter.println("return;")
    // }
  }

  private def generateMapAtomWrgCall(m: MapAtomWrg,
                                     call: FunCall,
                                     block: Block): Unit = {
    // build a new nested block
    val nestedBlock = OpenCLAST.Block(Vector.empty)
    (block: Block) += Comment("atomic_workgroup_map")

    // get shorthands for the loopvar/workvar
    val loopVar = m.loopVar
    val workVar = m.workVar
    // val threadid = new get_local_id(0 : Int)
    val threadid = get_local_id(0)

    // wrap the task update/getting functionality in a variable, as we need to use it twice
    def atomicGetTask(ob: Block): Unit = {
      generateConditional(ob, Predicate(threadid, 0, Predicate.Operator.==),
        (b) => {
          (b: Block) += AssignmentExpression(OpenCLAST.ArithExpression(loopVar),
            FunctionCall("atomic_inc", List(OpenCLAST.VarRef(workVar))))
        }, (_) => {})
    }

    // declare a global variable holding the next index to process, and assign it a value
    nestedBlock += OpenCLAST.VarDecl(workVar, opencl.ir.IntPtr,
      OpenCLAST.ArithExpression(m.globalTaskIndex.variable), addressSpace = GlobalMemory)
    // initialise it to zero
    //    generateConditional(nestedBlock, Predicate(new get_global_id(0), 0, Predicate.Operator.==),
    //      (b) => {
    //        b += OpenCLAST.AssignmentExpression(
    //          OpenCLAST.FunctionCall("*",List(OpenCLAST.Expression(workVar))), OpenCLAST.Expression(0))
    //      }, (_) => {}
    //    )

    // declare an index for this thread, the loop variable, and give it a value from the task index
    // we only wish for the first thread in the workgroup to perform the operation
    nestedBlock += OpenCLAST.VarDecl(loopVar, opencl.ir.Int, addressSpace = LocalMemory)
    atomicGetTask(nestedBlock)
    nestedBlock += OpenCLAST.Barrier(OpenCLMemory(workVar, 4, LocalMemory))
    // get the loop variable as a range variable
    val range = loopVar.range.asInstanceOf[RangeAdd]
    // generate a while loop which increments the task index atomically, while
    // it's less than the maximum range of the loop variable
    generateWhileLoop(nestedBlock,
      Predicate(loopVar, range.stop, Predicate.Operator.<),
      (b) => {
        generate(m.f.body, b)
        atomicGetTask(b)
        (b: Block) += OpenCLAST.Barrier(OpenCLMemory(workVar, 4, LocalMemory))
      })
    (block: Block) += nestedBlock

    // emit a barrier?
    if (m.emitBarrier)
      (block: Block) += OpenCLAST.Barrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // MapLcl
  private def generateMapLclCall(m: MapLcl,
                                 call: FunCall,
                                 block: Block): Unit = {
    generateForLoop(block, call.args.head, m.loopVar, generate(m.f.body, _), m.shouldUnroll)

    if (m.emitBarrier)
      (block: Block) += OpenCLAST.Barrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // MapAtomLcl
  private def generateMapAtomLclCall(m: MapAtomLcl,
                                     call: FunCall,
                                     block: Block): Unit = {

    // build a new nested block
    val nestedBlock = Block(Vector.empty)
    (block: Block) += OpenCLAST.Comment("atomic_local_map")

    // get shorthands for the loopvar/workvar
    val loopVar = m.loopVar
    val workVar = m.workVar
    val workVarPtr = OpenCLAST.FunctionCall("&", List(OpenCLAST.VarRef(workVar)))

    // declare a local variable holding the next index to process, and assign it a value
    // this must be done in a separate statement, as the variable is in LocalMemory
    nestedBlock += OpenCLAST.VarDecl(workVar, opencl.ir.Int, addressSpace = LocalMemory)
    nestedBlock += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(workVar), OpenCLAST.ArithExpression(0))

    // declare an index for this thread, the loop variable, and give it a value from the task index
    nestedBlock += OpenCLAST.VarDecl(loopVar, opencl.ir.Int,
      OpenCLAST.FunctionCall("atomic_inc", List(workVarPtr))
    )

    // get the loop variable as a range variable
    val range = loopVar.range.asInstanceOf[RangeAdd]

    // generate a while loop which increments the task index atomically, while
    // it's less than the maximum range of the loop variable
    generateWhileLoop(nestedBlock,
      Predicate(loopVar, range.stop, Predicate.Operator.<),
      (b) => {
        generate(m.f.body, b)
        (b: Block) += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(loopVar),
          OpenCLAST.FunctionCall("atomic_inc", List(workVarPtr))
        )
      })
    (block: Block) += nestedBlock

    // emit a barrier?
    if (m.emitBarrier)
      (block: Block) += OpenCLAST.Barrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // MapWarp
  private def generateMapWarpCall(m: MapWarp,
                                  call: FunCall,
                                  block: Block): Unit = {
    generateForLoop(block, call.args.head, m.loopVar, generate(m.f.body, _))
    call.mem match {
      case m: OpenCLMemory => (block: Block) += OpenCLAST.Barrier(m)
      case _               =>
    }
  }

  // MapLane
  private def generateMapLaneCall(m: MapLane,
                                  call: FunCall,
                                  block: Block): Unit = {
    generateForLoop(block, call.args.head, m.loopVar, generate(m.f.body, _))
  }

  // MapSeq
  private def generateMapSeqCall(m: MapSeq,
                                 call: FunCall,
                                 block: Block): Unit = {
    (block: Block) += OpenCLAST.Comment("map_seq")
    generateForLoop(block, call.args.head, m.loopVar, generate(m.f.body, _), m.shouldUnroll)
    (block: Block) += OpenCLAST.Comment("end map_seq")
  }

  // === Filter ===
  private def generateFilterSeqCall(f: FilterSeq,
                                    call: FunCall,
                                    block: Block): Unit = {

    (block: Block) += OpenCLAST.Comment("filter_seq")

    // Declare the index for the output array as a local variable
    (block: Block) += OpenCLAST.VarDecl(f.loopWrite, opencl.ir.Int, ArithExpression(0))

    // Code to be generated if the predicate is satisfied
    def copyAndIncrementIndex(block: Block): Unit = {
      // 1. Store the input value at "the top" of the output array
      (block: Block) += generateSeqCopy(
        call.args.head.mem, call.args.head.view.access(f.loopRead),
        call.mem, call.view.access(f.loopWrite),
        call.t.asInstanceOf[ArrayType].elemT
      )
      // 2. Increment the index of "the top" of the output array
      (block: Block) += AssignmentExpression(
        ArithExpression(f.loopWrite),
        ArithExpression(
          f.loopWrite
            + f.loopWrite.range.asInstanceOf[RangeAdd].step)
      )
    }

    def generateBody(block: Block): Unit = {
      generate(f.f.body, block) // Evaluates the predicate
      generateConditional(
        block,
        generateLoadNode(
          OpenCLMemory.asOpenCLMemory(f.f.body.mem),
          f.f.body.t,
          f.f.body.view
        ),
        copyAndIncrementIndex,
        _ => ()
      )
    }

    generateForLoop(block, call.args.head, f.loopRead, generateBody)

    // Write the header of the output array
    (block: Block) += AssignmentExpression(
      to = accessNode(call.mem.variable, call.addressSpace, call.view.size()),
      value = VarRef(f.loopWrite)
    )

    (block: Block) += OpenCLAST.Comment("end filter_seq")
  }

  // === Sorting ===
  private def generateInsertSortSeqCall(iss: InsertionSortSeq,
                                        call: FunCall,
                                        block: Block): Unit = {
    (block: Block) += OpenCLAST.Comment("insertion sort")

    generateForLoop(block, call.args.head, iss.loopRead, generateInsertion(call, _))

    (block: Block) += OpenCLAST.Comment("end insertion sort")
  }

  private def generateInsertion(call: FunCall, block: Block): Unit = {
    val iss = call.f.asInstanceOf[InsertionSortSeq]
    val i = iss.loopRead
    val j = iss.loopWrite
    val jStep = j.range.asInstanceOf[RangeAdd].step

    (block: Block) += OpenCLAST.VarDecl(
      j, Int,
      ArithExpression(i - i.range.asInstanceOf[RangeAdd].step)
    )

    /**
     * out[j+1] = out[j];
     * j = j - 1;
     */
    def shift(block: Block): Unit = {
      (block: Block) += generateSeqCopy(
        call.mem, call.view.access(j),
        call.mem, call.view.access(j + jStep),
        iss.f.params.head.t
      )
      (block: Block) += AssignmentExpression(
        ArithExpression(j),
        ArithExpression(j - jStep)
      )
    }


    def generateBody(block: Block): Unit = {
      // Compare out[j-1] and in[i]
      generate(iss.f.body, block)
      // Shift or insert
      val comp = generateLoadNode(
        OpenCLMemory.asOpenCLMemory(iss.f.body.mem),
        iss.f.body.t,
        iss.f.body.view
      )
      generateConditional(block, comp, shift, (_: Block) += OpenCLAST.Break())
    }

    generateWhileLoop(
      block,
      Predicate(j, Cst(0), Predicate.Operator.>=),
      generateBody
    )
    (block: Block) += generateSeqCopy(
      iss.f.params.head.mem, iss.f.params.head.view,
      call.mem, call.view.access(j + jStep),
      iss.f.params.head.t
    )
  }

  // === Reduce ===
  private def generateReduceSeqCall(r: ReduceSeq,
                                    call: FunCall,
                                    block: Block): Unit = {

    val innerBlock = OpenCLAST.Block(Vector.empty)
    (block: Block) += OpenCLAST.Comment("reduce_seq")

    generateForLoop(block, call.args(1), r.loopVar, generate(r.f.body, _), r.shouldUnroll)

    (block: Block) += OpenCLAST.Comment("end reduce_seq")
  }


  private def generateMapSeqSlideCall(sp: MapSeqSlide,
                                      call: FunCall,
                                      block: Block): Unit = {
    (block: Block) += OpenCLAST.Comment("slideSeq_plus")
    generateMapSeqSlideLoop(block, sp, call, generate(sp.f.body, _), sp.shouldUnroll)
    (block: Block) += OpenCLAST.Comment("end slideSeq_plus")
  }

  // === ReduceWhile ===
  private def generateReduceWhileCall(r: ReduceWhileSeq,
                                      call: FunCall,
                                      block: Block): Unit = {
    val innerBlock = OpenCLAST.Block(Vector.empty)
    (block: Block) += OpenCLAST.Comment("reduce_while_seq")

    // get the memory address of the predicate result
    val pResMem = generateLoadNode(OpenCLMemory.asOpenCLMemory(r.pmem), r.p.body.t, r.p.body.view)

    val generateBody = (ib: Block) => {
      // generate the Predicate
      generate(r.p.body, ib)
      // generate the access and break
      generateConditional(ib,
        pResMem,
        _ => {},
        (ccb) => {
          (ccb: Block) += OpenCLAST.Break()
        }
      )
      // generate the body
      generate(r.f.body, ib)
    }

    generateForLoop(block, call.args(1), r.loopVar, generateBody(_), r.shouldUnroll)

    (block: Block) += OpenCLAST.Comment("end reduce_while_seq")
  }


  // BSearch
  private def generateBSearchCall(s: AbstractSearch,
                                  call: FunCall,
                                  block: Block): Unit = {
    val nestedBlock = OpenCLAST.Block(Vector.empty)
    (block: Block) += OpenCLAST.Comment("binary_search")
    // get the default value handily
    val defaultVal = call.args.head
    // get the input handily
    val inArr = call.args(1)
    // get the type of the input (handily, concretely)
    val inArrT = inArr.t.asInstanceOf[ArrayType with Size with Capacity]
    // get an opencl version of the input mem
    val clInArrMem = OpenCLMemory.asOpenCLMemory(inArr.mem)
    // get a reference to it for loading
    val inArrRef = generateLoadNode(clInArrMem, inArrT, inArr.view.access(s.indexVar))
    // declare temporary vars for declaring the upper and lower indicies of the search
    val lowerIndex = Var("li")
    val upperIndex = Var("ui")
    // declare the lower and upper indicies
    nestedBlock += OpenCLAST.VarDecl(lowerIndex, opencl.ir.Int)
    nestedBlock += OpenCLAST.VarDecl(upperIndex, opencl.ir.Int)
    nestedBlock += OpenCLAST.VarDecl(s.indexVar, opencl.ir.Int)
    // assign initial values
    nestedBlock += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(lowerIndex), OpenCLAST.ArithExpression(0))
    nestedBlock += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(upperIndex), OpenCLAST.ArithExpression(inArrT.size))
    // Declare a variable to copy the result of the user function into
    // We have to do this, as we currently have no nice way of describing normal C statements
    // in a way that works private memory properly.
    // TODO: Find some way of representing the arguments to while/if/etc...
    val compFuncResVar = Var("cmp_res_var")
    varDecls = varDecls.updated(compFuncResVar, Type.devectorize(s.f.body.t))
    // create a memory object for it
    val compFuncResMem = OpenCLMemory(compFuncResVar, Type.getAllocatedSize(s.f.body.t), PrivateMemory)
    // set the memory of the call to the mem
    s.f.body.mem = compFuncResMem
    // declare it, with the same type as the comparison result
    nestedBlock += OpenCLAST.VarDecl(compFuncResVar, s.f.body.t)

    // create a variable for each goto label
    val finishLabel = Var("done")
    val writeResultLabel = Var("writeresult")
    generateWhileLoop(nestedBlock, Predicate(lowerIndex, upperIndex, Predicate.Operator.<),
      (b) => {
        (b: Block) += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(s.indexVar),
          OpenCLAST.ArithExpression(lowerIndex + (upperIndex - lowerIndex) / 2))

        generate(s.f.body, b)

        generateConditional(b,
          Predicate(compFuncResVar, 0, Predicate.Operator.<),
          (cb) => {
            (cb: Block) += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(upperIndex), OpenCLAST.ArithExpression(s.indexVar))
          },
          (cb) => {
            generateConditional(cb,
              Predicate(compFuncResVar, 0, Predicate.Operator.>),
              (ccb) => {
                (ccb: Block) += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(lowerIndex), OpenCLAST.ArithExpression(s.indexVar + 1))
              },
              (ccb) => {
                (ccb: Block) += OpenCLAST.GOTO(writeResultLabel)
              }
            )
          }
        )
      }
    )
    nestedBlock += generateStoreNode(OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.outputView.access(Cst(0)),
      generateLoadNode(OpenCLMemory.asOpenCLMemory(defaultVal.mem), defaultVal.t, defaultVal.view))
    nestedBlock += OpenCLAST.GOTO(finishLabel)
    nestedBlock += OpenCLAST.Label(writeResultLabel)
    nestedBlock += generateStoreNode(
      OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.outputView.access(Cst(0)),
      inArrRef)
    nestedBlock += OpenCLAST.Label(finishLabel)
    (block: Block) += nestedBlock
    (block: Block) += OpenCLAST.Comment("binary_search")
  }

  // LSearch
  private def generateLSearchCall(s: AbstractSearch,
                                  call: FunCall,
                                  block: Block): Unit = {
    val nestedBlock = OpenCLAST.Block(Vector.empty)
    (block: Block) += OpenCLAST.Comment("linear_search")
    // get the default value handily
    val defaultVal = call.args.head
    // get the input handily
    val inArr = call.args(1)
    // get the type of the input (handily, concretely)
    val inArrT = inArr.t.asInstanceOf[ArrayType with Size with Capacity]
    // get an opencl version of the input mem
    val clInArrMem = OpenCLMemory.asOpenCLMemory(inArr.mem)
    // get a reference to it for loading
    val inArrRef = generateLoadNode(clInArrMem, inArrT, inArr.view.access(s.indexVar))
    // declare the index var
    nestedBlock += OpenCLAST.VarDecl(s.indexVar, opencl.ir.Int)
    // assign initial values
    nestedBlock += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(s.indexVar), OpenCLAST.ArithExpression(0))
    // Declare a variable to copy the result of the user function into
    // We have to do this, as we currently have no nice way of describing normal C statements
    // in a way that works private memory properly.
    // TODO: Find some way of representing the arguments to while/if/etc...
    val compFuncResVar = Var("cmp_res_var")
    varDecls = varDecls.updated(compFuncResVar, Type.devectorize(s.f.body.t))
    // create a memory object for it
    val compFuncResMem = OpenCLMemory(compFuncResVar, Type.getAllocatedSize(s.f.body.t), PrivateMemory)
    // set the memory of the call to the mem
    s.f.body.mem = compFuncResMem
    // declare it, with the same type as the comparison result
    nestedBlock += OpenCLAST.VarDecl(compFuncResVar, s.f.body.t)
    // get an AST node describing a load from the comparator function result
    // val cmpResMemVar = s.f.body.mem.variable
    // create a variable for each goto label
    val finishLabel = Var("done")
    val writeResultLabel = Var("writeresult")
    val searchFailedLabel = Var("searchfailed")

    // todo - need to simplify inArrT.len, as currently it evaluates to unexecutable code on some tests
    generateWhileLoop(nestedBlock, Predicate(s.indexVar, inArrT.size, Predicate.Operator.<),
      (b) => {

        generate(s.f.body, b)

        generateConditional(b,
          // if the result of the comparator is greater than zero, the element we're currently
          // comparing to is smaller than the element we are searching for
          Predicate(compFuncResVar, 0, Predicate.Operator.>),
          (cb) => {
            // therefore, keep searching!
            (cb: Block) += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(s.indexVar), OpenCLAST.ArithExpression(s.indexVar + 1))
          },
          (cb) => {
            // else...
            generateConditional(cb,
              // if the result is less than 0, we've gone past the value we're looking for, so abort
              Predicate(compFuncResVar, 0, Predicate.Operator.<),
              // if the value is greater than, it's gone past! the search has failed.
              (ccb) => {
                (ccb: Block) += OpenCLAST.GOTO(searchFailedLabel)
              },
              // otherwise, it must be equal to, so jump to returning the result
              (ccb) => {
                (ccb: Block) += OpenCLAST.GOTO(writeResultLabel)
              }
            )
          }
        )
      }
    )
    nestedBlock += OpenCLAST.Label(searchFailedLabel)
    nestedBlock += generateStoreNode(OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.outputView.access(Cst(0)),
      generateLoadNode(OpenCLMemory.asOpenCLMemory(defaultVal.mem), defaultVal.t, defaultVal.view))
    nestedBlock += OpenCLAST.GOTO(finishLabel)
    nestedBlock += OpenCLAST.Label(writeResultLabel)
    nestedBlock += generateStoreNode(
      OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.outputView.access(Cst(0)),
      inArrRef)
    nestedBlock += OpenCLAST.Label(finishLabel)
    (block: Block) += nestedBlock
    (block: Block) += OpenCLAST.Comment("linear_search")
  }

  // ScanSeqCall
  private def generateScanSeqCall(scan: ScanSeq,
                                  call: FunCall,
                                  block: Block): Unit = {
    (block: Block) += OpenCLAST.Comment("scan_seq")
    generateForLoop(block, call.args(1), scan.loopVar, block => {
      generate(scan.f.body, block)
      val copyNode = generateSeqCopy(
        scan.f.params.head.mem,
        scan.f.params.head.view,
        call.mem,
        call.view.access(scan.loopVar),
        scan.f.body.t, scan.f.params.head.addressSpace == PrivateMemory
      )
      (block: Block) += copyNode
    }, needUnroll = scan.shouldUnroll)
    (block: Block) += OpenCLAST.Comment("end scan_seq")
  }

  private def generateUnsafeArrayAccess(ua: UnsafeArrayAccess,
                                        call: FunCall,
                                        block: Block): Unit = {
    val index = ua.index
    val clIndexMem = OpenCLMemory.asOpenCLMemory(index.mem)

    val loadIndex = generateLoadNode(clIndexMem, index.t, index.view)

    val indexVar = Var("index")
    (block: Block) += OpenCLAST.VarDecl(indexVar, Int, init = loadIndex)

    val inArr = call.args(0)
    val clInArrMem = OpenCLMemory.asOpenCLMemory(inArr.mem)

    val loadFromArray = generateLoadNode(clInArrMem, inArr.t, inArr.view.access(indexVar))

    val storeToOutput = generateStoreNode(OpenCLMemory.asOpenCLMemory(call.mem), call.t,
      call.view, loadFromArray)
    (block: Block) += storeToOutput
  }

  private def generateCheckedArrayAccess(ca: CheckedArrayAccess,
                                         call: FunCall,
                                         block: Block): Unit = {
    // generate a load to get the index at which we're accessing
    val index = ca.index
    val clIndexMem = OpenCLMemory.asOpenCLMemory(index.mem)
    val loadIndex = generateLoadNode(clIndexMem, index.t, index.view)

    // and generate a load to get the "default" value that we'd otherwise return
    val default = call.args(0)
    val clDefaultMem = OpenCLMemory.asOpenCLMemory(default.mem)
    val loadDefault = generateLoadNode(clDefaultMem, default.t, default.view)

    // generate the load into an index variable
    val indexVar = Var("index")
    (block: Block) += OpenCLAST.VarDecl(indexVar, Int, init = loadIndex)

    // pre-generate the load from default/store to output
    val storeDefaultToOutput = generateStoreNode(OpenCLMemory.asOpenCLMemory(call.mem), call.t,
      call.view, loadDefault)

    // get the in array so we can use it later
    val inArr = call.args(1)
    val arrLength = Type.getLength(inArr.t)

    // generate a conditional to check the bounds of the index
    generateConditional(block,
      // Lower bound of the index var
      Predicate(indexVar, 0, Predicate.Operator.<),
      (ccb) => { // If the indexVar is less than zero it's trivially invalid (true branch)
        (ccb: Block) += storeDefaultToOutput
      },
      // otherwise, it must be equal to, so jump to returning the result
      (ccb) => {
        generateConditional(ccb,
          // Upper bound of the index var
          Predicate(indexVar, arrLength, Predicate.Operator.>=),
          (cccb) => { // If the indexVar is greater than or equal, it is also invalid (true branch)
            (cccb: Block) += storeDefaultToOutput
          },
          (cccb) => { // Otherwise, it's valid!

            val clInArrMem = OpenCLMemory.asOpenCLMemory(inArr.mem)
            val loadFromArray = generateLoadNode(clInArrMem, inArr.t, inArr.view.access(indexVar))
            val storeToOutput = generateStoreNode(OpenCLMemory.asOpenCLMemory(call.mem), call.t,
              call.view, loadFromArray)
            (cccb: Block) += storeToOutput
          }
        )
      }
    )
    // all done!
  }

  private def generateValue(v: Value, block: Block): Unit = {
    val temp = Var("tmp")

    (block: Block) += OpenCLAST.VarDecl(temp, Type.getValueType(v.t),
      init = OpenCLAST.OpenCLCode(v.value))
    (block: Block) += OpenCLAST.AssignmentExpression(
      OpenCLAST.VarRef(v.mem.variable),
      OpenCLAST.VarRef(temp))
  }

  // === Iterate ===
  private def generateIterateCall(i: Iterate,
                                  call: FunCall,
                                  block: Block): Unit = {

    val inputMem = OpenCLMemory.asOpenCLMemory(call.args.head.mem)
    val outputMem = OpenCLMemory.asOpenCLMemory(call.mem)
    val swapMem = OpenCLMemory.asOpenCLMemory(i.swapBuffer)

    // generate for loop while swapping input and output buffer after each iteration

    assert(inputMem.addressSpace == outputMem.addressSpace)

    val funCall = i.f.body match {
      case call: FunCall => call
    }

    // use the type var as the var holding the iterating size if it exists
    if (TypeVar.getTypeVars(funCall.argsType).size > 1) {
      println("size: " + TypeVar.getTypeVars(funCall.argsType).size)
      TypeVar.getTypeVars(funCall.argsType).foreach((tv) => {
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

    (block: Block) +=
      OpenCLAST.VarDecl(curOutLen, Int,
        OpenCLAST.ArithExpression(Type.getLength(call.argsType)))

    // create new temporary input and output pointers
    varDecls = varDecls.updated(i.vPtrIn, Type.devectorize(call.t))
    varDecls = varDecls.updated(i.vPtrOut, Type.devectorize(call.t))

    // ADDRSPC TYPE ptrIn = in;
    (block: Block) += OpenCLAST.VarDecl(i.vPtrIn, Type.devectorize(call.t),
      OpenCLAST.VarRef(inputMem.variable),
      outputMem.addressSpace)

    // ADDRSPC TYPE tin = (odd ? out : swap);
    (block: Block) += OpenCLAST.VarDecl(i.vPtrOut, Type.devectorize(call.t),
      init = OpenCLAST.ArithExpression(
        ((i.n % 2) ne Cst(0)) ?? outputMem.variable !! swapMem.variable),
      addressSpace = outputMem.addressSpace)

    generateForLoop(block, call.args.head, i.indexVar, (b) => {

      // modify the pointers to the memory before generating the body
      val oldInV = inputMem.variable
      val oldOutV = outputMem.variable
      inputMem.variable = i.vPtrIn // TODO: remove this, shouldn't be necessary since all the info will be in the view (however, one test fails, need to pathc up generateStore/Load to only use view information)
      outputMem.variable = i.vPtrOut // TODO: remove this

      // generate the function call in the body
      generate(funCall, b)

      // restore the pointers to memory
      inputMem.variable = oldInV
      outputMem.variable = oldOutV

      val curOutLenRef = OpenCLAST.VarRef(curOutLen)

      val innerOutputLength = Type.getLength(funCall.t)

      // tmp = tmp * outputLen / inputLen
      (b: Block) += OpenCLAST.AssignmentExpression(curOutLenRef,
        OpenCLAST.ArithExpression(innerOutputLength))


      val tinVStrRef = OpenCLAST.VarRef(i.vPtrIn)

      // tin = (tout == swap) ? swap : out
      (b: Block) += OpenCLAST.AssignmentExpression(tinVStrRef,
        OpenCLAST.ArithExpression((i.vPtrOut eq swapMem.variable) ??
          swapMem.variable !! outputMem.variable))


      val toutVStrRef = OpenCLAST.VarRef(i.vPtrOut)

      // tout = (tout == swap) ? out : swap
      (b: Block) += OpenCLAST.AssignmentExpression(toutVStrRef,
        OpenCLAST.ArithExpression((i.vPtrOut eq swapMem.variable) ??
          outputMem.variable !! swapMem.variable))

      // TODO: CD: do we really need this?? any mapLocal inside iterate *should* take care of generating a barrier
      if (outputMem.addressSpace != PrivateMemory)
        (b: Block) += OpenCLAST.Barrier(outputMem)

    } /*, i.iterationCount*/)
  }

  private def generateMapSeqSlideLoop(block: Block,
                                      sSP: MapSeqSlide,
                                      call: FunCall,
                                      generateBody: (Block) => Unit,
                                      needUnroll: Boolean = false): Unit = {
    val indexVar = sSP.loopVar
    val step = sSP.step
    val size = sSP.size
    val range = indexVar.range.asInstanceOf[RangeAdd]
    val init = ArithExpression(range.start)
    val stop = range match {
      case ra: RangeAdd => ra.stop
      case _            => throw new OpenCLGeneratorException("Cannot handle range for ForLoop: " + range)
    }

    val reuse = size - step
    val cond = BinaryExpression(indexVar, BinaryExpression.Operator.<, (stop - reuse) / step)

    val vType = call.args.head.view.access(0).t


    val nDim = ArrayType.getDimension(1, vType)

    def getNType(v: View, n: Int): Type = n match {
      case 1 => v.access(0).t
      case _ => getNType(v.access(0), n - 1)
    }

    def getWindowSize(s: Int, n: Int): Int = n match {
      case 1 => s
      case _ => s * getWindowSize(s, n - 1)
    }

    val viewType = getNType(call.args.head.view, nDim)
    val windowSize = getWindowSize(size.eval, nDim)

    val v = Value(0.0f, ArrayTypeWSWC(viewType, windowSize))
    varDecls = varDecls.updated(sSP.windowVar, Type.devectorize(call.t))
    privateMems = privateMems :+ TypedOpenCLMemory(OpenCLMemory(sSP.windowVar, windowSize, PrivateMemory), v.t)
    val varD = OpenCLAST.VarDecl(sSP.windowVar, v.t,
      init = null, PrivateMemory, windowSize)
    privateDecls += (sSP.windowVar -> varD)
    (block: Block) += varD

    var accesses: Array[Int] = Array.fill(nDim)(0) // cannot do a direct access-on-access because the ordering is wrong

    def getView(v: View, accesses: Array[Int]): View = {
      var viewReturn = v
      for (i <- 0 to accesses.length - 1) {
        viewReturn = viewReturn.access(accesses(i))
      }
      viewReturn
    }

    // initial window values are set
    def setupInitialWindowVars(idx: Int, n: Int, accesses: Array[Int]): Unit = n match {
      case 1 => for (j <- 0 to reuse.eval - 1) {
        accesses(n - 1) = j; (block: Block) += AssignmentExpression(VarRef(sSP.windowVar, suffix = s"_${j + idx}"), ViewPrinter.emit(getView(call.args.head.view, accesses)))
      }
      case _ => for (i <- 0 to size.eval - 1) {
        accesses(n - 1) = i; setupInitialWindowVars(idx + i * math.pow(size.eval, n - 1).toInt, n - 1, accesses)
      }
    }

    setupInitialWindowVars(0, nDim, accesses)

    // window values get updated at the start of the loop
    val increment = AssignmentExpression(ArithExpression(indexVar), ArithExpression(indexVar + 1))
    val innerBlock = OpenCLAST.Block(Vector.empty)
    (block: Block) += OpenCLAST.ForLoop(VarDecl(indexVar, opencl.ir.Int, init, PrivateMemory), ExpressionStatement(cond), increment, innerBlock)

    def getViewIncrement(v: View, idx: Var, accesses: Array[Int]): View = {
      var viewReturn = v
      var idxToAdd: ArithExpr = 0
      for (i <- 0 to accesses.length - 1) {
        idxToAdd = if (i == 0) {
          (idx * step.eval)
        } else {
          0
        }
        viewReturn = viewReturn.access(accesses(i) + idxToAdd)
      }
      viewReturn
    }

    def updateWindowVars(idx: Int, n: Int, accesses: Array[Int]): Unit = n match {
      case 1 => for (j <- reuse.eval to size.eval - 1) {
        accesses(n - 1) = j
        innerBlock += AssignmentExpression(VarRef(sSP.windowVar, suffix = s"_${j + idx}"), ViewPrinter.emit(getViewIncrement(call.args.head.view, indexVar, accesses)))
      }
      case _ => for (i <- 0 to size.eval - 1) {
        accesses(n - 1) = i
        updateWindowVars(idx + i * math.pow(size.eval, n - 1).toInt, n - 1, accesses)
      }
    }

    updateWindowVars(0, nDim, accesses)
    generateBody(innerBlock)

    // window values are swapped at the end of the loop
    def swapWindowVars(idx: Int, n: Int): Unit = n match {
      case 1 => for (j <- 1 to reuse.eval) {
        val newidx = j + idx + size.eval - reuse.eval - 1
        innerBlock += AssignmentExpression(VarRef(sSP.windowVar, suffix = s"_${j + idx - 1}"), VarRef(sSP.windowVar, suffix = s"_${newidx}"))
      }
      case _ => for (i <- 0 to size.eval - 1) {
        swapWindowVars(idx + i * math.pow(size.eval, n - 1).toInt, n - 1)
      }
    }

    swapWindowVars(0, nDim)

  }

  private def generateForLoop(block: Block,
                              array: Expr,
                              indexVar: Var,
                              generateBody: (Block) => Unit,
                              needUnroll: Boolean = false): Unit = {

    // if we need to unroll (e.g. because of access to private memory)
    if (needUnroll) {
      generateForLoopUnrolled(block, indexVar, generateBody)
    } else {
      // TODO: Information needed elsewhere. See analysis.ControlFlow
      // try to see if we really need a loop
      if (PerformLoopOptimisation())
        generateOptimizedForLoopRepresentations(block, array, indexVar, generateBody)
      else
        generateDefaultForLoopRepresentation(block, array, indexVar, generateBody)
    }
  }

  private def getRangeAdd(indexVar: Var) = {
    indexVar.range match {
      case r: RangeAdd => r
      case _           =>
        throw new OpenCLGeneratorException("Cannot handle range for ForLoop: " + indexVar.range)
    }
  }

  private def getIterationCount(range: RangeAdd): Int = {
    try {
      range.numVals.enforceSimplification.eval
    } catch {
      case NotEvaluableException()      =>
        throw new OpenCLGeneratorException("Trying to unroll loop, but iteration count could " +
          "not be determined statically.")
      case NotEvaluableToIntException() =>
        throw new OpenCLGeneratorException("Trying to unroll loop, " +
          "but iteration count is larger than scala.Int.MaxValue.")
    }
  }

  private def generateForLoopUnrolled(block: Block,
                                      indexVar: Var,
                                      generateBody: (Block) => Unit): Unit = {
    val range = getRangeAdd(indexVar)
    val iterationCount = getIterationCount(range)

    if (iterationCount > 0) {
      (block: Block) += OpenCLAST.Comment("unroll")

      for (i <- 0 until iterationCount) {
        replacements = replacements.updated(indexVar, i)
        val j: ArithExpr = range.min match {
          case _: OclFunction => range.min + range.step * i
          case _              => i
        }
        replacementsWithFuns = replacementsWithFuns.updated(indexVar, j)
        generateBody(block)
      }
      // cleanup
      replacements = replacements - indexVar
      replacementsWithFuns = replacementsWithFuns - indexVar

      (block: Block) += OpenCLAST.Comment("end unroll")
    } else {
      throw new OpenCLGeneratorException(s"Trying to unroll loop," +
        s"but iteration count is $iterationCount.")
    }
  }

  private def generateOptimizedForLoopRepresentations(block: Block,
                                                      array: Expr,
                                                      indexVar: Var,
                                                      generateBody: (Block) => Unit): Unit = {
    val range = getRangeAdd(indexVar)
    val init = ArithExpression(range.start)

    array.t match {
      case _: ArrayType =>
        range.numVals match {
          case Cst(0) => (block: Block) += OpenCLAST.Comment("iteration count is 0, no loop emitted")

          case Cst(1) => generateStatement(block, indexVar, generateBody, init)

          // TODO: See TestInject.injectExactlyOneIterationVariable
          // TODO: M / 128 is not equal to M /^ 128 even though they print to the same C code
          case _ if range.start.min.min == Cst(0) &&
            ArithExpr.substituteDiv(range.stop) == ArithExpr.substituteDiv(range.step) =>
            generateStatement(block, indexVar, generateBody, init)

          // TODO: See TestOclFunction.numValues and issue #62
          case _ if range.start.min.min == Cst(0) && range.stop == Cst(1) =>
            generateIfStatement(block, indexVar, generateBody, init, range.stop)

          case _ if range.numVals.min == Cst(0) && range.numVals.max == Cst(1) =>
            generateIfStatement(block, indexVar, generateBody, init, range.stop)

          case _ =>
            generateDefaultForLoopRepresentation(block, array, indexVar, generateBody)
        }
      case _            => throw new NotImplementedError() // should never get there
    }
  }

  private def generateDefaultForLoopRepresentation(block: Block,
                                                   array: Expr,
                                                   indexVar: Var,
                                                   generateBody: (Block) => Unit): Unit = {
    array.t match {

      case _: ArrayType =>
        generateDefaultForLoopForArrayType(block, array, indexVar, generateBody)

      case _ => throw new NotImplementedError() // should never get there
    }
  }

  private def getVarForSize(mem: Memory): Var = {
    OpenCLMemory.asOpenCLMemory(mem) match {
      case ms: OpenCLMemoryCollection => getVarForSize(ms.subMemories.head)
      case x                          => x.variable
    }
  }

  private def generateDefaultForLoopForArrayType(block: Block,
                                                 array: Expr,
                                                 indexVar: Var,
                                                 generateBody: (Block) => Unit): Unit = {
    val range = getRangeAdd(indexVar)
    val ty = array.t.asInstanceOf[ArrayType]

    val innerBlock = OpenCLAST.Block(Vector.empty)

    val start = ArithExpression(range.start)
    val stop = ty match {
      case _: Size => ArithExpression(range.stop)
      case _       =>
        val mem = OpenCLMemory.asOpenCLMemory(array.mem)
        val stopVar = Var("stop")
        (block: Block) += VarDecl(stopVar, Int, getArraySize(mem, array.view.size()))
        VarRef(stopVar)
    }
    val init = VarDecl(indexVar, Int, start, PrivateMemory)
    val increment = AssignmentExpression(ArithExpression(indexVar), ArithExpression(indexVar + range.step))
    val cond = BinaryExpression(ArithExpression(indexVar), BinaryExpression.Operator.<, stop)

    (block: Block) += OpenCLAST.ForLoop(init, cond, increment, innerBlock)
    generateBody(innerBlock)
  }

  /**
   * Generate an access to the size of an array (handle arrays of tuples)
   */
  private def getArraySize(mem: OpenCLMemory, view: View): Expression = {
    mem match {
      case OpenCLMemoryCollection(subMemories, _) =>
        val sizes = subMemories.zipWithIndex.map(p =>
          getArraySize(p._1, view.get(p._2))
        )
        sizes.reduce((x, y) => FunctionCall("min", List(x, y)))
      case _                                      =>
        ViewPrinter.emit(view, addressSpace = mem.addressSpace)
    }
  }

  private def generateStatement(block: Block,
                                indexVar: Var,
                                generateBody: (Block) => Unit, init: ArithExpression): Unit = {
    // one iteration
    (block: Block) += OpenCLAST.Comment("iteration count is exactly 1, no loop emitted")
    val innerBlock = OpenCLAST.Block(Vector.empty)
    innerBlock += OpenCLAST.VarDecl(indexVar, opencl.ir.Int, init, PrivateMemory)
    generateBody(innerBlock)
    (block: Block) += innerBlock
  }

  private def generateIfStatement(block: Block, indexVar: Var, generateBody: (Block) => Unit, init: ArithExpression, stop: ArithExpr): Unit = {
    (block: Block) += OpenCLAST.Comment("iteration count is exactly 1 or less, no loop emitted")
    val innerBlock = OpenCLAST.Block(Vector.empty)
    innerBlock += OpenCLAST.VarDecl(indexVar, opencl.ir.Int, init, PrivateMemory)
    (block: Block) += OpenCLAST.IfThenElse(BinaryExpression(init, BinaryExpression.Operator.<, ArithExpression(stop)), innerBlock)
    generateBody(innerBlock)
  }

  private def generateWhileLoop(block: Block,
                                loopPredicate: Predicate,
                                generateBody: (Block) => Unit): Unit = {
    val innerBlock = OpenCLAST.Block(Vector.empty)
    (block: Block) += OpenCLAST.WhileLoop(loopPredicate, body = innerBlock)
    generateBody(innerBlock)
  }

  private def generateConditional(block: Block,
                                  condition: Expression,
                                  genTrueBranch: (Block) => Unit,
                                  genFalseBranch: (Block) => Unit): Unit = {
    val trueBlock = OpenCLAST.Block(Vector.empty)
    val falseBlock = OpenCLAST.Block(Vector.empty)
    (block: Block) += OpenCLAST.IfThenElse(condition, trueBody = trueBlock, falseBody = falseBlock)
    genTrueBranch(trueBlock)
    genFalseBranch(falseBlock)
  }

  private def generateUserFunCall(u: UserFun,
                                  call: FunCall,
                                  block: Block): Block = {
    // Handle vector assignments for vector types
    val mem = OpenCLMemory.asOpenCLMemory(call.mem)
    val funcall_node = generateFunCall(call, generateLoadNodes(call.args: _*))
    val store_node = generateStoreNode(mem, call.t, call.outputView, funcall_node)
    (block: Block) += store_node

    block
  }

  @scala.annotation.tailrec
  private def generateFunCall(expr: Expr,
                              args: List[OclAstNode]): FunctionCall = {
    expr match {
      case call: FunCall => call.f match {
        case uf: UserFun          =>
          OpenCLAST.FunctionCall(uf.name, args)
        case vf: VectorizeUserFun =>
          OpenCLAST.FunctionCall(vf.vectorizedFunction.name, args)
        case l: Lambda            => generateFunCall(l.body, args)

        case _ => throw new NotImplementedError()
      }
      case _             => throw new NotImplementedError()
    }
  }

  private def getOriginalType(mem: OpenCLMemory) =
    OpenCLGenerator.getOriginalType(mem, varDecls)

  /**
   * Generate a simple or vector store.
   * This function emits a store[n] if the LHS is an array of scala types or
   * an assignment otherwise.
   */
  private def generateStoreNode(mem: OpenCLMemory,
                                currentType: Type,
                                view: View,
                                value: OclAstNode): Expression = {
    val originalType = getOriginalType(mem)

    if (Type.haveSameValueTypes(originalType, currentType)) {
      OpenCLAST.AssignmentExpression(
        to = accessNode(mem.variable, mem.addressSpace, view),
        value = value
      )
    } else {
      (originalType, currentType) match {
        // originally a scalar type in global memory, but now a vector type
        //  => emit vstore
        case (at: ArrayType, vt: VectorType)
          if Type.getValueType(at) == vt.scalarT
            && (mem.addressSpace == GlobalMemory
            || mem.addressSpace == LocalMemory) =>

          val offset = ViewPrinter.emit(view, replacementsWithFuns, mem.addressSpace) match {
            case VarRef(_, _, idx) => ArithExpression(idx.content / vt.len)
            case x                 => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
          }

          OpenCLAST.Store(OpenCLAST.VarRef(mem.variable), vt, value, offset, mem.addressSpace)
      }
    }
  }

  private def generateLoadNodes(args: Expr*): List[OclAstNode] = {
    args.map(arg => {
      val mem = OpenCLMemory.asOpenCLMemory(arg.mem)
      generateLoadNode(mem, arg.t, arg.view)
    }).toList
  }

  private def generateLoadNode(mem: OpenCLMemory, t: Type, view: View): Expression = {
    mem match {
      // we want to generate a load for a tuple constructed by a corresponding view (i.e. zip)
      case coll: OpenCLMemoryCollection =>
        if (!t.isInstanceOf[TupleType])
          throw new OpenCLGeneratorException(s"Found a OpenCLMemoryCollection for var: " +
            s"${mem.variable}, but corresponding type: $t is " +
            s"not a tuple.")
        val tt = t.asInstanceOf[TupleType]

        var args: Vector[OclAstNode] = Vector()
        for (i <- (coll.subMemories zip tt.elemsT).indices) {
          args = args :+ generateLoadNode(coll.subMemories(i), tt.elemsT(i), view.get(i))
        }

        StructConstructor(t = tt, args = args)

      // an OpenCLNullMemory object indicates that the view is not backed by memory and will directly return a value
      case OpenCLNullMemory =>
        ViewPrinter.emit(view, replacementsWithFuns)

      // not a memory collection: the default case
      case _ =>
        val currentType = t
        val originalType: Type = getOriginalType(mem)

        if (Type.haveSameValueTypes(originalType, currentType)) {
          accessNode(mem.variable, mem.addressSpace, view)
        } else {
          (originalType, currentType) match {
            // originally a scalar type, but now a vector type
            //  => emit cast
            case (st: ScalarType, vt: VectorType) if st == vt.scalarT =>
              OpenCLAST.Cast(OpenCLAST.VarRef(mem.variable), st)

            // originally an array of scalar values in global memory,
            // but now a vector type
            //  => emit vload
            case (at: ArrayType, vt: VectorType)
              if Type.getValueType(at) == vt.scalarT
                && (mem.addressSpace == GlobalMemory || mem.addressSpace == LocalMemory) =>

              val offset = ViewPrinter.emit(view, replacementsWithFuns, mem.addressSpace) match {
                case VarRef(_, _, idx) => ArithExpression(idx.content / vt.len)
                case x                 => throw new MatchError(s"Expected a VarRef but got $x.")
              }

              OpenCLAST.Load(OpenCLAST.VarRef(mem.variable), vt, offset, mem.addressSpace)

            // originally an array of scalar values in private memory,
            // but now a vector type
            //  => emit (float2)(f1, f2) primitive
            case (at: ArrayType, vt: VectorType)
              if Type.getValueType(at) == vt.scalarT && (mem.addressSpace == PrivateMemory) =>

              assert(privateMems.exists(m => m.mem == mem))

              // TODO: this seems like a very specific local solution ... find a more generic proper one

              // iterate over the range, assuming that it is contiguous
              val arraySuffixStartIndex = arrayAccessPrivateMemIndex(mem.variable, view)
              val arraySuffixStopIndex = arraySuffixStartIndex + vt.len.eval

              val seq = (arraySuffixStartIndex until arraySuffixStopIndex).map(i => {
                OpenCLAST.VarRef(mem.variable, suffix = "_" + i)
              })

              OpenCLAST.VectorLiteral(vt, seq: _*)

            // originally a vector value in private memory,
            // but now a scalar type
            //  => emit load from components
            case (vt: VectorType, st: ScalarType)
              if st == vt.scalarT && (mem.addressSpace == PrivateMemory) =>

              val componentSuffix = componentAccessVectorVar(mem.variable, view, mem.addressSpace)
              OpenCLAST.VarRef(mem.variable, suffix = componentSuffix)

            // originally an array of vector values in private memory,
            // but now a scalar type
            //  => emit load from components
            case (at: ArrayType, st: ScalarType)
              if Type.getValueType(at).isInstanceOf[VectorType]
                && Type.haveSameBaseTypes(at, st)
                && (mem.addressSpace == PrivateMemory) =>

              val actualArray = privateMems.exists(m => m.mem == mem)

              val arraySuffix =
                if (actualArray)
                  arrayAccessPrivateMem(mem.variable, view)
                else // Workaround for values
                  ""

              val componentSuffix = componentAccessVectorVar(mem.variable, view, mem.addressSpace)
              OpenCLAST.VarRef(mem.variable, suffix = arraySuffix + componentSuffix)

            // originally an array of vector values in private memory,
            // but now a different vector type
            //  => emit load from components
            case (at: ArrayType, vt: VectorType)
              if Type.getValueType(at).isInstanceOf[VectorType]
                && Type.haveSameBaseTypes(at, vt)
                && (mem.addressSpace == PrivateMemory) =>

              // TODO: this seems like a very specific local solution ... find a more generic proper one

              assert(privateMems.exists(m => m.mem == mem))

              val arraySuffix = arrayAccessPrivateMem(mem.variable, view)

              val componentSuffixStartIndex = componentAccessvectorVarIndex(mem.variable, view, mem.addressSpace)
              val componentSuffixStopIndex = componentSuffixStartIndex + vt.len.eval

              // iterate over the range, assuming that it is contiguous
              val componentSuffix = (componentSuffixStartIndex until componentSuffixStopIndex).foldLeft(".s")(_ + _)

              OpenCLAST.VarRef(mem.variable, suffix = arraySuffix + componentSuffix)

            // originally a tuple, now a value. => generate stuff like var[i]._j
            case (t: Type, _: ScalarType)
              if Type.getValueType(t).isInstanceOf[TupleType] =>
              // get tuple component and generate suffix (._j)
              val vtc = view.asInstanceOf[ViewTupleComponent]
              val suffix = s"._${vtc.i}"

              // throw away the tuple component view for generating array index [i]
              val innerView = vtc.iv

              mem.addressSpace match {
                case LocalMemory | GlobalMemory =>
                  ViewPrinter.emit(innerView, replacementsWithFuns, mem.addressSpace) match {
                    case VarRef(v, _, index) => VarRef(v, suffix, index)
                    case x                   => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
                  }

                case PrivateMemory                                 =>

                  val declaration = privateDecls.find(m => m._1 == mem.variable)
                  val arraySuffix =
                    if (declaration.isDefined && declaration.get._2.t.isInstanceOf[ArrayType]) // check if this is actually an array
                      arrayAccessPrivateMem(mem.variable, innerView)
                    else // Workaround for values
                      ""
                  OpenCLAST.VarRef(mem.variable, suffix = arraySuffix + suffix)
                case UndefAddressSpace | AddressSpaceCollection(_) =>
                  throw new IllegalArgumentException(s"Cannot load data from ${mem.addressSpace}")
              }
          }
        }
    }
  }

  /**
   * Create an access node(i.e. of type VarRef) for variable v based on the
   * given address space and view
   *
   * @param v The variable to access
   * @param addressSpace The address space, i.e. global, local, private
   * @param view The view to access var `v`
   * @return An VarRef node accessing `v` as described in `view`.
   */
  private def accessNode(v: Var,
                         addressSpace: OpenCLAddressSpace,
                         view: View): OpenCLAST.Expression = {
    addressSpace match {
      case LocalMemory | GlobalMemory =>
        val originalType = varDecls(v)
        originalType match {
          case _: ArrayType                                 => arrayAccessNode(v, addressSpace, view)
          case _: ScalarType | _: VectorType | _: TupleType => valueAccessNode(v)
          case NoType | UndefType                           =>
            throw new TypeException(originalType, "A valid type", null)
        }

      case PrivateMemory                                 =>
        privateMems.find(m => m.mem.variable == v) match {
          case Some(typedMemory) => typedMemory.t match {
            case _: ArrayType                                 => arrayAccessNode(v, addressSpace, view)
            case _: ScalarType | _: VectorType | _: TupleType => valueAccessNode(v)
            case NoType | UndefType                           =>
              throw new TypeException(typedMemory.t, "A valid type", null)
          }
          case _                 => valueAccessNode(v)
        }
      case UndefAddressSpace | AddressSpaceCollection(_) =>
        throw new IllegalArgumentException(s"Cannot store data to $addressSpace")
    }
  }

  /**
   * Accessing v as an array
   *
   * @param v The variable to access
   * @param addressSpace The address space `v` lives in
   * @param view The view describing the access
   * @return An VarRef node accessing `v` as described in `view`.
   */
  private def arrayAccessNode(v: Var,
                              addressSpace: OpenCLAddressSpace,
                              view: View): OpenCLAST.Expression = {
    addressSpace match {
      case LocalMemory | GlobalMemory =>
        ViewPrinter.emit(view, replacementsWithFuns, addressSpace)

      case PrivateMemory =>
        ViewPrinter.emit(view, replacementsWithFuns, addressSpace) match {
          case VarRef(_, _, _) =>
            OpenCLAST.VarRef(v, suffix = arrayAccessPrivateMem(v, view))
          case e: Expression   => e
        }

      case UndefAddressSpace | AddressSpaceCollection(_) =>
        throw new IllegalArgumentException(s"Cannot load data from $addressSpace")
    }
  }

  /**
   * Generating the suffix appended to emulate an array access in private memory
   *
   * @param v The variable to access
   * @param view The view describing the access
   * @return A string of the form '_index' where index is the computed
   * array index. The index must be computable at compile time.
   */
  private def arrayAccessPrivateMem(v: Var, view: View): String = {
    // Compute the index ...
    val index = arrayAccessPrivateMemIndex(v, view)
    // ... and append it
    "_" + Printer.toString(index)
  }

  private def arrayAccessPrivateMemIndex(v: Var, view: View): Int = {
    val declaration = privateDecls(v)
    val originalType = declaration.t
    val valueType = Type.getValueType(originalType)

    val i = valueType match {
      case _: ScalarType | _: TupleType => ViewPrinter.emit(view, replacements, PrivateMemory) match {
        case VarRef(_, _, idx) => idx.content
        case x                 => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
      }
      // if the original value type is a vector:
      //   divide index by vector length
      case _: VectorType                     =>
        val length = Type.getLength(Type.getValueType(originalType))
        val index = ViewPrinter.emit(view, replacements, PrivateMemory) match {
          case VarRef(_, _, idx) => idx.content
          case x                 => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
        }
        index / length
      case ArrayType(_) | NoType | UndefType =>
        throw new TypeException(valueType, "A valid non array type", null)
    }

    val real = try {
      val sbstIdx = ArithExpr.substitute(i, replacements)
      sbstIdx.eval
    } catch {
      case NotEvaluableException()      =>
        throw new OpenCLGeneratorException(s"Could not access private array, as index $i could " +
          s"not be evaluated statically (given these replacements: $replacements)")
      case NotEvaluableToIntException() =>
        throw new OpenCLGeneratorException(s"Could not access private array, as index $i is " +
          s"larger than scala.Int.MaxValue (given these replacements: $replacements)")
    }

    if (real >= declaration.length) {
      throw new OpenCLGeneratorException(s"Out of bounds access to $v with $real")
    }

    real
  }

  /**
   * Create a string representing for a component access into a vector variable
   *
   * @param v The variable to access. Must have been declared with a vector type
   * @param view The view to access this variable
   * @return OpenCL code for accessing v, e.g.: v.s0
   */
  private def componentAccessVectorVar(v: Var, view: View, as: OpenCLAddressSpace): String = {
    // Compute the index ...
    val index = componentAccessvectorVarIndex(v, view, as)
    // ... and append it
    ".s" + Printer.toString(index)
  }

  private def componentAccessvectorVarIndex(v: Var, view: View, as: OpenCLAddressSpace): Int = {
    val originalType = varDecls(v)
    val valueType = Type.getValueType(originalType)
    val i = valueType match {
      case _: VectorType                                                       =>
        val length = Type.getLength(Type.getValueType(originalType))
        val index = ViewPrinter.emit(view, replacements, as) match {
          case VarRef(_, _, idx) => idx.content
          case x                 => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
        }
        index % length
      case ArrayType(_) | NoType | ScalarType(_, _) | TupleType(_) | UndefType =>
        throw new TypeException(valueType, "VectorType", null)
    }

    ArithExpr.substitute(i, replacements).eval
  }

  /**
   * An access to a variable as a value, i.e. a direct access by name.
   *
   * @param v The variable to access
   * @return A VarRef node wrapping `v`
   */
  private def valueAccessNode(v: Var): OpenCLAST.VarRef = {
    OpenCLAST.VarRef(v)
  }

  /**
   * Generate code for *sequentially* copying data of type `ty` from a Memory
   * to an other using the provided views. We can only write in one single
   * location in memory: no memory collection and no tuple of arrays. Although,
   * reading from a memory collection is possible, for instance: copying after
   * a Zip.
   *
   * @param inMem memory location of the data to be copied
   * @param inView view explaining how to access the data to be copied
   * @param outMem memory location where to copy the data
   * @param outView view explaining how to access the destination memory
   * @param ty the type of the data to be copied.
   * @param shouldUnroll whether the generated copy loop should be unrolled          *
   * @return a piece of OpenCL code that performs the copy *sequentially*
   */
  private def generateSeqCopy(inMem: Memory, inView: View, outMem: Memory, outView: View,
                              ty: Type, shouldUnroll: Boolean = false): OpenCLAST.OclAstNode with BlockMember = {
    assert(!outMem.isInstanceOf[OpenCLMemoryCollection]) // cannot handle that: see comment above
    ty match {
      case ScalarType(_, _) | _: TupleType | _: VectorType =>
        val load = generateLoadNode(OpenCLMemory.asOpenCLMemory(inMem), ty, inView)
        generateStoreNode(OpenCLMemory.asOpenCLMemory(outMem), ty, outView, load)
      case at: ArrayType                                   =>
        val innerBlock = Block(Vector.empty)
        val length = at match {
          case s: Size => s.size
          case _       => throw new NotImplementedError()
        }
        val loopVar = Var("cp", RangeAdd(0, length, 1))

        if (shouldUnroll) {
          generateForLoopUnrolled(innerBlock, loopVar, (block) => {
            (block: Block) += generateSeqCopy(
              inMem, inView.access(loopVar),
              outMem, outView.access(loopVar),
              at.elemT, shouldUnroll
            )
          })
          innerBlock
        } else {
          innerBlock += generateSeqCopy(
            inMem, inView.access(loopVar),
            outMem, outView.access(loopVar),
            at.elemT, shouldUnroll
          )
          ForLoop(
            VarDecl(loopVar, Int, ArithExpression(0)),
            BinaryExpression(loopVar, BinaryExpression.Operator.<, length),
            AssignmentExpression(VarRef(loopVar), ArithExpression(loopVar + 1)),
            innerBlock
          )
        }
      case _                                               => throw new NotImplementedError(s"generateSeqCopy: $ty")
    }
  }
}
