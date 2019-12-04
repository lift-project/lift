package opencl.generator

import lift.profiler._
import arithmetic.TypeVar
import core.generator.Generator
import core.generator.{AstPrinter, GenericAST}
import ir._
import ir.ast._
import ir.view._
import lift.arithmetic._
import core.generator.GenericAST._
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

  def !! (f: Lambda, localSize: NDRange, globalSize: NDRange,
               valueMap: immutable.Map[ArithExpr, ArithExpr]) : MutableBlock = {

    (new OpenCLGenerator) !! (f, localSize, globalSize, valueMap)
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
    *         component all remaining memory objects (i.e. dynamically allocated local and global memory objects)
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
    val block = MutableBlock()
    if (uf.tupleTypes.length == 1)
      block += TupleAlias(uf.tupleTypes.head, "Tuple")
    else uf.tupleTypes.zipWithIndex.foreach({ case (x, i) =>
      // TODO: think about this one ...
      block += TupleAlias(x, s"Tuple$i")
    })
    block += OclCode(uf.body)

    Function(
      name = uf.name,
      ret = uf.outT,
      params = (uf.inTs, uf.paramNames).
        zipped.map((t, n) => ParamDecl(n, t)).toList,
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


//  private val openCLCodeGen = new OpenCLPrinter

  private var replacements: ValueTable = immutable.Map.empty
  private var replacementsWithFuns: ValueTable = immutable.Map.empty
  private var privateMems = Seq[TypedOpenCLMemory]()
  private var privateDecls = immutable.Map[Var, VarDeclT]()

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

  @Profile
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

    val globalBlock = MutableBlock(Vector.empty, global = true)

    val containsDouble = Expr.visitWithState(false)(f.body, {
      case (expr, state) =>
        // A `Double` may be hidden in a TupleType. We need to visit the type
        // of each expression
        var found = false
        Type.visit(expr.t, t => if (t == Double) found = true, _ => ())
        found || state
    })

    if (containsDouble) {
      globalBlock += OclExtension("cl_khr_fp64")
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

    tupleTypes.foreach(globalBlock += TypeDef(_))

    // pass 2: find and generate user and group functions
    generateUserFunctions(f.body).foreach(globalBlock += _)

    // pass 3: generate the
    globalBlock += generateKernel(f)

    // unroll private memory in the AST
    val unrollBlock = UnrollValues.unrollPrivateMemoryArrayValues(globalBlock)
    var inlineBlock = unrollBlock

    // inline structs if requested
    if(InlineStructs())
    {
      try
      {
        var hasChanged = true
        while(hasChanged )
        {
            inlineBlock = UnrollValues.inlinePrivateMemoryStructValues(inlineBlock)
            hasChanged = UnrollValues.hasChanged
        }

      } catch {
        case err : NotImplementedError => // we know about these errors and we do not allow the user to inline structs in these cases
          print(s"Warning: Cannot inline structs: ")
          println(err.getMessage())
          inlineBlock = unrollBlock
        case err : Exception => // otherwise genuine issue, throw the exception again
          throw(err)
      }
    }

    val oclstring = AstPrinter(inlineBlock)()

    if(Verbose())
      println(s"Generated AST: \n${inlineBlock}")

    oclstring
  }

  def !! (f: Lambda, localSize: NDRange, globalSize: NDRange,
    valueMap: collection.Map[ArithExpr, ArithExpr]): MutableBlock = {


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

    val (inputs, outputs, intermediateMemory, localTmps) = CollectTypedOpenCLMemory(f, includePrivate = true)
//    val globalTmps = intermediateMemory.filter(typedMem => typedMem.mem.addressSpace.isInstanceOf[GlobalMemory.type])

    if (Verbose()) {

      println("Types:")
      OpenCLGenerator.printTypes(f.body)

      println("Memory:")
      printMemories(f.body)

      println("Allocated Memory:")
      //val (inputs, outputs, intermediateMemory, localTmps) = CollectTypedOpenCLMemory(f, includePrivate = true)
      println(" inputs:")
      inputs.foreach(println(_))
      println(" outputs:")
      outputs.foreach(println(_))
      println(" global intermediate tmps:")
      intermediateMemory.foreach(println(_))
      println(" local intermediate tmps:")
      localTmps.foreach(println(_))
      println()
    }

    View(f)

    val globalBlock = MutableBlock(Vector.empty, global = true)

    val containsDouble = Expr.visitWithState(false)(f.body, {
      case (expr, state) =>
        // A `Double` may be hidden in a TupleType. We need to visit the type
        // of each expression
        var found = false
        Type.visit(expr.t, t => if (t == Double) found = true, _ => ())
        found || state
    })

    if (containsDouble) {
      globalBlock += OclExtension("cl_khr_fp64")
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

    tupleTypes.foreach(globalBlock += TypeDef(_))

    // pass 2: find and generate user and group functions
    generateUserFunctions(f.body).foreach(globalBlock += _)

    // pass 3: generate the cast
    globalBlock += generateKernel(f)

    globalBlock
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
      case call@FunCall(Map(Lambda(_, expr,_)), _*) if expr.isConcrete            =>
        throw new IllegalKernel(s"Illegal use of UserFun where it won't generate code in $call")
      case call@FunCall(Reduce(Lambda(_, expr,_)), _, _) if expr.isConcrete       =>
        throw new IllegalKernel(s"Illegal use of UserFun where it won't generate code in $call")
      case call@FunCall(PartRed(Lambda(_, expr,_)), _, _) if expr.isConcrete      =>
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
  private def generateUserFunctions(expr: Expr): Seq[DeclarationT] = {

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
    RemoveRedundantMemory(f)
    Kernel.memory = CollectTypedOpenCLMemory.asFlatSequence(f)
  }

  private object Kernel {
    var memory = Seq.empty[TypedOpenCLMemory]
    var staticLocalMemory = Seq.empty[TypedOpenCLMemory]
  }


  @Profile("OpenCL generator")
  private def generateKernel(f: Lambda): DeclarationT = {

    val someMemories = OpenCLGenerator.getDifferentMemories(f)

    val typedValueMems = someMemories._1
    this.privateMems = someMemories._2
    this.varDecls = someMemories._3

    val memories = OpenCLGenerator.getMemories(f)

    Kernel.memory = memories._2
    Kernel.staticLocalMemory = memories._1

    f.params.foreach(_.mem.readOnly = true)

    // array of all unique vars (like N, iterSize, etc. )
    val allVars = Kernel.memory.map(_.mem.size.varList).filter(_.nonEmpty).flatten.distinct
    // partition into iteration variables and all others variables
    val (iterateVars, vars) = allVars.partition(_.name == Iterate.varName)

    val attribute =
      if (localSize.forall(_.isEvaluable) &&
        f.body.contains({ case FunCall(MapWrg(_, _), _) => }))
        Some(RequiredWorkGroupSize(localSize))
      else None

    // Create the actual kernel function
    val kernel = OpenCLAST.OclFunction(
      name = "KERNEL",
      ret = UndefType, // = void
      params =
        Kernel.memory.map(x =>
          OclParamDecl(
            name = x.mem.variable.toString,
            t = x.t match { // Only de-vectorise arrays
              case vt: VectorType => vt
              case t              => Type.devectorize(t)
            },
            const = x.mem.readOnly,
            addressSpace = x.mem.addressSpace
          )
        ).toList ++
          // size parameters
          vars.sortBy(_.name).map(x => ParamDecl(x.toString, Int)),
      body = MutableBlock(Vector.empty),
      attribute = attribute,
      kernel = true
    )

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

    kernel.body += Comment("Static local memory")
    Kernel.staticLocalMemory.foreach(x =>
      kernel.body +=
        OclVarDecl(x.mem.variable, x.t,
          addressSpace = x.mem.addressSpace,
          length = (x.mem.size /^ Type.getMaxAllocatedSize(Type.getBaseType(x.t))).eval))

    kernel.body += Comment("Typed Value memory")
    typedValueMems.foreach(x =>
      kernel.body +=
        OclVarDecl(x.mem.variable,
          Type.getValueType(x.t),
          addressSpace = x.mem.addressSpace))

    kernel.body += Comment("Private Memory")
    privateMems.foreach(x => {

      val length =
        (x.mem.size /^ Type.getMaxAllocatedSize(Type.getValueType(x.t))).enforceSimplification

      if (!length.isEvaluable)
        throw new IllegalKernel("Private memory length has to be" +
          s" evaluable, but found $length")

      val decl = OclVarDecl(x.mem.variable, x.t,
        addressSpace = x.mem.addressSpace,
        length = length.eval)

      privateDecls += x.mem.variable -> decl

      kernel.body += decl
    })

    generate(f.body, kernel.body)

    if (CSE())
      kernel.copy(body = CommonSubexpressionElimination(kernel.body))
    else
      kernel
  }


  private def generate(expr: Expr, block: MutableBlock): Unit = {
    assert(expr.t != UndefType)
   // assert(expr.view != NoView)
   // assert(expr.outputView != NoView,expr.toString())

    // here new views are being created ??
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
              (block: MutableBlock) += AssignmentExpression(
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

        case dpv @ debug.PrintView(msg, _) => debugPrintView(dpv, call, msg, block)
        case fp: FPattern                 => generate(fp.f.body, block)
        case l: Lambda                    => generate(l.body, block)
        case ua: UnsafeArrayAccess        => generateUnsafeArrayAccess(ua, call, block)
        case ca: CheckedArrayAccess       => generateCheckedArrayAccess(ca, call, block)
        case debug.PrintComment(msg)      => debugPrintComment(msg, block)

        case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
             Split(_) | Join() | Slide(_, _) | Zip(_) | Concat(_) | Tuple(_) | Filter() |
             Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Pad(_, _, _) | SkipW(_, _) | PadConstant(_, _, _) |
             ArrayAccess(_) | debug.PrintType(_) | debug.PrintTypeInConsole(_) | debug.AssertType(_, _) |
             RewritingGuidePost(_) =>
        case _                            => (block: MutableBlock) += Comment("__" + call.toString + "__")
      }
      case v: Value             => generateValue(v, block)
      case _: Param             =>
      case ArrayFromExpr(e)     => generate(e, block)
      case _: ArrayConstructors =>
    }
  }

  // === Debugging primitives ===
  private def debugPrintComment(msg: String, block: MutableBlock): Unit = {
    (block: MutableBlock) += Comment(msg)
  }
  
  private def prettifyView(inlineView: String): String = {
    var printedView: String = ""
    val commas = """,(.)(?<![\d|c=|s=])|\(|\)""".r.findAllIn(inlineView)
    var level = 1
    var start = 0
    while (commas.hasNext) {
      printedView += inlineView.substring(start, commas.start)
      inlineView.charAt(commas.start) match {
        case '(' =>
          printedView += "("
          level += 1
        case ')' =>
          printedView += ")"
          level -= 1
        case _ =>
          printedView += ",\n" + "  " * level
      }
      start = commas.start + 1
      commas.next()
    }
    printedView += inlineView.substring(start, inlineView.length()) + ",\n" + "  " * level
    
    printedView
  }

  /* Prints the view as a comment in OpenCL kernel with newlines and tabs */
  private def debugPrintView(dpv: debug.PrintView, call: FunCall, msg: String, block: MutableBlock): Unit = {
    val printedView: String = prettifyView(call.args.head.view.toString)

    (block: MutableBlock) += Comment(msg + ":\n" + printedView)

    generate(dpv.f.body, block)
  }

  // === Maps ===

  // MapWrg
  private def generateMapWrgCall(m: MapWrg,
                                 call: FunCall,
                                 block: MutableBlock): Unit = {
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
                                 block: MutableBlock): Unit = {
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
                                     block: MutableBlock): Unit = {
    // build a new nested block
    val nestedBlock = MutableBlock(Vector.empty)
    (block: MutableBlock) += Comment("atomic_workgroup_map")

    // get shorthands for the loopvar/workvar
    val loopVar = m.loopVar
    val workVar = m.workVar
    // val threadid = new get_local_id(0 : Int)
    val threadid = get_local_id(0)

    // wrap the task update/getting functionality in a variable, as we need to use it twice
    def atomicGetTask(ob: MutableBlock): Unit = {
      generateConditional(ob, Predicate(threadid, 0, Predicate.Operator.==),
        (b) => {
          (b: MutableBlock) += AssignmentExpression(ArithExpression(loopVar),
            FunctionCall("atomic_inc", List(VarIdxRef(workVar))))
        }, (_) => {})
    }

    // declare a global variable holding the next index to process, and assign it a value
    nestedBlock += OclVarDecl(workVar, opencl.ir.IntPtr,
      Some(ArithExpression(m.globalTaskIndex.variable)), addressSpace =
        GlobalMemory)
    // initialise it to zero
    //    generateConditional(nestedBlock, Predicate(new get_global_id(0), 0, Predicate.Operator.==),
    //      (b) => {
    //        b += OpenCLAST.AssignmentExpression(
    //          OpenCLAST.FunctionCall("*",List(OpenCLAST.Expression(workVar))), OpenCLAST.Expression(0))
    //      }, (_) => {}
    //    )

    // declare an index for this thread, the loop variable, and give it a value from the task index
    // we only wish for the first thread in the workgroup to perform the operation
    nestedBlock += OclVarDecl(loopVar, opencl.ir.Int, addressSpace = LocalMemory)
    atomicGetTask(nestedBlock)
    nestedBlock += OclBarrier(OpenCLMemory(workVar, 4, LocalMemory))
    // get the loop variable as a range variable
    val range = loopVar.range.asInstanceOf[RangeAdd]
    // generate a while loop which increments the task index atomically, while
    // it's less than the maximum range of the loop variable
    generateWhileLoop(nestedBlock,
      Predicate(loopVar, range.stop, Predicate.Operator.<),
      (b) => {
        generate(m.f.body, b)
        atomicGetTask(b)
        (b: MutableBlock) += OclBarrier(OpenCLMemory(workVar, 4, LocalMemory))
      })
    (block: MutableBlock) += nestedBlock

    // emit a barrier?
    if (m.emitBarrier)
      (block: MutableBlock) += OclBarrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // MapLcl
  private def generateMapLclCall(m: MapLcl,
                                 call: FunCall,
                                 block: MutableBlock): Unit = {
    generateForLoop(block, call.args.head, m.loopVar, generate(m.f.body, _), m.shouldUnroll)

    if (m.emitBarrier)
      (block: MutableBlock) += OclBarrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // MapAtomLcl
  private def generateMapAtomLclCall(m: MapAtomLcl,
                                     call: FunCall,
                                     block: MutableBlock): Unit = {

    // build a new nested block
    val nestedBlock = MutableBlock(Vector.empty)
    (block: MutableBlock) += Comment("atomic_local_map")

    // get shorthands for the loopvar/workvar
    val loopVar = m.loopVar
    val workVar = m.workVar
    val workVarPtr = FunctionCall("&", List(VarIdxRef(workVar)))

    // declare a local variable holding the next index to process, and assign it a value
    // this must be done in a separate statement, as the variable is in LocalMemory
    nestedBlock += OclVarDecl(workVar, opencl.ir.Int, addressSpace = LocalMemory)
    nestedBlock += AssignmentExpression(ArithExpression(workVar), ArithExpression(0))

    // declare an index for this thread, the loop variable, and give it a value from the task index
    nestedBlock += OclVarDecl(loopVar, opencl.ir.Int,
      Some(FunctionCall("atomic_inc", List(workVarPtr)))
    )

    // get the loop variable as a range variable
    val range = loopVar.range.asInstanceOf[RangeAdd]

    // generate a while loop which increments the task index atomically, while
    // it's less than the maximum range of the loop variable
    generateWhileLoop(nestedBlock,
      Predicate(loopVar, range.stop, Predicate.Operator.<),
      (b) => {
        generate(m.f.body, b)
        (b: MutableBlock) += AssignmentExpression(ArithExpression(loopVar),
          FunctionCall("atomic_inc", List(workVarPtr))
        )
      })
    (block: MutableBlock) += nestedBlock

    // emit a barrier?
    if (m.emitBarrier)
      (block: MutableBlock) += OclBarrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // MapWarp
  private def generateMapWarpCall(m: MapWarp,
                                  call: FunCall,
                                  block: MutableBlock): Unit = {
    generateForLoop(block, call.args.head, m.loopVar, generate(m.f.body, _))
    call.mem match {
      case m: OpenCLMemory => (block: MutableBlock) += OclBarrier(m)
      case _               =>
    }
  }

  // MapLane
  private def generateMapLaneCall(m: MapLane,
                                  call: FunCall,
                                  block: MutableBlock): Unit = {
    generateForLoop(block, call.args.head, m.loopVar, generate(m.f.body, _))
  }

  // MapSeq
  private def generateMapSeqCall(m: MapSeq,
                                 call: FunCall,
                                 block: MutableBlock): Unit = {
    (block: MutableBlock) += Comment("map_seq")
    generateForLoop(block, call.args.head, m.loopVar, generate(m.f.body, _), m.shouldUnroll)
    (block: MutableBlock) += Comment("end map_seq")
  }

  // === Filter ===
  private def generateFilterSeqCall(f: FilterSeq,
                                    call: FunCall,
                                    block: MutableBlock): Unit = {

    (block: MutableBlock) += Comment("filter_seq")

    // Declare the index for the output array as a local variable
    (block: MutableBlock) += OclVarDecl(f.loopWrite, opencl.ir.Int,
      Some(ArithExpression(0)))

    // Code to be generated if the predicate is satisfied
    def copyAndIncrementIndex(block: MutableBlock): Unit = {
      // 1. Store the input value at "the top" of the output array
      (block: MutableBlock) += generateSeqCopy(
        call.args.head.mem, call.args.head.view.access(f.loopRead),
        call.mem, call.view.access(f.loopWrite),
        call.t.asInstanceOf[ArrayType].elemT
      )
      // 2. Increment the index of "the top" of the output array
      (block: MutableBlock) += AssignmentExpression(
        ArithExpression(f.loopWrite),
        ArithExpression(
          f.loopWrite
            + f.loopWrite.range.asInstanceOf[RangeAdd].step)
      )
    }

    def generateBody(block: MutableBlock): Unit = {
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
    (block: MutableBlock) += AssignmentExpression(
      to = accessNode(call.mem.variable, call.addressSpace, call.view.size()),
      value = VarIdxRef(f.loopWrite)
    )

    (block: MutableBlock) += Comment("end filter_seq")
  }

  // === Sorting ===
  private def generateInsertSortSeqCall(iss: InsertionSortSeq,
                                        call: FunCall,
                                        block: MutableBlock): Unit = {
    (block: MutableBlock) += Comment("insertion sort")

    generateForLoop(block, call.args.head, iss.loopRead, generateInsertion(call, _))

    (block: MutableBlock) += Comment("end insertion sort")
  }

  private def generateInsertion(call: FunCall, block: MutableBlock): Unit = {
    val iss = call.f.asInstanceOf[InsertionSortSeq]
    val i = iss.loopRead
    val j = iss.loopWrite
    val jStep = j.range.asInstanceOf[RangeAdd].step

    (block: MutableBlock) += OclVarDecl(
      j, Int,
      Some(ArithExpression(i - i.range.asInstanceOf[RangeAdd].step))
    )

    /**
      * out[j+1] = out[j];
      * j = j - 1;
      */
    def shift(block: MutableBlock): Unit = {
      (block: MutableBlock) += generateSeqCopy(
        call.mem, call.view.access(j),
        call.mem, call.view.access(j + jStep),
        iss.f.params.head.t
      )
      (block: MutableBlock) += AssignmentExpression(
        ArithExpression(j),
        ArithExpression(j - jStep)
      )
    }


    def generateBody(block: MutableBlock): Unit = {
      // Compare out[j-1] and in[i]
      generate(iss.f.body, block)
      // Shift or insert
      val comp = generateLoadNode(
        OpenCLMemory.asOpenCLMemory(iss.f.body.mem),
        iss.f.body.t,
        iss.f.body.view
      )
      generateConditional(block, comp, shift, (_: MutableBlock) += Break())
    }

    generateWhileLoop(
      block,
      Predicate(j, Cst(0), Predicate.Operator.>=),
      generateBody
    )
    (block: MutableBlock) += generateSeqCopy(
      iss.f.params.head.mem, iss.f.params.head.view,
      call.mem, call.view.access(j + jStep),
      iss.f.params.head.t
    )
  }

  // === Reduce ===
  private def generateReduceSeqCall(r: ReduceSeq,
                                    call: FunCall,
                                    block: MutableBlock): Unit = {

    val innerBlock = MutableBlock(Vector.empty)
    (block: MutableBlock) += Comment("reduce_seq")

    generateForLoop(block, call.args(1), r.loopVar, generate(r.f.body, _), r.shouldUnroll)

    (block: MutableBlock) += Comment("end reduce_seq")
  }


  private def generateMapSeqSlideCall(sp: MapSeqSlide,
                                      call: FunCall,
                                      block: MutableBlock): Unit = {
    (block: MutableBlock) += Comment("mapSeqSlide")
    generateMapSeqSlideLoop(block, sp, call, generate(sp.f.body, _), sp.shouldUnroll)
    (block: MutableBlock) += Comment("end mapSeqSlide")
  }

  // === ReduceWhile ===
  private def generateReduceWhileCall(r: ReduceWhileSeq,
                                      call: FunCall,
                                      block: MutableBlock): Unit = {
    val innerBlock = MutableBlock(Vector.empty)
    (block: MutableBlock) += Comment("reduce_while_seq")

    // get the memory address of the predicate result
    val pResMem = generateLoadNode(OpenCLMemory.asOpenCLMemory(r.pmem), r.p.body.t, r.p.body.view)

    val generateBody = (ib: MutableBlock) => {
      // generate the Predicate
      generate(r.p.body, ib)
      // generate the access and break
      generateConditional(ib,
        pResMem,
        _ => {},
        (ccb) => {
          (ccb: MutableBlock) += Break()
        }
      )
      // generate the body
      generate(r.f.body, ib)
    }

    generateForLoop(block, call.args(1), r.loopVar, generateBody(_), r.shouldUnroll)

    (block: MutableBlock) += Comment("end reduce_while_seq")
  }


  // BSearch
  private def generateBSearchCall(s: AbstractSearch,
                                  call: FunCall,
                                  block: MutableBlock): Unit = {
    val nestedBlock = MutableBlock(Vector.empty)
    (block: MutableBlock) += Comment("binary_search")
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
    nestedBlock += OclVarDecl(lowerIndex, opencl.ir.Int)
    nestedBlock += OclVarDecl(upperIndex, opencl.ir.Int)
    nestedBlock += OclVarDecl(s.indexVar, opencl.ir.Int)
    // assign initial values
    nestedBlock += AssignmentExpression(ArithExpression(lowerIndex), ArithExpression(0))
    nestedBlock += AssignmentExpression(ArithExpression(upperIndex), ArithExpression(inArrT.size))
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
    nestedBlock += OclVarDecl(compFuncResVar, s.f.body.t)

    // create a variable for each goto label
    val finishLabel = Var("done")
    val writeResultLabel = Var("writeresult")
    generateWhileLoop(nestedBlock, Predicate(lowerIndex, upperIndex, Predicate.Operator.<),
      (b) => {
        (b: MutableBlock) += AssignmentExpression(ArithExpression(s.indexVar),
          ArithExpression(lowerIndex + (upperIndex - lowerIndex) / 2))

        generate(s.f.body, b)

        generateConditional(b,
          Predicate(compFuncResVar, 0, Predicate.Operator.<),
          (cb) => {
            (cb: MutableBlock) += AssignmentExpression(ArithExpression(upperIndex), ArithExpression(s.indexVar))
          },
          (cb) => {
            generateConditional(cb,
              Predicate(compFuncResVar, 0, Predicate.Operator.>),
              (ccb) => {
                (ccb: MutableBlock) += AssignmentExpression(ArithExpression(lowerIndex), ArithExpression(s.indexVar + 1))
              },
              (ccb) => {
                (ccb: MutableBlock) += GOTO(writeResultLabel)
              }
            )
          }
        )
      }
    )
    nestedBlock += generateStoreNode(OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.outputView.access(Cst(0)),
      generateLoadNode(OpenCLMemory.asOpenCLMemory(defaultVal.mem), defaultVal.t, defaultVal.view))
    nestedBlock += GOTO(finishLabel)
    nestedBlock += Label(writeResultLabel)
    nestedBlock += generateStoreNode(
      OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.outputView.access(Cst(0)),
      inArrRef)
    nestedBlock += Label(finishLabel)
    (block: MutableBlock) += nestedBlock
    (block: MutableBlock) += Comment("binary_search")
  }

  // LSearch
  private def generateLSearchCall(s: AbstractSearch,
                                  call: FunCall,
                                  block: MutableBlock): Unit = {
    val nestedBlock = MutableBlock(Vector.empty)
    (block: MutableBlock) += Comment("linear_search")
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
    nestedBlock += OclVarDecl(s.indexVar, opencl.ir.Int)
    // assign initial values
    nestedBlock += AssignmentExpression(ArithExpression(s.indexVar), ArithExpression(0))
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
    nestedBlock += OclVarDecl(compFuncResVar, s.f.body.t)
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
            (cb: MutableBlock) += AssignmentExpression(ArithExpression(s.indexVar), ArithExpression(s.indexVar + 1))
          },
          (cb) => {
            // else...
            generateConditional(cb,
              // if the result is less than 0, we've gone past the value we're looking for, so abort
              Predicate(compFuncResVar, 0, Predicate.Operator.<),
              // if the value is greater than, it's gone past! the search has failed.
              (ccb) => {
                (ccb: MutableBlock) += GOTO(searchFailedLabel)
              },
              // otherwise, it must be equal to, so jump to returning the result
              (ccb) => {
                (ccb: MutableBlock) += GOTO(writeResultLabel)
              }
            )
          }
        )
      }
    )
    nestedBlock += Label(searchFailedLabel)
    nestedBlock += generateStoreNode(OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.outputView.access(Cst(0)),
      generateLoadNode(OpenCLMemory.asOpenCLMemory(defaultVal.mem), defaultVal.t, defaultVal.view))
    nestedBlock += GOTO(finishLabel)
    nestedBlock += Label(writeResultLabel)
    nestedBlock += generateStoreNode(
      OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.outputView.access(Cst(0)),
      inArrRef)
    nestedBlock += Label(finishLabel)
    (block: MutableBlock) += nestedBlock
    (block: MutableBlock) += Comment("linear_search")
  }

  // ScanSeqCall
  private def generateScanSeqCall(scan: ScanSeq,
                                  call: FunCall,
                                  block: MutableBlock): Unit = {
    (block: MutableBlock) += Comment("scan_seq")
    generateForLoop(block, call.args(1), scan.loopVar, block => {
      generate(scan.f.body, block)
      val copyNode = generateSeqCopy(
        scan.f.params.head.mem,
        scan.f.params.head.view,
        call.mem,
        call.view.access(scan.loopVar),
        scan.f.body.t, scan.f.params.head.addressSpace == PrivateMemory
      )
      (block: MutableBlock) += copyNode
    }, needUnroll = scan.shouldUnroll)
    (block: MutableBlock) += Comment("end scan_seq")
  }

  private def generateUnsafeArrayAccess(ua: UnsafeArrayAccess,
                                        call: FunCall,
                                        block: MutableBlock): Unit = {
    val index = ua.index
    val clIndexMem = OpenCLMemory.asOpenCLMemory(index.mem)

    val loadIndex = generateLoadNode(clIndexMem, index.t, index.view)

    val indexVar = Var("index")
    (block: MutableBlock) += OclVarDecl(indexVar, Int, init = Some(loadIndex))

    val inArr = call.args(0)
    val clInArrMem = OpenCLMemory.asOpenCLMemory(inArr.mem)

    val loadFromArray = generateLoadNode(clInArrMem, inArr.t, inArr.view.access(indexVar))

    val storeToOutput = generateStoreNode(OpenCLMemory.asOpenCLMemory(call.mem), call.t,
      call.view, loadFromArray)
    (block: MutableBlock) += storeToOutput
  }

  private def generateCheckedArrayAccess(ca: CheckedArrayAccess,
                                         call: FunCall,
                                         block: MutableBlock): Unit = {
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
    (block: MutableBlock) += OclVarDecl(indexVar, Int, init = Some(loadIndex))

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
        (ccb: MutableBlock) += storeDefaultToOutput
      },
      // otherwise, it must be equal to, so jump to returning the result
      (ccb) => {
        generateConditional(ccb,
          // Upper bound of the index var
          Predicate(indexVar, arrLength, Predicate.Operator.>=),
          (cccb) => { // If the indexVar is greater than or equal, it is also invalid (true branch)
            (cccb: MutableBlock) += storeDefaultToOutput
          },
          (cccb) => { // Otherwise, it's valid!

            val clInArrMem = OpenCLMemory.asOpenCLMemory(inArr.mem)
            val loadFromArray = generateLoadNode(clInArrMem, inArr.t, inArr.view.access(indexVar))
            val storeToOutput = generateStoreNode(OpenCLMemory.asOpenCLMemory(call.mem), call.t,
              call.view, loadFromArray)
            (cccb: MutableBlock) += storeToOutput
          }
        )
      }
    )
    // all done!
  }

  private def generateValue(v: Value, block: MutableBlock): Unit = {
    val temp = Var("tmp")

    (block: MutableBlock) += OclVarDecl(temp, Type.getValueType(v.t),
      init = Some(OclCode(v.value)))
    (block: MutableBlock) += AssignmentExpression(
      VarIdxRef(v.mem.variable),
      VarIdxRef(temp))
  }

  // === Iterate ===
  private def generateIterateCall(i: Iterate,
                                  call: FunCall,
                                  block: MutableBlock): Unit = {

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

    (block: MutableBlock) +=
      OclVarDecl(curOutLen, Int,
        Some(ArithExpression(Type.getLength(call.argsType))))

    // create new temporary input and output pointers
    varDecls = varDecls.updated(i.vPtrIn, Type.devectorize(call.t))
    varDecls = varDecls.updated(i.vPtrOut, Type.devectorize(call.t))

    // ADDRSPC TYPE ptrIn = in;
    (block: MutableBlock) += OclVarDecl(
      v = i.vPtrIn,
      t = Type.devectorize(call.t),
      init = Some(VarIdxRef(inputMem.variable)),
      addressSpace = outputMem.addressSpace
    )

    // ADDRSPC TYPE tin = (odd ? out : swap);
    (block: MutableBlock) += OclVarDecl(i.vPtrOut, Type.devectorize(call.t),
      init = Some(ArithExpression(
        ((i.n % 2) ne Cst(0)) ?? outputMem.variable !! swapMem.variable)),
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

      val curOutLenRef = VarIdxRef(curOutLen)

      val innerOutputLength = Type.getLength(funCall.t)

      // tmp = tmp * outputLen / inputLen
      (b: MutableBlock) += AssignmentExpression(curOutLenRef,
        ArithExpression(innerOutputLength))


      val tinVStrRef = VarIdxRef(i.vPtrIn)

      // tin = (tout == swap) ? swap : out
      (b: MutableBlock) += AssignmentExpression(tinVStrRef,
        ArithExpression((i.vPtrOut eq swapMem.variable) ??
          swapMem.variable !! outputMem.variable))


      val toutVStrRef = VarIdxRef(i.vPtrOut)

      // tout = (tout == swap) ? out : swap
      (b: MutableBlock) += AssignmentExpression(toutVStrRef,
        ArithExpression((i.vPtrOut eq swapMem.variable) ??
          outputMem.variable !! swapMem.variable))

      // TODO: CD: do we really need this?? any mapLocal inside iterate *should* take care of generating a barrier
      if (outputMem.addressSpace != PrivateMemory)
        (b: MutableBlock) += OclBarrier(outputMem)

    } /*, i.iterationCount*/)
  }

  private def generateMapSeqSlideLoop(block: MutableBlock,
                                      sSP: MapSeqSlide,
                                      call: FunCall,
                                      generateBody: (MutableBlock) => Unit,
                                      needUnroll: Boolean = false): Unit = {
    val indexVar = sSP.loopVar
    val step = sSP.step
    val size = sSP.size
    val range: RangeAdd = indexVar.range.asInstanceOf[RangeAdd]
    val init = ArithExpression(range.start)
    val reuse = size - step
    val stop = range match {
      case ra: RangeAdd => ra.stop
      case _ => throw new OpenCLGeneratorException("Cannot handle range for ForLoop: " + range)
    }
    // for indexing (3)(2)(1)(0)
    // ordering expected 2D: x y
    // ordering expected 3D: x y z
    // ordering expected 4D: w x y z  (ETC)


    val cond = BinaryExpression(ArithExpression(indexVar), BinaryExpressionT
      .Operator.<, ArithExpression(stop))

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
    varDecls = varDecls.updated(sSP.windowVar, v.t)
    privateMems = privateMems :+ TypedOpenCLMemory(sSP.f.params(0).mem, sSP.f.params(0).t)
    val varD = OpenCLAST.OclVarDecl(
      v = CVar(sSP.windowVar),
      t= v.t,
      init = None,
      length = windowSize,
      addressSpace = PrivateMemory)
    privateDecls += (sSP.windowVar -> varD)
    (block: MutableBlock) += varD

    var accesses: Array[Int] = Array.fill(nDim)(0)

    var eachWindow = 1 //  1 value for 1D or size for 2D or size*size for 3D
    for(i <- 1 until nDim)
    {
      eachWindow = eachWindow * size.eval
    }

    def getView(v: View, accesses: Array[Int]): View = {
      var viewReturn = v
      for (i <- 0 to accesses.length - 1) {
        viewReturn = viewReturn.access(accesses(i))
      }
      viewReturn
    }

    def getIndexFromAccessPoints(accesses : Array[Int]): Int =
    {
      var newIdx : Int = 0
      var newAccesses = accesses.reverse

      for ( k <- 0 to (nDim - 1))
      {
        newIdx = newIdx + newAccesses(k)*math.pow(size.eval, k).toInt
      }

      newIdx

    }

    // initial window values are set
    def setupInitialWindowVars(n: Int, accesses: Array[Int]): Unit =
    {
        n match
        {
          case `nDim` =>
            for(j <- 0 until reuse.eval)
            {
              accesses(math.abs(n-nDim)) = j
              val newIdx = getIndexFromAccessPoints(accesses)
              val argMem = OpenCLMemory.asOpenCLMemory(call.args.head.mem)
              val argViewi = getView(call.args.head.view, accesses)
              val loadi = generateLoadNode(argMem, argViewi.t, argViewi)
              (block: MutableBlock) += AssignmentExpression(VarIdxRef(sSP.windowVar,
              suffix =
                Some(s"_${newIdx}")), loadi)
            }
          case _ =>
            for (i <- 0 to size.eval - 1)
            {
              accesses(math.abs(n-nDim)) = i
              setupInitialWindowVars( n+1,accesses )
            }
        }
    }

   setupInitialWindowVars(1, accesses)

    // window values get updated at the start of the loop
    val increment = AssignmentExpression(ArithExpression(indexVar), ArithExpression(indexVar + 1))
    val innerBlock = MutableBlock(Vector.empty)
    (block: MutableBlock) += ForLoop(
      OclVarDecl(v = indexVar, t = opencl.ir.Int, init = Some(init),
        addressSpace = PrivateMemory), ExpressionStatement(cond), increment,
      innerBlock)

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

    def updateWindowVars( n: Int, accesses : Array[Int] ): Unit = {
      n match
      {
        case `nDim` =>
          for(j <- reuse.eval until size.eval)
          {
            accesses(math.abs(nDim-n)) = j
            val newIdx = getIndexFromAccessPoints(accesses)
            val argMem = OpenCLMemory.asOpenCLMemory(call.args.head.mem)
            val viewInc = getViewIncrement(call.args.head.view, indexVar, accesses)
            val loadi = generateLoadNode(argMem, viewInc.t, viewInc)
            innerBlock += AssignmentExpression(VarIdxRef(sSP.windowVar, suffix =
              Some(s"_${newIdx}")), loadi)

          }
        case _ =>
          for(i <- 0 to size.eval - 1)
          {
            accesses(math.abs(nDim-n)) = i
            updateWindowVars( n+1,accesses )
          }
      }
    }

    accesses = Array.fill(nDim)(0)

    updateWindowVars(1, accesses)
    generateBody(innerBlock)

    def getReuseSize(n: Int): Int = n match {
      case 1 => 1
      case _ => size.eval * getReuseSize(n - 1)
    }
    // window values are swapped at the end of the loop
    def swapWindowVars(idx: Int, n: Int): Unit =
    {
      val reuseSize = getReuseSize(n)

      for(i <- 0 until reuseSize)
        {
            for( j <- 1 to reuse.eval)
            {
                  val skip  = size.eval - reuse.eval - 1
                  innerBlock += AssignmentExpression(VarIdxRef(sSP.windowVar, suffix = Some(s"_${i+(j-1)*reuseSize}")), VarIdxRef(sSP.windowVar, suffix = Some(s"_${i+((skip+j)*reuseSize)}")))
            }
        }
    }

    swapWindowVars(0, nDim)

  }

  private def generateForLoop(block: MutableBlock,
                              array: Expr,
                              indexVar: Var,
                              generateBody: (MutableBlock) => Unit,
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

  private def generateForLoopUnrolled(block: MutableBlock,
                                      indexVar: Var,
                                      generateBody: (MutableBlock) => Unit): Unit = {
    val range = getRangeAdd(indexVar)
    val iterationCount = getIterationCount(range)

    if (iterationCount > 0) {
      (block: MutableBlock) += Comment("unroll")

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

      (block: MutableBlock) += Comment("end unroll")
    } else {
      throw new OpenCLGeneratorException(s"Trying to unroll loop," +
        s"but iteration count is $iterationCount.")
    }
  }

  private def generateOptimizedForLoopRepresentations(block: MutableBlock,
                                                      array: Expr,
                                                      indexVar: Var,
                                                      generateBody: (MutableBlock) => Unit): Unit = {
    val range = getRangeAdd(indexVar)
    val init = ArithExpression(range.start)

    array.t match {
      case _: ArrayType =>
        range.numVals match {
          case Cst(0) => (block: MutableBlock) += Comment("iteration count is 0, no loop emitted")

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

  private def generateDefaultForLoopRepresentation(block: MutableBlock,
                                                   array: Expr,
                                                   indexVar: Var,
                                                   generateBody: (MutableBlock) => Unit): Unit = {
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

  private def generateDefaultForLoopForArrayType(block: MutableBlock,
                                                 array: Expr,
                                                 indexVar: Var,
                                                 generateBody: (MutableBlock) => Unit): Unit = {
    val range = getRangeAdd(indexVar)
    val ty = array.t.asInstanceOf[ArrayType]

    val innerBlock = MutableBlock(Vector.empty)

    val start = ArithExpression(range.start)
    val stop = ty match {
      case _: Size => ArithExpression(range.stop)
      case _       =>
        val mem = OpenCLMemory.asOpenCLMemory(array.mem)
        val stopVar = Var("stop")
        (block: MutableBlock) += OclVarDecl(stopVar, Int, Some(getArraySize
        (mem, array.view.size())))
        VarIdxRef(stopVar)
    }
    val init = OclVarDecl(v = indexVar, t = Int, init = Some(start),
      addressSpace =
      PrivateMemory)
    val increment = AssignmentExpression(ArithExpression(indexVar), ArithExpression(indexVar + range.step))
    val cond = BinaryExpression(ArithExpression(indexVar), BinaryExpressionT
      .Operator.<, stop)

    (block: MutableBlock) += ForLoop(init, cond, increment, innerBlock)
    generateBody(innerBlock)
  }

  /**
    * Generate an access to the size of an array (handle arrays of tuples)
    */
  private def getArraySize(mem: OpenCLMemory, view: View): ExpressionT = {
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

  private def generateStatement(block: MutableBlock,
                                indexVar: Var,
                                generateBody: (MutableBlock) => Unit, init: ArithExpression): Unit = {
    // one iteration
    (block: MutableBlock) += Comment("iteration count is exactly 1, no loop emitted")
    val innerBlock = MutableBlock(Vector.empty)
    innerBlock += OclVarDecl(v = indexVar, t = opencl.ir.Int, init = Some(init),
      addressSpace = PrivateMemory)
    generateBody(innerBlock)
    (block: MutableBlock) += innerBlock
  }

  private def generateIfStatement(block: MutableBlock, indexVar: Var, generateBody: (MutableBlock) => Unit, init: ArithExpression, stop: ArithExpr): Unit = {
    (block: MutableBlock) += Comment("iteration count is exactly 1 or less, no loop emitted")
    val innerBlock = MutableBlock(Vector.empty)
    innerBlock += OclVarDecl(v = indexVar, t = opencl.ir.Int, init = Some(init),
      addressSpace =  PrivateMemory)
    (block: MutableBlock) += GenericAST.IfThenElse(
      BinaryExpression(init, BinaryExpressionT.Operator.<, ArithExpression(stop)),
      trueBody = innerBlock, falseBody = MutableBlock())
    generateBody(innerBlock)
  }

  private def generateWhileLoop(block: MutableBlock,
                                loopPredicate: Predicate,
                                generateBody: (MutableBlock) => Unit): Unit = {
    val innerBlock = MutableBlock(Vector.empty)
    (block: MutableBlock) += WhileLoop(loopPredicate, body = innerBlock)
    generateBody(innerBlock)
  }

  private def generateConditional(block: MutableBlock,
                                  condition: ExpressionT,
                                  genTrueBranch: (MutableBlock) => Unit,
                                  genFalseBranch: (MutableBlock) => Unit): Unit = {
    val trueBlock = MutableBlock(Vector.empty)
    val falseBlock = MutableBlock(Vector.empty)
    (block: MutableBlock) += GenericAST.IfThenElse(cond = condition, trueBody =
      trueBlock, falseBody = falseBlock)
    genTrueBranch(trueBlock)
    genFalseBranch(falseBlock)
  }

  private def generateUserFunCall(u: UserFun,
                                  call: FunCall,
                                  block: MutableBlock): MutableBlock = {
    // Handle vector assignments for vector types
    val mem = OpenCLMemory.asOpenCLMemory(call.mem)
    val funcall_node = generateFunCall(call, generateLoadNodes(call.args: _*))
    val store_node = generateStoreNode(mem, call.t, call.outputView, funcall_node)
    (block: MutableBlock) += store_node

    block
  }

  @scala.annotation.tailrec
  private def generateFunCall(expr: Expr,
                              args: List[AstNode]): FunctionCall = {
    expr match {
      case call: FunCall => call.f match {
        case uf: UserFun          =>
          FunctionCall(uf.name, args)
        case vf: VectorizeUserFun =>
          FunctionCall(vf.vectorizedFunction.name, args)
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
                                value: AstNode): ExpressionT = {
    val originalType = getOriginalType(mem)

    if (Type.haveSameValueTypes(originalType, currentType)) {
      AssignmentExpression(
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
            case VarIdxRef(_, _, idx) => ArithExpression(idx.get.asInstanceOf[ArithExpression].content / vt.len)
            case x                 => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
          }

          OclStore(VarIdxRef(mem.variable), vt, value, offset, mem.addressSpace)
      }
    }
  }

  private def generateLoadNodes(args: Expr*): List[AstNode] = {
    args.map(arg => {
      val mem = OpenCLMemory.asOpenCLMemory(arg.mem)
      generateLoadNode(mem, arg.t, arg.view)
    }).toList
  }

  private def generateLoadNode(mem: OpenCLMemory, t: Type, view: View): ExpressionT = {
    mem match {
      // we want to generate a load for a tuple constructed by a corresponding view (i.e. zip)
      case coll: OpenCLMemoryCollection =>
        if (!t.isInstanceOf[TupleType])
          throw new OpenCLGeneratorException(s"Found a OpenCLMemoryCollection for var: " +
            s"${mem.variable}, but corresponding type: $t is " +
            s"not a tuple.")
        val tt = t.asInstanceOf[TupleType]

        var args: Vector[AstNode] = Vector()
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
              Cast(VarIdxRef(mem.variable), st)

            // originally an array of scalar values in global memory,
            // but now a vector type
            //  => emit vload
            case (at: ArrayType, vt: VectorType)
              if Type.getValueType(at) == vt.scalarT
                && (mem.addressSpace == GlobalMemory || mem.addressSpace == LocalMemory) =>

              val (offset, shift) = ViewPrinter.emit(view, replacementsWithFuns, mem.addressSpace) match {
                case VarIdxRef(_, _, idx) =>
                  (ArithExpression(idx.get.asInstanceOf[ArithExpression].content / vt.len),
                    ArithExpression(idx.get.asInstanceOf[ArithExpression].content % vt.len))
                case x                 => throw new MatchError(s"Expected a VarRef but got $x.")
              }

              OclLoad(VarIdxRef(mem.variable), vt, offset, shift, mem.addressSpace)

            case (at: ArrayType, vt: VectorType)
              if Type.getValueType(at) == vt.scalarT && (mem.addressSpace == PrivateMemory) =>

              assert(privateMems.exists(m => m.mem == mem))

              // TODO: this seems like a very specific local solution ... find a more generic proper one

              // iterate over the range, assuming that it is contiguous
              val arraySuffixStartIndex = arrayAccessPrivateMemIndex(mem.variable, view)
              val arraySuffixStopIndex = arraySuffixStartIndex + vt.len.eval

              val seq = (arraySuffixStartIndex until arraySuffixStopIndex).map(i => {
                VarIdxRef(mem.variable, suffix = Some("_" + i))
              })

              VectorLiteral(vt, seq: _*)

            // originally a vector value in private memory,
            // but now a scalar type
            //  => emit load from components
            case (vt: VectorType, st: ScalarType)
              if st == vt.scalarT && (mem.addressSpace == PrivateMemory) =>

              val componentSuffix = componentAccessVectorVar(mem.variable, view, mem.addressSpace)
              VarIdxRef(mem.variable, suffix = Some(componentSuffix))

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
              VarIdxRef(mem.variable, suffix = Some(arraySuffix + componentSuffix))

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

              VarIdxRef(mem.variable, suffix = Some(arraySuffix + componentSuffix))

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
                    case VarIdxRef(v, _, index) => VarIdxRef(v, Some(suffix), index)
                    case x                   => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
                  }

                case PrivateMemory                                 =>

                  val declaration = privateDecls.find(m => m._1 == mem.variable)
                  val arraySuffix =
                    if (declaration.isDefined && declaration.get._2.t.isInstanceOf[ArrayType]) // check if this is actually an array
                      arrayAccessPrivateMem(mem.variable, innerView)
                    else // Workaround for values
                      ""
                  VarIdxRef(mem.variable, suffix = Some(arraySuffix + suffix))
                case UndefAddressSpace | AddressSpaceCollection(_) =>
                  throw new IllegalArgumentException(s"Cannot load data from ${mem.addressSpace}")
              }
          }
        }
    }
  }

  /**
    * Create an access node (i.e. of type VarRef) for variable v based on the
    * given address space and view
    *
    * @param v            The variable to access
    * @param addressSpace The address space, i.e. global, local, private
    * @param view         The view to access var `v`
    * @return An VarRef node accessing `v` as described in `view`.
    */
  private def accessNode(v: Var,
                         addressSpace: OpenCLAddressSpace,
                         view: View): ExpressionT = {
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
    * @param v            The variable to access
    * @param addressSpace The address space `v` lives in
    * @param view         The view describing the access
    * @return An VarRef node accessing `v` as described in `view`.
    */
  private def arrayAccessNode(v: Var,
                              addressSpace: OpenCLAddressSpace,
                              view: View): ExpressionT = {
    addressSpace match {
      case LocalMemory | GlobalMemory =>
        ViewPrinter.emit(view, replacementsWithFuns, addressSpace)

      case PrivateMemory =>
        ViewPrinter.emit(view, replacementsWithFuns, addressSpace) match {
          case VarIdxRef(_, _, _) =>
            VarIdxRef(v, suffix = Some(arrayAccessPrivateMem(v, view)))
          case e: ExpressionT  => e
        }

      case UndefAddressSpace | AddressSpaceCollection(_) =>
        throw new IllegalArgumentException(s"Cannot load data from $addressSpace")
    }
  }

  /**
    * Generating the suffix appended to emulate an array access in private memory
    *
    * @param v    The variable to access
    * @param view The view describing the access
    * @return A string of the form '_index' where index is the computed
    *         array index. The index must be computable at compile time.
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
        case VarIdxRef(_, _, idx) => idx.get.content
        case x                 => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
      }
      // if the original value type is a vector:
      //   divide index by vector length
      case _: VectorType                     =>
        val length = Type.getLength(Type.getValueType(originalType))
        val index = ViewPrinter.emit(view, replacements, PrivateMemory) match {
          case VarIdxRef(_, _, idx) => idx.get.content
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
    * @param v    The variable to access. Must have been declared with a vector type
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
          case VarIdxRef(_, _, idx) => idx.get.asInstanceOf[ArithExpression].content
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
  private def valueAccessNode(v: Var): VarIdxRef = {
    VarIdxRef(v)
  }

  /**
    * Generate code for *sequentially* copying data of type `ty` from a Memory
    * to an other using the provided views. We can only write in one single
    * location in memory: no memory collection and no tuple of arrays. Although,
    * reading from a memory collection is possible, for instance: copying after
    * a Zip.
    *
    * @param inMem        memory location of the data to be copied
    * @param inView       view explaining how to access the data to be copied
    * @param outMem       memory location where to copy the data
    * @param outView      view explaining how to access the destination memory
    * @param ty           the type of the data to be copied.
    * @param shouldUnroll whether the generated copy loop should be unrolled          *
    * @return a piece of OpenCL code that performs the copy *sequentially*
    */
  private def generateSeqCopy(inMem: Memory, inView: View, outMem: Memory, outView: View,
                              ty: Type, shouldUnroll: Boolean = false): AstNode with BlockMember = {
    assert(!outMem.isInstanceOf[OpenCLMemoryCollection]) // cannot handle that: see comment above
    ty match {
      case ScalarType(_, _) | _: TupleType | _: VectorType =>
        val load = generateLoadNode(OpenCLMemory.asOpenCLMemory(inMem), ty, inView)
        generateStoreNode(OpenCLMemory.asOpenCLMemory(outMem), ty, outView, load)
      case at: ArrayType                                   =>
        val innerBlock = MutableBlock(Vector.empty)
        val length = at match {
          case s: Size => s.size
          case _       => throw new NotImplementedError()
        }
        val loopVar = Var("cp", RangeAdd(0, length, 1))

        if (shouldUnroll) {
          generateForLoopUnrolled(innerBlock, loopVar, (block) => {
            (block: MutableBlock) += generateSeqCopy(
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
            OclVarDecl(loopVar, Int, Some(ArithExpression(0))),
            BinaryExpression(ArithExpression(loopVar),
              BinaryExpressionT.Operator.<, ArithExpression(length)),
            AssignmentExpression(VarIdxRef(CVar(loopVar)), ArithExpression
            (loopVar + 1)),
            innerBlock
          )
        }
      case _                                               => throw new NotImplementedError(s"generateSeqCopy: $ty")
    }
  }
}
