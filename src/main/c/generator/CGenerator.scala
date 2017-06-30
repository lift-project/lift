package c.generator

import lift.arithmetic._
import arithmetic.TypeVar
import generator.Generator
import ir._
import ir.ast._
import ir.view._
import c.generator.CAst._
import opencl.ir._
import opencl.ir.pattern._
import opencl.generator._

import scala.collection.immutable

object CGenerator extends Generator {

  def generate(f: Lambda): String = {
    generate(f, NDRange(?, ?, ?))
  }

  def generate(f: Lambda, localSizes: NDRange): String = {
    generate(f, localSizes, NDRange(?, ?, ?), immutable.Map())
  }

  // Compile a type-checked function into an OpenCL kernel
  def generate(f: Lambda, localSize: NDRange, globalSize: NDRange,
               valueMap: immutable.Map[ArithExpr, ArithExpr]): String = {
    (new CGenerator).generate(f, localSize, globalSize, valueMap)
  }

  def printTypes(expr: Expr): Unit = {
    Expr.visit(expr, {
      case e@(call: FunCall) => println(e + "\n    " +
        e.t + " <- " + call.argsType + "\n")
      case e => println(e + "\n    " + e.t + "\n")
    }, (e: Expr) => {})
  }

  def printTypes(lambda: Lambda): Unit = printTypes(lambda.body)

  def getMemories(f: Lambda): (Array[TypedOpenCLMemory], Array[TypedOpenCLMemory]) = {
    val memories = TypedOpenCLMemory.get(f.body, f.params).toArray

    val numInputs = f.params.length

    val outputMem = memories.last

    if (memories.length > numInputs) {
      val temp = memories(numInputs)
      memories(numInputs) = outputMem
      memories(memories.length - 1) = temp
    }

    val (locals, globals) = memories.partition(_.mem.addressSpace == LocalMemory)
    val globalsFirst = globals ++ locals

    if (AllocateLocalMemoryStatically())
      globalsFirst.partition(isFixedSizeLocalMemory)
    else
      (Array.empty[TypedOpenCLMemory], globalsFirst)
  }

  def getDifferentMemories(lambda: Lambda): (Array[TypedOpenCLMemory], Array[TypedOpenCLMemory], Predef.Map[Var, Type]) = {

    val valMems = Expr.visitWithState(Set[Memory]())(lambda.body, (expr, set) =>
      expr match {
        case value: Value => set + value.mem
        case _ => set
      })

    val typedMems =
      TypedOpenCLMemory.get(lambda.body, lambda.params, includePrivate = true).toArray

    val memory = TypedOpenCLMemory.get(lambda.body, lambda.params)

    val (typedValueMems, privateMems) =
      typedMems.diff(memory).partition(m => valMems.contains(m.mem))

    // the base type is used for allocation of all variables ...
    var varDecls =
      typedMems.map(tm => {
        if (tm.mem.addressSpace == PrivateMemory) {
          // do not de-vectorise for private memory
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

  private[generator] def isFixedSizeLocalMemory: (TypedOpenCLMemory) => Boolean = {
    mem => try {
      mem.mem.size.eval
      mem.mem.addressSpace == LocalMemory
    } catch {
      case NotEvaluableException() => false
    }
  }
}

class CGenerator extends Generator {

  type ValueTable = immutable.Map[ArithExpr, ArithExpr]
  type SymbolTable = immutable.Map[Var, Type]

  protected val openCLCodeGen = new CPrinter

  protected var replacements: ValueTable = immutable.Map.empty
  protected var replacementsWithFuns: ValueTable = immutable.Map.empty
  protected var privateMems: Array[TypedOpenCLMemory] = Array[TypedOpenCLMemory]()
  protected var privateDecls: Predef.Map[Var, VarDecl] = immutable.Map[Var, CAst.VarDecl]()

  protected var varDecls: SymbolTable = immutable.Map.empty

  private def printMemories(expr: Expr): Unit = {
    Expr.visit(expr, {
      case e@(call: FunCall) =>
        println(e + "\n    " +
          e.mem.toString + " <- " +
          call.argsMemory.toString + "\n")
      case e => println(e + "\n    " + e.mem.toString + "\n")
    }, (f: Expr) => {})
  }

  def generate(f: Lambda): String  = {
    generate(f, NDRange(?, ?, ?))
  }

  def generate(f: Lambda, localSizes: NDRange): String = {
    generate(f, localSizes, NDRange(?, ?, ?), immutable.Map())
  }

  def generate(f: Lambda, localSize: NDRange, globalSize: NDRange,
                valueMap: collection.Map[ArithExpr, ArithExpr]): String = {

    if (f.body.t == UndefType)
      throw new OpenCLGeneratorException("Lambda has to be type-checked to generate code")

    InferOpenCLAddressSpace(f)
    // Allocate the params and set the corresponding type
    f.params.foreach((p) => {
      p.mem = OpenCLMemory.allocMemory(Type.getAllocatedSize(p.t), p.addressSpace)
    })

    RangesAndCounts(f, localSize, globalSize, valueMap)
    allocateMemory(f)

    ShouldUnroll(f)

    if (PerformBarrierElimination())
      BarrierElimination(f)

    checkLambdaIsLegal(f)

    if (Verbose()) {

      println("Types:")
      CGenerator.printTypes(f.body)

      println("Memory:")
      printMemories(f.body)

      println("Allocated Memory:")
      TypedOpenCLMemory.get(f.body, f.params, includePrivate = true).foreach(println(_))
      println()
    }

    View(f)

    val globalBlock = CAst.Block(Vector.empty, global = true)

    val containsDouble = Expr.visitWithState(false)(f.body, {
      case (expr, _) if expr.t == Double => true
      case (_, state) => state
    })

    if (containsDouble) {
      globalBlock += OpenCLExtension("cl_khr_fp64")
    }

    val tupleTypes = Expr.visitWithState(Set[TupleType]())(f.body, (expr, typeList) => {
      expr match {
        case FunCall(uf: UserFun, _*) => typeList ++ uf.tupleTypes
        case FunCall(vec: VectorizeUserFun, _*) => typeList ++ vec.vectorizedFunction.tupleTypes
        case _ =>
          expr.t match {
            case t: TupleType if t.elemsT.forall(!_.isInstanceOf[ArrayType]) => typeList + t
            case _ => typeList
          }
      }
    })

    tupleTypes.foreach(globalBlock += CAst.TypeDef(_))

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

    Context.updateContext(lambda.body)

    Expr.visit(lambda.body, _ => Unit, {
      case call@FunCall(MapGlb(dim, _), _*) if call.context.inMapGlb(dim) =>
        throw new IllegalKernel(s"Illegal nesting of $call inside MapGlb($dim)")
      case call@FunCall(MapWrg(dim, _), _*) if call.context.inMapWrg(dim) =>
        throw new IllegalKernel(s"Illegal nesting of $call inside MapWrg($dim)")
      case call@FunCall(MapLcl(dim, _), _*) if call.context.inMapLcl(dim) =>
        throw new IllegalKernel(s"Illegal nesting of $call inside MapLcl($dim)")
      case call@FunCall(MapLcl(dim, _), _*) if !call.context.inMapWrg(dim) =>
        throw new IllegalKernel(s"Illegal use of $call without MapWrg($dim)")
      case call@FunCall(toLocal(_), _) if !call.context.inMapWrg.reduce(_ || _) =>
        throw new IllegalKernel(s"Illegal use of local memory, without using MapWrg $call")
      case call@FunCall(Map(nestedLambda), _*) if nestedLambda.body.isConcrete =>
        throw new IllegalKernel(s"Illegal use of UserFun where it won't generate code in $call")
      case _ =>
    })

    lambda.body.mem match {
      case m: OpenCLMemory if m.addressSpace != GlobalMemory =>
        throw new IllegalKernel("Final result must be stored in global memory")
      case _ =>
    }
  }

  /** Traversals f and print all user functions using oclPrinter */
  private def generateUserFunctions(expr: Expr): Seq[Declaration] = {
    var fs = Seq[Declaration]()

    val userFuns = Expr.visitWithState(Set[UserFun]())(expr, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case uf: UserFun => set + uf
          case _ => set
        }
        case _ => set
      })

    userFuns.foreach(uf => {

      val block = CAst.Block()
      if (uf.tupleTypes.length == 1)
        block += CAst.TupleAlias(uf.tupleTypes.head, "Tuple")
      else uf.tupleTypes.zipWithIndex.foreach({ case (x, i) =>
        // TODO: think about this one ...
        block += CAst.TupleAlias(x, s"Tuple$i")
      })
      block += CAst.OpenCLCode(uf.body)

      fs = fs :+ CAst.Function(
        name = uf.name,
        ret = uf.outT,
        params = (uf.inTs, uf.paramNames).
          zipped.map((t, n) => CAst.ParamDecl(n, t)).toList,
        body = block)
    })

    fs
  }

  def allocateMemory(f: Lambda): Unit = {
    OpenCLMemoryAllocator(f)
    Kernel.memory = TypedOpenCLMemory.get(f.body, f.params).toArray
  }

  private object Kernel {
    var memory = Array.empty[TypedOpenCLMemory]
    var staticLocalMemory = Array.empty[TypedOpenCLMemory]
  }


  def generateKernel(f: Lambda): Declaration = {


    val someMemories = CGenerator.getDifferentMemories(f)

    val typedValueMems = someMemories._1
    this.privateMems = someMemories._2
    varDecls = someMemories._3
    /*
    val memories = CGenerator.getMemories(f)

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
          // size parameters
          vars.sortBy(_.name).map(x => OpenCLAST.ParamDecl(x.toString, Int)),
      body = OpenCLAST.Block(Vector.empty),
      kernel = true) */
    /*
    // print out allocated memory sizes
    val varMap = iterateVars.map(v => (v, ArithExpr.asCst(v.range.max))).toMap
    Kernel.memory.foreach(mem => {
      val m = mem.mem
      if (Verbose()) {
        println("Allocated " + ArithExpr.substitute(m.size, varMap.toMap) +
          " bytes for variable " + openCLCodeGen.toString(m.variable) +
          " in " + m.addressSpace + " memory")
      }
    })

    kernel.body += OpenCLAST.Comment("Static local memory")
    Kernel.staticLocalMemory.foreach(x =>
      kernel.body +=
        OpenCLAST.VarDecl(x.mem.variable, x.t,
          addressSpace = x.mem.addressSpace,
          length = (x.mem.size /^ Type.getMaxSize(Type.getBaseType(x.t))).eval))

    kernel.body += OpenCLAST.Comment("Typed Value memory")
    typedValueMems.foreach(x =>
      kernel.body +=
        OpenCLAST.VarDecl(x.mem.variable,
          Type.getValueType(x.t),
          addressSpace = x.mem.addressSpace))

    kernel.body += OpenCLAST.Comment("Private Memory")
    privateMems.foreach(x => {
      val length = x.mem.size /^ Type.getMaxSize(Type.getValueType(x.t))

      if (!length.isEvaluable)
        throw new IllegalKernel("Private memory length has to be" +
          s"evaluable, but found $length")

      val decl = OpenCLAST.VarDecl(x.mem.variable, x.t,
        addressSpace = x.mem.addressSpace,
        length = length.eval)

      privateDecls += x.mem.variable -> decl

      kernel.body += decl
    }) */

    //val temp = Kernel.memory.map((x => CAst.ParamDecl(x.mem.variable.toString,x.t))).toList

    val liftKernel = CAst.Function(
      name = "liftKernel",
      ret = UndefType,
      params = Kernel.memory.map(x => CAst.ParamDecl(x.mem.variable.toString,x.t)).toList, //f.params.map(x => CAst.ParamDecl(x.toString,x.t)).toList,
      body = Block(),
      kernel = true
    )

    //Declaration of various other variables
    liftKernel.body += CAst.Comment("Static local memory")
    Kernel.staticLocalMemory.foreach(x =>
      liftKernel.body +=
        CAst.VarDecl(x.mem.variable, x.t,
          addressSpace = x.mem.addressSpace,
          length = (x.mem.size /^ Type.getMaxAllocatedSize(Type.getBaseType(x.t))).eval))

    liftKernel.body += CAst.Comment("Typed Value memory")
    typedValueMems.foreach(x =>
      liftKernel.body +=
        CAst.VarDecl(x.mem.variable,
          Type.getValueType(x.t),
          addressSpace = x.mem.addressSpace))

    liftKernel.body += CAst.Comment("Private Memory")
    privateMems.foreach(x => {
      val length = x.mem.size /^ Type.getMaxAllocatedSize(Type.getValueType(x.t))

      if (!length.isEvaluable)
        throw new IllegalKernel("Private memory length has to be" +
          s"evaluable, but found $length")

      val decl = CAst.VarDecl(x.mem.variable, x.t,
        addressSpace = x.mem.addressSpace,
        length = length.eval)

      privateDecls += x.mem.variable -> decl

      liftKernel.body += decl
    })



    generateExpr(f.body, liftKernel.body)

    if (CSE())
      CommonSubexpressionElimination(liftKernel.body)

    liftKernel
  }


  protected def generateExpr(expr: Expr, block: Block): Unit = {
    assert(expr.t != UndefType)

    expr match {
      case f: FunCall => f.args.foreach(generateExpr(_, block))
      case _ =>
    }

    generateExprPostArguments(expr,block)
  }

  protected def generateExprPostArguments(expr:Expr, block:Block):Unit = {
    expr match {
      case call: FunCall => call.f match {
        case m: MapSeq => generateMapSeqCall(m, call, block)
        case _: Map =>

        case r: ReduceSeq => generateReduceSeqCall(r, call, block)
        case _: Search =>

        case i: Iterate => generateIterateCall(i, call, block)

        case vec: VectorizeUserFun => generateUserFunCall(vec.vectorizedFunction, call, block)
        case u: UserFun => generateUserFunCall(u, call, block)

        case fp: FPattern => generateExpr(fp.f.body, block)
        case l: Lambda => generateExpr(l.body, block)
        case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
             Split(_) | Join() | Slide(_, _) | Zip(_) | Tuple(_) | Filter() |
             Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Pad(_, _, _) =>

      }
      case v: Value => generateValue(v, block)
      case p: Param =>
    }
  }

  // === Maps ===

  // MapSeq
  private def generateMapSeqCall(m: MapSeq,
                                 call: FunCall,
                                 block: Block): Unit = {
    (block: Block) += CAst.Comment("map_seq")
    generateForLoop(block, m.loopVar, generateExpr(m.f.body, _), m.shouldUnroll)
    (block: Block) += CAst.Comment("end map_seq")
  }

  // Expr should be an array
  protected def generateLength(e: Expr): Either[Expression, ArithExpr] = {
    e match {
      case e: Expr =>
        e.t match {
          case a: RuntimeSizedArrayType =>
            // TODO: Emitting a view of type ArrayType is illegal!
            Left(ViewPrinter.emit(e.mem.variable, e.view) match {
              case OpenCLAST.VarRef(v, s, i) => VarRef(v, s, ArithExpression(i.content))
              case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
            })
          case ArrayTypeWS(_,s) => Right(s)
          case ArrayType(_) =>
            // layout in memory: | capacity | size | ... |
            Left(ViewPrinter.emit(e.mem.variable, e.view) match {
              case OpenCLAST.VarRef(v, s, i) => VarRef(v, s, ArithExpression(i.content+1))
              case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
            })
          case NoType | ScalarType(_, _) | TupleType(_) | UndefType | VectorType(_, _) =>
            throw new TypeException(e.t, "Array", e)
        }
    }
  }

  // === Reduce ===
  private def generateReduceSeqCall(r: ReduceSeq,
                                    call: FunCall,
                                    block: Block): Unit = {

    val innerBlock = CAst.Block(Vector.empty)
    (block: Block) += CAst.Comment("reduce_seq")

    val inputLen = generateLength(call.args(1))
    inputLen match {

      case Left(len: Expression) =>
        val indexVar = r.loopVar
        val range = indexVar.range.asInstanceOf[RangeAdd]

        val init = ArithExpression(range.start)
        val cond = CondExpression(ArithExpression(r.loopVar), len, CondExpression.Operator.<)
        val increment = AssignmentExpression(ArithExpression(r.loopVar), ArithExpression(r.loopVar + range.step))

        (block: Block) += CAst.ForLoop(VarDecl(r.loopVar, opencl.ir.Int, init, PrivateMemory), ExpressionStatement(cond), increment, innerBlock)

        generateExpr(r.f.body, innerBlock)

      case Right(len: ArithExpr) =>
        generateForLoop(block, r.loopVar, generateExpr(r.f.body, _), r.shouldUnroll)
    }

    (block: Block) += CAst.Comment("end reduce_seq")
  }

  private def generateValue(v: Value, block: Block): Unit = {
    val temp = Var("tmp")

    (block: Block) += CAst.VarDecl(temp, Type.getValueType(v.t),
      init = CAst.OpenCLCode(v.value))
    (block: Block) += CAst.AssignmentExpression(
      CAst.VarRef(v.mem.variable),
      CAst.VarRef(temp))
  }

  // === Iterate ===
  private def generateIterateCall(i: Iterate,
                                  call: FunCall,
                                  block: Block): Unit = {

    val inputMem = OpenCLMemory.asOpenCLMemory(call.args.head.mem)
    val outputMem = OpenCLMemory.asOpenCLMemory(call.mem)
    val swapMem = OpenCLMemory.asOpenCLMemory(i.swapBuffer)

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
      CAst.VarDecl(curOutLen, Int,
        CAst.ArithExpression(Type.getLength(call.argsType)))

    // create new temporary input and output pointers
    val tin = Var("tin")
    val tout = Var("tout")
    varDecls = varDecls.updated(tin, Type.devectorize(call.t))
    varDecls = varDecls.updated(tout, Type.devectorize(call.t))

    // ADDRSPC TYPE tin = in;
    (block: Block) += CAst.VarDecl(tin, Type.devectorize(call.t),
      CAst.VarRef(inputMem.variable),
      outputMem.addressSpace)

    val range = i.indexVar.range.asInstanceOf[RangeAdd]

    // ADDRSPC TYPE tin = (odd ? out : swap);
    (block: Block) += CAst.VarDecl(tout, Type.devectorize(call.t),
      init = CAst.ArithExpression(
        ((range.stop % 2) ne Cst(0)) ?? outputMem.variable !! swapMem.variable),
      addressSpace = outputMem.addressSpace)

    generateForLoop(block, i.indexVar, (b) => {

      // modify the pointers to the memory before generating the body
      val oldInV = inputMem.variable
      val oldOutV = outputMem.variable
      inputMem.variable = tin
      outputMem.variable = tout

      // generate the function call in the body
      generateExpr(funCall, b)

      // restore the pointers to memory
      inputMem.variable = oldInV
      outputMem.variable = oldOutV

      val curOutLenRef = CAst.VarRef(curOutLen)

      val innerOutputLength = Type.getLength(funCall.t)

      // tmp = tmp * outputLen / inputLen
      (b: Block) += CAst.AssignmentExpression(curOutLenRef,
        CAst.ArithExpression(innerOutputLength))


      val tinVStrRef = CAst.VarRef(tin)

      // tin = (tout == swap) ? swap : out
      (b: Block) += CAst.AssignmentExpression(tinVStrRef,
        CAst.ArithExpression((tout eq swapMem.variable) ??
          swapMem.variable !! outputMem.variable))


      val toutVStrRef = CAst.VarRef(tout)

      // tout = (tout == swap) ? out : swap
      (b: Block) += CAst.AssignmentExpression(toutVStrRef,
        CAst.ArithExpression((tout eq swapMem.variable) ??
          outputMem.variable !! swapMem.variable))

      if (outputMem.addressSpace != PrivateMemory)
        (b: Block) += CAst.Barrier(outputMem)

    } /*, i.iterationCount*/)
  }

  protected def generateForLoop(block: Block,
                              indexVar: Var,
                              generateBody: (Block) => Unit,
                              needUnroll: Boolean = false): Unit = {

    val range = indexVar.range.asInstanceOf[RangeAdd]
    val step = range.step
    val init = ArithExpression(range.start)
    val stop = range match {
      case ra: RangeAdd => ra.stop
      case _ => throw new OpenCLGeneratorException("Cannot handle range for ForLoop: " + range)
    }
    val cond = CondExpression(ArithExpression(indexVar), ArithExpression(stop), CondExpression.Operator.<)

    // if we need to unroll (e.g. because of access to private memory)
    if (needUnroll) {
      val iterationCount = try {
        indexVar.range.numVals.eval
      } catch {
        case NotEvaluableException() =>
          throw new OpenCLGeneratorException("Trying to unroll loop, but iteration count " +
            "could not be determined statically.")
        case NotEvaluableToIntException() =>
          throw new OpenCLGeneratorException("Trying to unroll loop, but iteration count " +
            "is larger than scala.Int.MaxValue.")
      }

      if (iterationCount > 0) {
        (block: Block) += CAst.Comment("unroll")

        for (i <- 0 until iterationCount) {
          replacements = replacements.updated(indexVar, i)
          val j: ArithExpr =
            if (range.min.isInstanceOf[OclFunction]) {
              range.min + step * i
            } else {
              i
            }
          replacementsWithFuns = replacementsWithFuns.updated(indexVar, j)

          generateBody(block)
        }
        // cleanup
        replacements = replacements - indexVar
        replacementsWithFuns = replacementsWithFuns - indexVar

        (block: Block) += CAst.Comment("end unroll")
        return
      } else {
        throw new OpenCLGeneratorException(s"Trying to unroll loop, but iteration count is $iterationCount.")
      }

    }

    // TODO: Information needed elsewhere. See analysis.ControlFlow
    // try to see if we really need a loop
    //Optimized introduced in order to stop messing with OpenMP loops!!
      indexVar.range.numVals match {
        case Cst(0) =>
          // zero iterations
          (block: Block) += CAst.Comment("iteration count is 0, no loop emitted")
          return

        case Cst(1) =>
          generateStatement(block, indexVar, generateBody, init)
          return

        // TODO: See TestInject.injectExactlyOneIterationVariable
        // TODO: M / 128 is not equal to M /^ 128 even though they print to the same C code
        case _ if range.start.min.min == Cst(0) &&
          ArithExpr.substituteDiv(range.stop) == ArithExpr.substituteDiv(range.step) =>

          generateStatement(block, indexVar, generateBody, init)
          return

        // TODO: See TestOclFunction.numValues and issue #62
        case _ if range.start.min.min == Cst(0) && range.stop == Cst(1) =>
          generateIfStatement(block, indexVar, generateBody, init, stop)
          return
        case _ =>
          (indexVar.range.numVals.min, indexVar.range.numVals.max) match {
            case (Cst(0), Cst(1)) =>
              // one or less iteration
              generateIfStatement(block, indexVar, generateBody, init, stop)
              return

            case _ =>
          }
    }

    val increment = AssignmentExpression(ArithExpression(indexVar), ArithExpression(indexVar + range.step))
    val innerBlock = CAst.Block(Vector.empty)
    (block: Block) += CAst.ForLoop(VarDecl(indexVar, opencl.ir.Int, init, PrivateMemory), ExpressionStatement(cond), increment, innerBlock)
    generateBody(innerBlock)
  }

  protected def generateStatement(block: Block, indexVar: Var, generateBody: (Block) => Unit, init: ArithExpression): Unit = {
    // one iteration
    (block: Block) += CAst.Comment("iteration count is exactly 1, no loop emitted")
    val innerBlock = CAst.Block(Vector.empty)
    innerBlock += CAst.VarDecl(indexVar, opencl.ir.Int, init, PrivateMemory)
    generateBody(innerBlock)
    (block: Block) += innerBlock
  }

  protected def generateIfStatement(block: Block, indexVar: Var, generateBody: (Block) => Unit, init: ArithExpression, stop: ArithExpr): Unit = {
    (block: Block) += CAst.Comment("iteration count is exactly 1 or less, no loop emitted")
    val innerBlock = CAst.Block(Vector.empty)
    innerBlock += CAst.VarDecl(indexVar, opencl.ir.Int, init, PrivateMemory)
    (block: Block) += CAst.IfThenElse(CondExpression(init, ArithExpression(stop), CondExpression.Operator.<), innerBlock)
    generateBody(innerBlock)
  }
  /*
  private def generateWhileLoop(block: Block,
                                loopPredicate: Predicate,
                                generateBody: (Block) => Unit): Unit = {
    val innerBlock = OpenCLAST.Block(Vector.empty)
    (block: Block) += OpenCLAST.WhileLoop(loopPredicate, body = innerBlock)
    generateBody(innerBlock)
  }

  private def generateConditional(block: Block,
                                  switchPredicate: Predicate,
                                  genTrueBranch: (Block) => Unit,
                                  genFalseBranch: (Block) => Unit): Unit = {
    val trueBlock = OpenCLAST.Block(Vector.empty)
    val falseBlock = OpenCLAST.Block(Vector.empty)
    (block: Block) += OpenCLAST.IfThenElse(switchPredicate, trueBody = trueBlock, falseBody = falseBlock)
    genTrueBranch(trueBlock)
    genFalseBranch(falseBlock)
  }*/

  private def generateUserFunCall(u: UserFun,
                                  call: FunCall,
                                  block: Block): Block = {
    // Handle vector assignments for vector types
    val mem = OpenCLMemory.asOpenCLMemory(call.mem)
    (block: Block) += generateStoreNode(mem, call.t, call.outputView,
      generateFunCall(call, generateLoadNodes(call.args: _*)))

    block
  }

  @scala.annotation.tailrec
  private def generateFunCall(expr: Expr,
                              args: List[OclAstNode]): OclAstNode = {
    expr match {
      case call: FunCall => call.f match {
        case uf: UserFun =>
          CAst.FunctionCall(uf.name, args)
        case vf: VectorizeUserFun =>
          CAst.FunctionCall(vf.vectorizedFunction.name, args)
        case l: Lambda => generateFunCall(l.body, args)

        case _ => throw new NotImplementedError()
      }
      case _ => throw new NotImplementedError()
    }
  }

  private def getOriginalType(mem: OpenCLMemory) =
    CGenerator.getOriginalType(mem, varDecls)

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
      CAst.AssignmentExpression(
        to = accessNode(mem.variable, mem.addressSpace, view),
        value = value
      )
    } else {
      (originalType, currentType) match {
        // originally a scalar type in global memory, but now a vector type
        //  => emit vstore
        case (at: ArrayType, vt: VectorType)
          if Type.isEqual(Type.getValueType(at), vt.scalarT)
            && (mem.addressSpace == GlobalMemory
            || mem.addressSpace == LocalMemory) =>

          val offset = ViewPrinter.emit(mem.variable, view, replacementsWithFuns) match {
            case OpenCLAST.VarRef(_, _, i) => CAst.ArithExpression(i.content / vt.len)
            case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
          }
          CAst.Store(CAst.VarRef(mem.variable), vt, value, offset, mem.addressSpace)
      }
    }
  }

  private def generateLoadNodes(args: Expr*): List[OclAstNode] = {
    args.map(arg => {
      val mem = OpenCLMemory.asOpenCLMemory(arg.mem)
      generateLoadNode(mem, arg.t, arg.view)
    }).toList
  }

  private def generateLoadNode(mem: OpenCLMemory, t: Type, view: View): OclAstNode = {
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
            case (st: ScalarType, vt: VectorType) if Type.isEqual(st, vt.scalarT) =>
              CAst.Cast(CAst.VarRef(mem.variable), st)

            // originally an array of scalar values in global memory,
            // but now a vector type
            //  => emit vload
            case (at: ArrayType, vt: VectorType)
              if Type.isEqual(Type.getValueType(at), vt.scalarT)
                && (mem.addressSpace == GlobalMemory || mem.addressSpace == LocalMemory) =>

              val offset = ViewPrinter.emit(mem.variable, view, replacementsWithFuns) match {
                case OpenCLAST.VarRef(_, _, idx) => CAst.ArithExpression(idx.content / vt.len)
                case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
              }

              CAst.Load(CAst.VarRef(mem.variable), vt, offset, mem.addressSpace)

            // originally an array of scalar values in private memory,
            // but now a vector type
            //  => emit (float2)(f1, f2) primitive
            case (at: ArrayType, vt: VectorType)
              if Type.isEqual(Type.getValueType(at), vt.scalarT)
                && (mem.addressSpace == PrivateMemory) =>

              assert(privateMems.exists(m => m.mem == mem))

              // TODO: this seems like a very specific local solution ... find a more generic proper one

              // iterate over the range, assuming that it is contiguous
              val arraySuffixStartIndex = arrayAccessPrivateMemIndex(mem.variable, view)
              val arraySuffixStopIndex = arraySuffixStartIndex + vt.len.eval

              val seq = (arraySuffixStartIndex until arraySuffixStopIndex).map(i => {
                CAst.VarRef(mem.variable, suffix = "_" + i)
              })

              CAst.VectorLiteral(vt, seq: _*)

            // originally a vector value in private memory,
            // but now a scalar type
            //  => emit load from components
            case (vt: VectorType, st: ScalarType)
              if Type.isEqual(st, vt.scalarT)
                && (mem.addressSpace == PrivateMemory) =>

              val componentSuffix = componentAccessVectorVar(mem.variable, view)
              CAst.VarRef(mem.variable, suffix = componentSuffix)

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

              val componentSuffix = componentAccessVectorVar(mem.variable, view)
              CAst.VarRef(mem.variable, suffix = arraySuffix + componentSuffix)

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

              val componentSuffixStartIndex = componentAccessvectorVarIndex(mem.variable, view)
              val componentSuffixStopIndex = componentSuffixStartIndex + vt.len.eval

              // iterate over the range, assuming that it is contiguous
              val componentSuffix = (componentSuffixStartIndex until componentSuffixStopIndex).foldLeft(".s")(_ + _)

              CAst.VarRef(mem.variable, suffix = arraySuffix + componentSuffix)

            // originally a tuple, now a value. => generate stuff like var[i]._j
            case (at: ArrayType, st: ScalarType)
              if Type.getValueType(at).isInstanceOf[TupleType] =>
              // get tuple component and generate suffix (._j)
              val vtc = view.asInstanceOf[ViewTupleComponent]
              val suffix = s"._${vtc.i}"

              // throw away the tuple component view for generating array index [i]
              val innerView = vtc.iv

              mem.addressSpace match {
                case LocalMemory | GlobalMemory =>
                  ViewPrinter.emit(mem.variable, innerView, replacementsWithFuns) match {
                    case OpenCLAST.VarRef(v, _, i) =>
                      CAst.VarRef(v, suffix, CAst.ArithExpression(i.content))
                    case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
                  }

                case PrivateMemory =>

                  val arraySuffix =
                    if (privateMems.exists(m => m.mem == mem)) // check if this is actually an array
                      arrayAccessPrivateMem(mem.variable, innerView)
                    else // Workaround for values
                      ""
                  CAst.VarRef(mem.variable, suffix = arraySuffix + suffix)
                  
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
    * @param v            The variable to access
    * @param addressSpace The address space, i.e. global, local, private
    * @param view         The view to access var `v`
    * @return An VarRef node accessing `v` as described in `view`.
    */
  private def accessNode(v: Var,
                         addressSpace: OpenCLAddressSpace,
                         view: View): CAst.VarRef = {
    addressSpace match {
      case LocalMemory | GlobalMemory =>
        val originalType = varDecls(v)
        originalType match {
          case _: ArrayType => arrayAccessNode(v, addressSpace, view)
          case _: ScalarType | _: VectorType | _: TupleType => valueAccessNode(v)
          case NoType | UndefType =>
            throw new TypeException(originalType, "A valid type", null)
        }

      case PrivateMemory =>
        privateMems.find(m => m.mem.variable == v) match {
          case Some(typedMemory) => typedMemory.t match {
            case _: ArrayType => arrayAccessNode(v, addressSpace, view)
            case _: ScalarType | _: VectorType | _: TupleType => valueAccessNode(v)
            case NoType | UndefType =>
              throw new TypeException(typedMemory.t, "A valid type", null)
          }
          case _ => valueAccessNode(v)
        }
        
      case UndefAddressSpace | AddressSpaceCollection(_) =>
        throw new IllegalArgumentException(s"Cannot load data from $addressSpace")
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
                              view: View): CAst.VarRef = {
    addressSpace match {
      case LocalMemory | GlobalMemory =>
        ViewPrinter.emit(v, view, replacementsWithFuns) match {
          case OpenCLAST.VarRef(vr, s, i) => VarRef(vr, s, CAst.ArithExpression(i.content))
          case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
        }

      case PrivateMemory =>
        CAst.VarRef(v, suffix = arrayAccessPrivateMem(v, view))
        
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
    "_" + openCLCodeGen.toString(index)
  }

  private def arrayAccessPrivateMemIndex(v: Var, view: View): Int = {
    val declaration = privateDecls(v)
    val originalType = declaration.t
    val valueType = Type.getValueType(originalType)

    val i: ArithExpr = valueType match {
      case _: ScalarType | _: TupleType => ViewPrinter.emit(v, view) match {
        case OpenCLAST.VarRef(_, _, idx) => idx.content
        case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
      }
      // if the original value type is a vector:
      //   divide index by vector length
      case _: VectorType =>
        val length = Type.getLength(Type.getValueType(originalType))
        val index = ViewPrinter.emit(v, view) match {
          case OpenCLAST.VarRef(_, _, idx) => idx.content
          case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
        }
        index / length
      case ArrayType(_) | NoType | UndefType =>
        throw new TypeException(valueType, "A valid non array type", null)
    }

    val real = ArithExpr.substitute(i, replacements).eval

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
  private def componentAccessVectorVar(v: Var, view: View): String = {
    // Compute the index ...
    val index = componentAccessvectorVarIndex(v, view)
    // ... and append it
    ".s" + openCLCodeGen.toString(index)
  }

  private def componentAccessvectorVarIndex(v: Var, view: View): Int = {
    val originalType = varDecls(v)
    val valueType = Type.getValueType(originalType)
    val i = valueType match {
      case _: VectorType =>
        val length = Type.getLength(Type.getValueType(originalType))
        val index = ViewPrinter.emit(v, view) match {
          case OpenCLAST.VarRef(_, _, idx) => idx.content
          case x => throw new MatchError(s"Expected a VarRef, but got ${x.toString}.")
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
  private def valueAccessNode(v: Var): CAst.VarRef = {
    CAst.VarRef(v)
  }
}