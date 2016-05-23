package opencl.generator

import apart.arithmetic._
import arithmetic.TypeVar
import generator.{Generator, Kernel}
import ir._
import ir.ast._
import ir.view._
import opencl.generator.OpenCLAST._
import opencl.ir._
import opencl.ir.ast.OpenCLBuiltInFun
import opencl.ir.pattern._

import scala.collection.{immutable, mutable}


class NotPrintableExpression(msg: String) extends Exception(msg)
class NotI(msg: String) extends Exception(msg)

object OpenCLGenerator extends Generator {
  type NDRange = Array[ArithExpr]

  def generate(f: Lambda): Kernel = {
    generate(f, Array(?, ?, ?))
  }

  def generate(f: Lambda, localSizes: NDRange): Kernel = {
    generate(f, localSizes, Array(?, ?, ?), immutable.Map())
  }

  // Compile a type-checked function into an OpenCL kernel
  def generate(f: Lambda, localSize: NDRange, globalSize: NDRange,
               valueMap: immutable.Map[ArithExpr, ArithExpr]): Kernel = {
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

    val (locals, globals) = memories.partition(_.mem.addressSpace == LocalMemory)
    val globalsFirst = globals ++ locals

    if (AllocateLocalMemoryStatically())
      globalsFirst.partition(isFixedSizeLocalMemory)
    else
      (Array.empty[TypedOpenCLMemory], globalsFirst)
  }

  def substitute(f: Lambda, localSize: NDRange, globalSize: NDRange, valueMap: collection.Map[ArithExpr, ArithExpr]): Lambda = {
    // inject local and global size and valueMap information into the lambda
    val substitutions = collection.mutable.Map[ArithExpr, ArithExpr]() ++ valueMap
    for (i <- 0 to 2) {
      val lclsz = localSize(i)
      if (lclsz != ?)
        substitutions += ((get_local_size(i), lclsz))
      val glbsz = globalSize(i)
      if (glbsz != ?)
        substitutions += ((get_global_size(i), glbsz))
    }

    IRNode.visitArithExpr(f, ArithExpr.substitute(_, substitutions))
      .asInstanceOf[Lambda]
  }

  private[generator] def isFixedSizeLocalMemory: (TypedOpenCLMemory) => Boolean = {
    mem => try {
      mem.mem.size.eval
      mem.mem.addressSpace == LocalMemory
    } catch {
      case _: NotEvaluableException => false
    }
  }
}

class OpenCLGeneratorException(msg: String) extends Exception(msg)
class VariableNotDeclaredError(msg: String) extends OpenCLGeneratorException(msg)

class OpenCLGenerator extends Generator {

  type NDRange = Array[ArithExpr]
  type ValueTable = immutable.Map[ArithExpr, ArithExpr]
  type SymbolTable = immutable.Map[Var, Type]

  private val openCLCodeGen = new OpenCLPrinter

  private var replacements: ValueTable = immutable.Map.empty
  private var replacementsWithFuns: ValueTable = immutable.Map.empty
  private var privateMems = Array[TypedOpenCLMemory]()
  private var privateDecls = immutable.Map[Var, OpenCLAST.VarDecl]()

  private var varDecls: SymbolTable = immutable.Map.empty

  private def printMemories(expr: Expr): Unit = {
    Expr.visit(expr, {
      case e@(call: FunCall) => println(e + "\n    " +
                                        e.mem.toString + " <- " +
                                        call.argsMemory.toString + "\n")
      case e => println(e + "\n    " + e.mem.toString + "\n")
    }, (f: Expr) => {})
  }

  def generate(f: Lambda): Kernel = {
    generate(f, Array(?, ?, ?))
  }

  def generate(f: Lambda, localSizes: NDRange): Kernel = {
    generate(f, localSizes, Array(?, ?, ?), immutable.Map())
  }

  // Compile a type-checked function into an OpenCL kernel
  def generate(f: Lambda, localSize: NDRange, globalSize: NDRange,
               valueMap: immutable.Map[ArithExpr, ArithExpr]): Kernel = {

    assert(localSize.length == 3)
    assert(globalSize.length == 3)

    val newF: Lambda = OpenCLGenerator.substitute(f, localSize, globalSize, valueMap)
    new Kernel(_generate(newF, localSize, globalSize, valueMap), newF)
  }


  def _generate(f: Lambda, localSize: NDRange, globalSize: NDRange,
    valueMap: collection.Map[ArithExpr, ArithExpr]): String  = {

    println(f)

    if (f.body.t == UndefType)
      throw new OpenCLGeneratorException("Lambda has to be type-checked to generate code")

    InferOpenCLAddressSpace(f)

    // Allocate the params and set the corresponding type
    f.params.foreach((p) => {
      p.mem = OpenCLMemory.allocMemory(OpenCLMemory.getSizeInBytes(p.t), p.addressSpace)
    })

    RangesAndCounts(f, localSize, globalSize, valueMap)
    allocateMemory(f)

    if (PerformBarrierElimination())
      BarrierElimination(f)

    checkLambdaIsLegal(f)

    if (Verbose()) {

      println("Types:")
      OpenCLGenerator.printTypes(f.body)

      println("Memory:")
      printMemories(f.body)

      println("Allocated Memory:")
      TypedOpenCLMemory.get(f.body, f.params, includePrivate=true).foreach(m => println(m))
      println()
    }

    View(f)

    val globalBlock = new OpenCLAST.Block(Vector.empty, global = true)

    val containsDouble = Expr.visitWithState(false)(f.body, {
      case (expr, _) if expr.t == Double => true
      case (_, state) => state
    })

    if (containsDouble) {
      globalBlock += Extension("cl_khr_fp64")
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


    tupleTypes.foreach(globalBlock += OpenCLAST.TypeDef(_))

    // pass 2: find and generate user and group functions
    generateUserFunctions(f.body).foreach( globalBlock += _ )
    //generateLookupFunctionsForGroups(f.body).foreach( globalBlock += _ )

    // pass 3: generate the
    //try {
      globalBlock += generateKernel(f)
    //} catch {
    //  case t:Throwable =>
    //    //println("error")//e.printStackTrace()
    //    t.printStackTrace()
    //    throw t
    //}


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
  private def generateUserFunctions(expr: Expr): Seq[Statement] = {
    var fs = Seq[Statement]()

    val userFuns = Expr.visitWithState(Set[UserFun]())(expr, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case _: OpenCLBuiltInFun => set
          case uf: UserFun => set + uf
          case vec: VectorizeUserFun => set + vec.vectorizedFunction
          case _ => set
        }
        case _ => set
      })

    userFuns.foreach(uf => {

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
  /*
  private def generateLookupFunctionsForGroups(expr: Expr): Seq[OclAstNode] = {
    var fs = Seq[OclAstNode]()

    val groupFuns = Expr.visitWithState(Set[Group]())(expr, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case group: Group => set + group
          case _ => set
        }
        case _ => set
      })

    groupFuns.foreach(group => {
      fs = fs :+ OpenCLAST.Function(
        name = s"lookup${group.id}",
        ret = Int,
        params = List(
          OpenCLAST.ParamDecl("i", Int)
        ),
        body = OpenCLAST.Block(Vector(OpenCLAST.OpenCLCode(
          s"""|  int table[] = {${group.posIndices.deep.mkString(", ")}};
              |  return table[i];
              |""".stripMargin
        ))))
    })

    fs
  }
  */

  def allocateMemory(f: Lambda): Unit = {
    OpenCLMemoryAllocator(f)
    Kernel.memory = TypedOpenCLMemory.get(f.body, f.params).toArray
  }

  private object Kernel {
    var memory = Array.empty[TypedOpenCLMemory]
    var staticLocalMemory = Array.empty[TypedOpenCLMemory]
  }

  private def generateKernel(f: Lambda): Statement = {

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
      typedMems.map(tm => {
        if (tm.mem.addressSpace == PrivateMemory) {
          // do not devectorize for private memory
          (tm.mem.variable, tm.t)
        } else {
          (tm.mem.variable, Type.devectorize(tm.t))
        }
      }).toMap

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
    })

    generate(f.body, kernel.body)

    if (CSE())
      findAndDeclareCommonSubTerms(kernel.body)

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
        case m: MapAtomWrg => generateMapAtomWrgCall(m, call, block)
        case m: MapLcl => generateMapLclCall(m, call, block)
        case m: MapAtomLcl => generateMapAtomLclCall(m, call, block)
        case m: MapWarp => generateMapWarpCall(m, call, block)
        case m: MapLane => generateMapLaneCall(m, call, block)
        case m: MapSeq => generateMapSeqCall(m, call, block)
        case _: Map =>

        case r: ReduceSeq => generateReduceSeqCall(r, call, block)

        case bs: BSearch => generateBSearchCall(bs, call, block)
        case ls: LSearch => generateLSearchCall(ls, call, block)
        case _ : Search => 

        case i: Iterate => generateIterateCall(i, call, block)

        case vec: VectorizeUserFun => generateUserFunCall(vec.vectorizedFunction, call, block)
        case u : UserFun => generateUserFunCall(u, call, block)

        case fp: FPattern => generate(fp.f.body, block)
        case l: Lambda => generate(l.body, block)
        case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
             Split(_) | Join() | Slide(_,_) | Zip(_) | Tuple(_) | Filter() |
             Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Pad(_,_,_) =>

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
    generateForLoop(block, m.loopVar, (b) => generate(m.f.body, b))//,m.iterationCount)
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
    generateForLoop(block, m.loopVar, (b) => generate(m.f.body, b))//, m.iterationCount)
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
    block.asInstanceOf[Block] += Comment("atomic_workgroup_map")

    // get shorthands for the loopvar/workvar
    val loopVar = m.loopVar
    val workVar = m.workVar
    // val threadid = new get_local_id(0 : Int)
    val threadid = get_local_id(0)

    // wrap the task update/getting functionality in a variable, as we need to use it twice
    def atomicGetTask(ob: Block) = {
      generateConditional(ob, Predicate(threadid, 0, Predicate.Operator.==), 
      (b) => {
        b.asInstanceOf[Block] += AssignmentExpression(OpenCLAST.ArithExpression(loopVar),
          FunctionCall("atomic_inc", List(OpenCLAST.VarRef(workVar))))
      },(_) => {})
    }

    // declare a global variable holding the next index to process, and assign it a value
    nestedBlock += OpenCLAST.VarDecl(workVar, opencl.ir.IntPtr,
      OpenCLAST.ArithExpression(m.globalTaskIndex.variable), addressSpace=GlobalMemory)
    // initialise it to zero
//    generateConditional(nestedBlock, Predicate(new get_global_id(0), 0, Predicate.Operator.==),
//      (b) => {
//        b += OpenCLAST.AssignmentExpression(
//          OpenCLAST.FunctionCall("*",List(OpenCLAST.Expression(workVar))), OpenCLAST.Expression(0))
//      }, (_) => {}
//    )

    // declare an index for this thread, the loop variable, and give it a value from the task index
    // this must be done in a separate statement, as the variable is in LocalMemory, and
    // we only wish for the first thread in the workgroup to perform the operation
    nestedBlock += OpenCLAST.VarDecl(loopVar, opencl.ir.Int, addressSpace=LocalMemory)
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
        b.asInstanceOf[Block] += OpenCLAST.Barrier(OpenCLMemory(workVar, 4, LocalMemory))
      })
    block.asInstanceOf[Block] += nestedBlock

    // emit a barrier?
    if (m.emitBarrier)
      block.asInstanceOf[Block] += OpenCLAST.Barrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // MapLcl
  private def generateMapLclCall(m: MapLcl,
                                 call: FunCall,
                                 block: Block): Unit = {
    generateForLoop(block, m.loopVar, (b) => generate(m.f.body, b),/* m.iterationCount, */shouldUnrollLoop(call))

    if (m.emitBarrier)
      block.asInstanceOf[Block] += OpenCLAST.Barrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // MapAtomLcl 
  private def generateMapAtomLclCall(m: MapAtomLcl, 
                                     call: FunCall, 
                                     block: Block): Unit = {

    // build a new nested block
    val nestedBlock = Block(Vector.empty)
    block.asInstanceOf[Block] += OpenCLAST.Comment("atomic_local_map")

    // get shorthands for the loopvar/workvar
    val loopVar = m.loopVar
    val workVar = m.workVar
    val workVarPtr = OpenCLAST.FunctionCall("&",List(OpenCLAST.VarRef(workVar)))

    // declare a local variable holding the next index to process, and assign it a value
    // this must be done in a separate statement, as the variable is in LocalMemory
    nestedBlock += OpenCLAST.VarDecl(workVar, opencl.ir.Int, addressSpace=LocalMemory)
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
        b.asInstanceOf[Block] += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(loopVar),
          OpenCLAST.FunctionCall("atomic_inc", List(workVarPtr))
        )
      })
    block.asInstanceOf[Block] += nestedBlock

    // emit a barrier?
    if (m.emitBarrier)
      block.asInstanceOf[Block] += OpenCLAST.Barrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  // TODO: Separate pass. Information needed elsewhere
  private def shouldUnrollLoop(call: FunCall): Boolean = {
    var originalType: Type = UndefType
    try {
      originalType = getOriginalType(call.args.head.mem.asInstanceOf[OpenCLMemory])
    } catch {
      case _: VariableNotDeclaredError =>
    }
    val currentType = call.args.head.t

    val loopingOverVectorComponents = (originalType, currentType) match {
      case (_: VectorType, ArrayType(_: ScalarType, _)) => true
      case _ => false
    }

    loopingOverVectorComponents ||
    (OpenCLMemory.containsPrivateMemory(call.args.head.mem)
      && (call.args.head.mem match {
      case coll: OpenCLMemoryCollection =>
        coll.subMemories.exists(mem => existsInPrivateMemories(mem))
      case _ => existsInPrivateMemories(call.args.head.mem)
    })) ||
      // Don't unroll just for value
      OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == PrivateMemory
  }

  private def existsInPrivateMemories(mem: Memory): Boolean =
    privateMems.exists(_.mem == mem)

  // MapWarp
  private def generateMapWarpCall(m: MapWarp,
                                  call: FunCall,
                                  block: Block): Unit = {
    generateForLoop(block, m.loopVar, (b) => generate(m.f.body, b)/*,
                 m.iterationCount*/)
    call.mem match {
      case m : OpenCLMemory => block.asInstanceOf[Block] += OpenCLAST.Barrier(m)
      case _ =>
    }
  }

  // MapLane
  private def generateMapLaneCall(m: MapLane,
                                  call: FunCall,
                                  block: Block): Unit = {
    generateForLoop(block, m.loopVar, (b) => generate(m.f.body, b)/*,
                 m.iterationCount*/)
  }

  // MapSeq
  private def generateMapSeqCall(m: MapSeq,
                                 call: FunCall,
                                 block: Block): Unit = {
    val unroll =  m.isInstanceOf[MapSeqUnroll] || shouldUnrollLoop(call)
    block.asInstanceOf[Block] += OpenCLAST.Comment("map_seq")
    generateForLoop(block, m.loopVar, (b) => generate(m.f.body, b), unroll)
    block.asInstanceOf[Block] += OpenCLAST.Comment("end map_seq")
  }

  // Expr should be an array
  private def generateLength(e:Expr) : Either[Expression, ArithExpr] = {
    e match {
      case e: Expr =>
        e.t match {
          case a: UnknownLengthArrayType =>
            val arrayStart = ViewPrinter.emit(e.view)
            Left(VarRef(e.mem.variable, arrayIndex=ArithExpression(arrayStart)))
          case a: ArrayType => Right(a.len)
        }
    }
  }

  // === Reduce ===
  private def generateReduceSeqCall(r: AbstractReduce,
                                    call: FunCall,
                                    block: Block): Unit = {
    // TODO: Separate pass. Information needed elsewhere
    val unroll = r.isInstanceOf[ReduceSeqUnroll] || OpenCLMemory.containsPrivateMemory(call.args(1).mem)

    val innerBlock = OpenCLAST.Block(Vector.empty)
    block.asInstanceOf[Block] += OpenCLAST.Comment("reduce_seq")

    val inputLen = generateLength(call.args(1))
    inputLen match {

      case Left(len:Expression) =>
        val indexVar = r.loopVar
        val range = indexVar.range.asInstanceOf[RangeAdd]

        val init = ArithExpression(range.start)
        val cond =  CondExpression(ArithExpression(r.loopVar), len, CondExpression.Operator.<)
        val increment = AssignmentExpression(ArithExpression(r.loopVar), ArithExpression(r.loopVar + range.step))

        block.asInstanceOf[Block] += OpenCLAST.ForLoop(VarDecl(r.loopVar, opencl.ir.Int, init, PrivateMemory), ExpressionStatement(cond), increment, innerBlock)

        generate(r.f.body, innerBlock)

      case Right(len:ArithExpr) =>
        generateForLoop(block, r.loopVar, (b) => generate(r.f.body, b), /*r.iterationCount, */unroll)
    }

    block.asInstanceOf[Block] += OpenCLAST.Comment("end reduce_seq")
  }


  // BSearch
  private def generateBSearchCall(s: AbstractSearch,
                                  call: FunCall,
                                  block: Block): Unit = {
    val nestedBlock = OpenCLAST.Block(Vector.empty)
    block.asInstanceOf[Block] += OpenCLAST.Comment("binary_search")
    // get the default value handily
    val defaultVal = call.args(0)
    // get the input handily
    val inArr = call.args(1)
    // get the type of the input (handily, concretely)
    val inArrT = inArr.t match {
      case t: ArrayType => t
    }
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
    nestedBlock += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(upperIndex), OpenCLAST.ArithExpression(inArrT.len))
    // Declare a variable to copy the result of the user function into
    // We have to do this, as we currently have no nice way of describing normal C statements
    // in a way that works private memory properly.
    // TODO: Find some way of representing the arguments to while/if/etc...
    val compFuncResVar = Var("cmp_res_var")
    varDecls = varDecls.updated(compFuncResVar, Type.devectorize(s.f.body.t))
    // create a memory object for it
    val compFuncResMem = OpenCLMemory(compFuncResVar, OpenCLMemory.getSizeInBytes(s.f.body.t), PrivateMemory)
    // set the memory of the call to the mem
    s.f.body.mem = compFuncResMem
    // declare it, with the same type as the comparison result
    nestedBlock += OpenCLAST.VarDecl(compFuncResVar, s.f.body.t)

    // create a variable for each goto label
    val finishLabel = Var("done")
    val writeResultLabel = Var("writeresult")
    generateWhileLoop(nestedBlock, Predicate(lowerIndex,upperIndex,Predicate.Operator.<),
      (b) => {
        b.asInstanceOf[Block] += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(s.indexVar),
          OpenCLAST.ArithExpression(lowerIndex + (upperIndex - lowerIndex) / 2))

        generate(s.f.body, b)

        generateConditional(b,
          Predicate(compFuncResVar, 0, Predicate.Operator.<),
          (cb) => {
            cb.asInstanceOf[Block] += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(upperIndex),OpenCLAST.ArithExpression(s.indexVar))
          },
          (cb) => {
            generateConditional(cb,
              Predicate(compFuncResVar, 0, Predicate.Operator.>),
              (ccb) => {ccb.asInstanceOf[Block] += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(lowerIndex),OpenCLAST.ArithExpression(s.indexVar + 1))},
              (ccb) => {ccb.asInstanceOf[Block] += OpenCLAST.GOTO(writeResultLabel)}
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
    block.asInstanceOf[Block] += nestedBlock
    block.asInstanceOf[Block] += OpenCLAST.Comment("binary_search")
  }

  // LSearch
  private def generateLSearchCall(s: AbstractSearch,
                                  call: FunCall,
                                  block: Block): Unit = {
    val nestedBlock = OpenCLAST.Block(Vector.empty)
    block.asInstanceOf[Block] += OpenCLAST.Comment("linear_search")
    // get the default value handily
    val defaultVal = call.args(0)
    // get the input handily
    val inArr = call.args(1)
    // get the type of the input (handily, concretely)
    val inArrT = inArr.t match {
      case t: ArrayType => t
    }
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
    val compFuncResMem = OpenCLMemory(compFuncResVar, OpenCLMemory.getSizeInBytes(s.f.body.t), PrivateMemory)
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
    generateWhileLoop(nestedBlock, Predicate(s.indexVar, inArrT.len,Predicate.Operator.<),
      (b) => {

        generate(s.f.body, b)

        generateConditional(b,
          // if the result of the comparator is greater than zero, the element we're currently
          // comparing to is smaller than the element we are searching for
          Predicate(compFuncResVar, 0, Predicate.Operator.>),
          (cb) => {
            // therefore, keep searching!
            cb.asInstanceOf[Block] += OpenCLAST.AssignmentExpression(OpenCLAST.ArithExpression(s.indexVar),OpenCLAST.ArithExpression(s.indexVar + 1))
          },
          (cb) => {
            // else...
            generateConditional(cb,
              // if the result is less than 0, we've gone past the value we're looking for, so abort
              Predicate(compFuncResVar, 0, Predicate.Operator.<),
              // if the value is greater than, it's gone past! the search has failed.
              (ccb) => {
                ccb.asInstanceOf[Block] += OpenCLAST.GOTO(searchFailedLabel)
              },
              // otherwise, it must be equal to, so jump to returning the result
              (ccb) => {
                ccb.asInstanceOf[Block] += OpenCLAST.GOTO(writeResultLabel)
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
    block.asInstanceOf[Block] += nestedBlock
    block.asInstanceOf[Block] += OpenCLAST.Comment("linear_search")
  }

  private def generateValue(v: Value, block: Block): Unit = {
    val temp = Var("tmp")

    block.asInstanceOf[Block] += OpenCLAST.VarDecl(temp, Type.getValueType(v.t),
      init = OpenCLAST.OpenCLCode(v.value))
    block.asInstanceOf[Block] += OpenCLAST.AssignmentExpression(
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

    block.asInstanceOf[Block] +=
      OpenCLAST.VarDecl(curOutLen, Int,
                        OpenCLAST.ArithExpression(Type.getLength(call.argsType)))

    // create new temporary input and output pointers
    val tin = Var("tin")
    val tout = Var("tout")
    varDecls = varDecls.updated(tin, Type.devectorize(call.t))
    varDecls = varDecls.updated(tout, Type.devectorize(call.t))

    // ADDRSPC TYPE tin = in;
    block.asInstanceOf[Block] += OpenCLAST.VarDecl(tin, Type.devectorize(call.t),
                               OpenCLAST.VarRef(inputMem.variable),
                               outputMem.addressSpace)

    val range = i.indexVar.range.asInstanceOf[RangeAdd]

    // ADDRSPC TYPE tin = (odd ? out : swap);
    block.asInstanceOf[Block] += OpenCLAST.VarDecl(tout, Type.devectorize(call.t),
      init = OpenCLAST.ArithExpression(
        ((range.stop % 2) ne Cst(0)) ?? outputMem.variable !! swapMem.variable),
        addressSpace = outputMem.addressSpace)

    generateForLoop(block, i.indexVar, (b) => {

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

      val curOutLenRef = OpenCLAST.VarRef(curOutLen)

      val innerOutputLength = Type.getLength(funCall.t)

      // tmp = tmp * outputLen / inputLen
      b.asInstanceOf[Block] += OpenCLAST.AssignmentExpression(curOutLenRef,
        OpenCLAST.ArithExpression(innerOutputLength))


      val tinVStrRef = OpenCLAST.VarRef(tin)

      // tin = (tout == swap) ? swap : out
      b.asInstanceOf[Block] += OpenCLAST.AssignmentExpression(tinVStrRef,
        OpenCLAST.ArithExpression((tout eq swapMem.variable) ??
                             swapMem.variable !!  outputMem.variable))


      val toutVStrRef = OpenCLAST.VarRef(tout)

      // tout = (tout == swap) ? out : swap
      b.asInstanceOf[Block] += OpenCLAST.AssignmentExpression(toutVStrRef,
        OpenCLAST.ArithExpression((tout eq swapMem.variable) ??
                             outputMem.variable !! swapMem.variable))

      if(outputMem.addressSpace != PrivateMemory)
        b.asInstanceOf[Block] += OpenCLAST.Barrier(outputMem)

    }/*, i.iterationCount*/)
  }

  private def generateForLoop(block: Block,
                              indexVar: Var,
                              generateBody: (Block) => Unit,
                              /*iterationCountExpr: ArithExpr = ?,*/
                              needUnroll: Boolean = false): Unit = {

    val range = indexVar.range.asInstanceOf[RangeAdd]
    val step = range.step
    val init = ArithExpression(range.start)
    val stop = range match {
      case ra : RangeAdd => ra.stop
      case _ => throw new OpenCLGeneratorException("Cannot handle range for ForLoop: " + range)
    }
    val cond =  CondExpression(ArithExpression(indexVar), ArithExpression(stop), CondExpression.Operator.<)

    // if we need to unroll (e.g. because of access to private memory)
    if (needUnroll) {
      val iterationCount = try {
        indexVar.range.numVals.eval
      } catch {
        case _: NotEvaluableException =>
          throw new OpenCLGeneratorException("Trying to unroll loop, but iteration count " +
            "could not be determined statically.")
      }

      if (iterationCount > 0) {
        block.asInstanceOf[Block] += OpenCLAST.Comment("unroll")

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

        block.asInstanceOf[Block] += OpenCLAST.Comment("end unroll")
        return
      } else {
        throw new OpenCLGeneratorException(s"Trying to unroll loop, but iteration count is $iterationCount.")
      }

    }

    // TODO: Information needed elsewhere. See analysis.ControlFlow
    // try to see if we really need a loop
    indexVar.range.numVals match {
      case Cst(0) =>
        // zero iteration
        block.asInstanceOf[Block] += OpenCLAST.Comment("iteration count is 0, no loop emitted")
        return
      case Cst(1) =>
        // one iteration
        block.asInstanceOf[Block] += OpenCLAST.Comment("iteration count is exactly 1, no loop emitted")
        val innerBlock = OpenCLAST.Block(Vector.empty)
        innerBlock += OpenCLAST.VarDecl(indexVar, opencl.ir.Int, init, PrivateMemory)
        generateBody(innerBlock)
        block.asInstanceOf[Block] += innerBlock
        return

      // TODO: See TestOclFunction.numValues and issue #62
      case numVals if range.start.min.min == Cst(0) && range.stop == Cst(1) =>
        generateIfStatement(block, indexVar, generateBody, init, stop)
        return
      case _ =>
        (indexVar.range.numVals.min,indexVar.range.numVals.max) match {
          case (Cst(0),Cst(1)) =>
            // one or less iteration
            generateIfStatement(block, indexVar, generateBody, init, stop)
            return

          case _ =>
        }
    }

    val increment = AssignmentExpression(ArithExpression(indexVar), ArithExpression(indexVar + range.step))
    val innerBlock = OpenCLAST.Block(Vector.empty)
    block.asInstanceOf[Block] += OpenCLAST.ForLoop(VarDecl(indexVar, opencl.ir.Int, init, PrivateMemory), ExpressionStatement(cond), increment, innerBlock)
    generateBody(innerBlock)
  }

  private def generateIfStatement(block: Block, indexVar: Var, generateBody: (Block) => Unit, init: ArithExpression, stop: ArithExpr): Unit = {
    block.asInstanceOf[Block] += OpenCLAST.Comment("iteration count is exactly 1 or less, no loop emitted")
    val innerBlock = OpenCLAST.Block(Vector.empty)
    innerBlock += OpenCLAST.VarDecl(indexVar, opencl.ir.Int, init, PrivateMemory)
    block.asInstanceOf[Block] += OpenCLAST.IfThenElse(CondExpression(init, ArithExpression(stop), CondExpression.Operator.<), innerBlock)
    generateBody(innerBlock)
  }

  private def generateWhileLoop(block: Block,
                                loopPredicate: Predicate,
                                generateBody: (Block) => Unit) : Unit = {
    val innerBlock = OpenCLAST.Block(Vector.empty)
    block.asInstanceOf[Block] += OpenCLAST.WhileLoop(loopPredicate, body = innerBlock)
    generateBody(innerBlock)
  }

  private def generateConditional(block: Block,
                                  switchPredicate: Predicate,
                                  genTrueBranch: (Block) => Unit,
                                  genFalseBranch: (Block) => Unit ) : Unit = {
    val trueBlock = OpenCLAST.Block(Vector.empty)
    val falseBlock = OpenCLAST.Block(Vector.empty)
    block.asInstanceOf[Block] += OpenCLAST.IfThenElse(switchPredicate, trueBody = trueBlock, falseBody = falseBlock)
    genTrueBranch(trueBlock)
    genFalseBranch(falseBlock)
  }

  private def generateUserFunCall(u: UserFun,
                                  call: FunCall,
                                  block: Block): Block = {
    // Handle vector assignments for vector types
    val mem = OpenCLMemory.asOpenCLMemory(call.mem)
    block.asInstanceOf[Block] += generateStoreNode(mem, call.t, call.outputView,
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
          if Type.isEqual(Type.getValueType(at), vt.scalarT)
            && (   mem.addressSpace == GlobalMemory
            || mem.addressSpace == LocalMemory  ) =>

          OpenCLAST.Store(
            OpenCLAST.VarRef(mem.variable), vt,
            value = value,
            offset = OpenCLAST.ArithExpression(
              ViewPrinter.emit(view,
                replacementsWithFuns) / vt.len))
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
              OpenCLAST.Cast(OpenCLAST.VarRef(mem.variable), st)

            // originally an array of scalar values in global memory,
            // but now a vector type
            //  => emit vload
            case (at: ArrayType, vt: VectorType)
              if Type.isEqual(Type.getValueType(at), vt.scalarT)
                && (mem.addressSpace == GlobalMemory || mem.addressSpace == LocalMemory) =>

              OpenCLAST.Load(OpenCLAST.VarRef(mem.variable), vt,
                offset = OpenCLAST.ArithExpression(
                  ViewPrinter.emit(view,
                    replacementsWithFuns) / vt.len) )

            // originally an array of scalar values in private memory,
            // but now a vector type
            //  => emit (float2)(f1, f2) primitive
            case (at: ArrayType, vt: VectorType)
              if Type.isEqual(Type.getValueType(at), vt.scalarT)
                && (mem.addressSpace == PrivateMemory) =>

              assert( privateMems.exists(m => m.mem == mem) )

              // TODO: this seems like a very specific local solution ... find a more generic proper one

              // iterate over the range, assuming that it is contiguous
              val arraySuffixStartIndex: Int = arrayAccessPrivateMemIndex(mem.variable, view)
              val arraySuffixStopIndex: Int = arraySuffixStartIndex + vt.len.eval

              val seq = (arraySuffixStartIndex until arraySuffixStopIndex).map(i => {
                OpenCLAST.VarRef(mem.variable, suffix = "_" + i)
              })

              OpenCLAST.VectorLiteral(vt, seq:_*)

            // originally a vector value in private memory,
            // but now a scalar type
            //  => emit load from components
            case (vt: VectorType, st: ScalarType)
              if Type.isEqual(st, vt.scalarT)
                && (mem.addressSpace == PrivateMemory) =>

              val componentSuffix = componentAccessVectorVar(mem.variable, view)
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

              val componentSuffix = componentAccessVectorVar(mem.variable, view)
              OpenCLAST.VarRef(mem.variable, suffix = arraySuffix + componentSuffix)

            // originally an array of vector values in private memory,
            // but now a different vector type
            //  => emit load from components
            case (at: ArrayType, vt: VectorType)
              if Type.getValueType(at).isInstanceOf[VectorType]
                && Type.haveSameBaseTypes(at, vt)
                && (mem.addressSpace == PrivateMemory) =>

              // TODO: this seems like a very specific local solution ... find a more generic proper one

              assert( privateMems.exists(m => m.mem == mem) )

              val arraySuffix = arrayAccessPrivateMem(mem.variable, view)

              val componentSuffixStartIndex = componentAccessvectorVarIndex(mem.variable, view)
              val componentSuffixStopIndex = componentSuffixStartIndex + vt.len.eval

              // iterate over the range, assuming that it is contiguous
              val componentSuffix = (componentSuffixStartIndex until componentSuffixStopIndex).foldLeft(".s")(_+_)

              OpenCLAST.VarRef(mem.variable, suffix = arraySuffix + componentSuffix)

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
                  val index = ViewPrinter.emit(innerView,
                    replacementsWithFuns)
                  OpenCLAST.VarRef(mem.variable, arrayIndex = OpenCLAST.ArithExpression(index), suffix = suffix)

                case PrivateMemory =>

                  val arraySuffix =
                    if (privateMems.exists(m => m.mem == mem)) // check if this is actually an array
                      arrayAccessPrivateMem(mem.variable, innerView)
                    else // Workaround for values
                      ""
                  OpenCLAST.VarRef(mem.variable, suffix = arraySuffix + suffix)
              }
          }
        }
    }
  }

  private def getOriginalType(mem: OpenCLMemory): Type = {

    try {
      varDecls(mem.variable)
    } catch {
      case _: NoSuchElementException =>
        throw new VariableNotDeclaredError(s"Trying to generate access to variable " +
          s"${mem.variable} which was not previously declared.")
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
                         view: View): OpenCLAST.VarRef = {
    addressSpace match {
      case LocalMemory | GlobalMemory =>
        val originalType = varDecls(v)
        originalType match {
          case _: ArrayType => arrayAccessNode(v, addressSpace, view)
          case _:ScalarType | _:VectorType | _:TupleType => valueAccessNode(v)
        }

      case PrivateMemory =>
        privateMems.find(m => m.mem.variable == v) match {
          case Some(typedMemory) => typedMemory.t match {
            case _:ArrayType => arrayAccessNode(v, addressSpace, view)
            case _:ScalarType | _:VectorType | _:TupleType => valueAccessNode(v)
          }
          case _ => valueAccessNode(v)
        }
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
                              view: View): OpenCLAST.VarRef = {
    addressSpace match {
      case LocalMemory | GlobalMemory =>
        val index = ViewPrinter.emit(view, replacementsWithFuns)
        OpenCLAST.VarRef(v, arrayIndex = OpenCLAST.ArithExpression(index))

      case PrivateMemory =>
        OpenCLAST.VarRef(v, suffix = arrayAccessPrivateMem(v, view))
    }
  }

  /**
   * Generating the suffix appended to emulate an array access in private memory
    *
    * @param v The variable to access
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

    val i = valueType match {
      case _:ScalarType | _:TupleType => ViewPrinter.emit(view)
      // if the original value type is a vector:
      //   divide index by vector length
      case _:VectorType =>
        val length = Type.getLength(Type.getValueType(originalType))
        ViewPrinter.emit(view) / length
    }

    val real = ArithExpr.substitute(i, replacements).eval
    println(declaration.length)

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
      case _:VectorType =>
        val length = Type.getLength(Type.getValueType(originalType))
        ViewPrinter.emit(view) % length
    }

    ArithExpr.substitute(i,replacements).eval
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

  private def findAndDeclareCommonSubTerms(block: Block): Unit = {
    visitBlocks(block, process)

    def process(block: Block): Unit = {
      // get all the arithmetic expressions from this block
      var expressions = Seq[ArithExpression]()
      visitExpressionsInBlock(block, {
        case e: ArithExpression => expressions = expressions :+ e
        case _ =>
      })

      // map for counting how often subterms appear
      val counts = mutable.Map[ArithExpr, Int]()

      // count how many times a subterm appears in the expressions
      expressions.foreach({
        case ae : ArithExpression =>
          ArithExpr.visit(ae.content, subterm => {
            counts get subterm match {
              case None => counts put (subterm, 1)
              case Some(c) => counts put (subterm, c + 1)
            }
          })
        case _ =>
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

      val substitutions = mutable.Map[ArithExpr, ArithExpr]()

      val newVarDecls: mutable.Iterable[VarDecl] =
        // for every subterm p._1 ...
        subterms.map( p => {
          // ... create a new variable ...
          val newVar = Var("")
          // ... and prepare the substitution of the old expression with the
          //     new variable
          substitutions put (p._1, newVar)

          OpenCLAST.VarDecl(newVar,
                            t = Int,
                            init = OpenCLAST.ArithExpression(p._1),
                            addressSpace = PrivateMemory)
        })

      // update the Expression nodes to
      expressions.foreach {
        case ae: ArithExpression => ae.content = ArithExpr.substitute(ae.content, substitutions.toMap)
      }

      // find actually used variables
      val usedVars: mutable.Set[VarDecl] = mutable.Set()
      expressions.foreach(expr => {
        ArithExpr.visit(expr.content, {
          case v: Var => newVarDecls.find(_.v == v) match {
            case Some(s) => usedVars += s
            case None =>
          }
          case _ => // just interested in used vars
        })
      })

      // introduce new var decls at the beginning of the current block
      if (DeadCodeElimination())
        usedVars.foreach(_ :: block.asInstanceOf[Block])
      else
        newVarDecls.foreach(_ :: block.asInstanceOf[Block])
    }
  }
}
