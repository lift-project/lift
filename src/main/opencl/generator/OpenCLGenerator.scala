package opencl.generator

import apart.arithmetic._
import arithmetic.TypeVar
import generator.Generator
import ir._
import ir.ast._
import ir.view._
import opencl.generator.OpenCLAST._
import opencl.ir._
import opencl.ir.pattern._
import scala.collection.{mutable, immutable}

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

object CSE {
  val cse = System.getenv("APART_CSE") != null
  def apply() = cse
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
      p.view = View(p.t, openCLCodeGen.toString(p.mem.variable))
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

    val containsDouble = Expr.visitWithState(false)(f.body, {
      case (expr, _) if expr.t == Double => true
      case (_, state) => state
    })

    if (containsDouble)
      globalBlock += Extension("cl_khr_fp64")

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

    groupFuns.foreach(group => {
      fs = fs :+ OpenCLAST.Function(
        name = s"groupComp${group.id}",
        ret = Int,
        params = List(
          OpenCLAST.ParamDecl("j", Int),
          OpenCLAST.ParamDecl("i", Int)
        ),
        body = OpenCLAST.Block(Vector(OpenCLAST.OpenCLCode(
          s"""|  // Compute new index
              |  int relIndices[] = {${group.relIndices.deep.mkString(", ")}};
              |  return j + relIndices[i];
              |""".stripMargin
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
          length = (x.mem.size /^ Type.getSize(Type.getValueType(x.t))).eval))

    generate(f.body, kernel.body)

    if (CSE())
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

        case bs: BSearch => generateBSearchCall(bs, call, block)
        case ls: LSearch => generateLSearchCall(ls, call, block)
        case _ : Search => 

        case i: Iterate => generateIterateCall(i, call, block)

        case vec: VectorizeUserFun => generateUserFunCall(vec.vectorizedFunction, call, block)
        case u : UserFun => generateUserFunCall(u, call, block)

        case fp: FPattern => generate(fp.f.body, block)
        case l: Lambda => generate(l.body, block)
        case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
             Split(_) | Join() | Group(_) | Zip(_) | Tuple(_) | Filter() |
             Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Pad(_,_) =>

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
                 m.iterationCount, shouldUnrollLoop(call))

    if (m.emitBarrier)
      block += OpenCLAST.Barrier(call.mem.asInstanceOf[OpenCLMemory])
  }

  def shouldUnrollLoop(call: FunCall): Boolean = {
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
    val unroll = shouldUnrollLoop(call)
    println(m)
    println(m.iterationCount)
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

    val nestedBlock = OpenCLAST.Block(Vector.empty)

    nestedBlock += OpenCLAST.Comment("reduce_seq")
    generateLoop(nestedBlock, r.loopVar, (b) => generate(r.f.body, b),
                 r.iterationCount, unroll)
    nestedBlock += OpenCLAST.Comment("end reduce_seq")

    block += nestedBlock
  }

  // BSearch
  private def generateBSearchCall(s: AbstractSearch,
                                  call: FunCall,
                                  block: Block): Unit = {
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
    val inArrRef = generateLoadNode(inArr.mem.variable, clInArrMem.addressSpace, 
                      inArrT, inArr.view.access(s.indexVar))
    // declare temporary vars for declaring the upper and lower indicies of the search
    val lowerIndex = Var("li")
    val upperIndex = Var("ui")
    // declare the lower and upper indicies
    block += OpenCLAST.VarDecl(lowerIndex.toString, opencl.ir.Int)
    block += OpenCLAST.VarDecl(upperIndex.toString, opencl.ir.Int)
    block += OpenCLAST.VarDecl(s.indexVar.toString, opencl.ir.Int)
    // assign initial values
    block += OpenCLAST.Assignment(OpenCLAST.Expression(lowerIndex), OpenCLAST.Expression(0))
    block += OpenCLAST.Assignment(OpenCLAST.Expression(upperIndex), OpenCLAST.Expression(inArrT.len))
    // Don't need to do this assignment here
    // block += OpenCLAST.Assignment(OpenCLAST.Expression(s.indexVar), 
      // OpenCLAST.Expression((lowerIndex + upperIndex) / 2))
    // Declare a variable to copy the result of the user function into
    // We have to do this, as we currently have no nice way of describing normal C statements
    // in a way that works private memory properly. 
    // TODO: Find some way of representing the arguments to while/if/etc...
    val compFuncResVar = Var("cmp_res_var")
    // declare it, with the same type as the comparison result
    block += OpenCLAST.VarDecl(compFuncResVar.toString, s.f.body.t)
    // get an AST node describing a load from the comparator function result
    // val cmpResMemVar = s.f.body.mem.variable
    val compResRef = generateLoadNode(s.f.body.mem.variable, 
                                        OpenCLMemory.asOpenCLMemory(s.f.body.mem).addressSpace,
                                        s.f.body.t, s.f.body.view)
    generateWhileLoop(block, Predicate(lowerIndex,upperIndex,Predicate.Operator.<), 
      (b) => {
        b += OpenCLAST.Assignment(OpenCLAST.Expression(s.indexVar), 
          OpenCLAST.Expression((lowerIndex + upperIndex) / 2))
        generate(s.f.body, b)
        b += OpenCLAST.Assignment(OpenCLAST.Expression(compFuncResVar), compResRef)
        generateConditional(b, 
          Predicate(compFuncResVar, 0, Predicate.Operator.<),
          (cb) => {
            cb += OpenCLAST.Assignment(OpenCLAST.Expression(upperIndex),OpenCLAST.Expression(s.indexVar))
          },
          (cb) => {
            generateConditional(cb, 
              Predicate(compFuncResVar, 0, Predicate.Operator.>),
              (ccb) => {ccb += OpenCLAST.Assignment(OpenCLAST.Expression(lowerIndex),OpenCLAST.Expression(s.indexVar))},
              (ccb) => {ccb += OpenCLAST.GOTO("writeresult")}
            )
          }
        )
      }
    )
    block += generateStoreNode(block, OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.view.access(Cst(0)),
      generateLoadNode(defaultVal.mem.variable, OpenCLMemory.asOpenCLMemory(defaultVal.mem).addressSpace, defaultVal.t, defaultVal.view))
    block += OpenCLAST.GOTO("done")
    block += OpenCLAST.Label("writeresult")
    block += generateStoreNode(block, 
      OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.view.access(Cst(0)),
      inArrRef)
    block += OpenCLAST.Label("done")
    block += OpenCLAST.OpenCLCode("\n")
  }

  // LSearch 
  private def generateLSearchCall(s: AbstractSearch,
                                  call: FunCall,
                                  block: Block): Unit = {
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
    val inArrRef = generateLoadNode(inArr.mem.variable, clInArrMem.addressSpace, 
                      inArrT, inArr.view.access(s.indexVar))
    // declare the index var
    block += OpenCLAST.VarDecl(s.indexVar.toString, opencl.ir.Int)
    // assign initial values
    block += OpenCLAST.Assignment(OpenCLAST.Expression(s.indexVar), OpenCLAST.Expression(0))
    // Declare a variable to copy the result of the user function into
    // We have to do this, as we currently have no nice way of describing normal C statements
    // in a way that works private memory properly. 
    // TODO: Find some way of representing the arguments to while/if/etc...
    val compFuncResVar = Var("cmp_res_var")
    // declare it, with the same type as the comparison result
    block += OpenCLAST.VarDecl(compFuncResVar.toString, s.f.body.t)
    // get an AST node describing a load from the comparator function result
    // val cmpResMemVar = s.f.body.mem.variable
    val compResRef = generateLoadNode(s.f.body.mem.variable, 
                                        OpenCLMemory.asOpenCLMemory(s.f.body.mem).addressSpace,
                                        s.f.body.t, s.f.body.view)
    generateWhileLoop(block, Predicate(s.indexVar,inArrT.len,Predicate.Operator.<), 
      (b) => {
        generate(s.f.body, b)
        b += OpenCLAST.Assignment(OpenCLAST.Expression(compFuncResVar), compResRef)
        generateConditional(b, 
          // if the result of the comparator is greater than zero, the element we're currently
          // comparing to is smaller than the element we are searching for
          Predicate(compFuncResVar, 0, Predicate.Operator.>),
          (cb) => {
            // therefore, keep searching!
            cb += OpenCLAST.Assignment(OpenCLAST.Expression(s.indexVar),OpenCLAST.Expression(s.indexVar + 1))
          },
          (cb) => {
            // else...
            generateConditional(cb, 
              // if the result is less than 0, we've gone past the value we're looking for, so abort
              Predicate(compFuncResVar, 0, Predicate.Operator.<),
              // if the value is greater than, it's gone past! the search has failed.
              (ccb) => {ccb += OpenCLAST.GOTO("searchFailed")},
              // otherwise, it must be equal to, so jump to returning the result
              (ccb) => {ccb += OpenCLAST.GOTO("writeresult")}
            )
          }
        )
      }
    )
    block += OpenCLAST.Label("searchFailed")
    block += generateStoreNode(block, OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.view.access(Cst(0)),
      generateLoadNode(defaultVal.mem.variable, OpenCLMemory.asOpenCLMemory(defaultVal.mem).addressSpace, defaultVal.t, defaultVal.view))
    block += OpenCLAST.GOTO("done")
    block += OpenCLAST.Label("writeresult")
    block += generateStoreNode(block, 
      OpenCLMemory.asOpenCLMemory(call.mem), call.t, call.view.access(Cst(0)),
      inArrRef)
    block += OpenCLAST.Label("done")
    block += OpenCLAST.OpenCLCode("\n")
  }




  private def generateValue(v: Value, block: Block): Unit = {
    val temp = Var("")

    block += OpenCLAST.VarDecl(temp.toString, Type.getValueType(v.t),
      init = OpenCLAST.OpenCLCode(v.value))
    block += OpenCLAST.Assignment(
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
                               OpenCLAST.VarRef(inputMem.variable),
                               outputMem.addressSpace)

    val range = i.indexVar.range.asInstanceOf[RangeAdd]

    // ADDRSPC TYPE tin = (odd ? out : swap);
    block += OpenCLAST.VarDecl(openCLCodeGen.toString(tout), Type.devectorize(call.t),
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

      val curOutLenRef = OpenCLAST.VarRef(curOutLen)

      val innerOutputLength = Type.getLength(funCall.t)

      // tmp = tmp * outputLen / inputLen
      b += OpenCLAST.Assignment(curOutLenRef,
        OpenCLAST.Expression(innerOutputLength))


      val tinVStrRef = OpenCLAST.VarRef(tin)

      // tin = (tout == swap) ? swap : out
      b += OpenCLAST.Assignment(tinVStrRef,
        OpenCLAST.Expression((tout eq swapMem.variable) ??
                             swapMem.variable !!  outputMem.variable))


      val toutVStrRef = OpenCLAST.VarRef(tout)

      // tout = (tout == swap) ? out : swap
      b += OpenCLAST.Assignment(toutVStrRef,
        OpenCLAST.Expression((tout eq swapMem.variable) ??
                             outputMem.variable !! swapMem.variable))

      if(outputMem.addressSpace != PrivateMemory)
        b += OpenCLAST.Barrier(outputMem)

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
      // cleanup
      replacements = replacements - indexVar
      replacementsWithFuns = replacementsWithFuns - indexVar

      block += OpenCLAST.Comment("end unroll")
    } else /* the loop is not unrolled */ {
      println("Not unrolling")
      // Generate an for-loop
      val innerBlock = OpenCLAST.Block(Vector.empty)
      // add it to the current node:
      block += OpenCLAST.Loop(indexVar, iterationCount, body = innerBlock)
      generate(innerBlock)
    }
  }

  private def generateWhileLoop(block: Block,
                                loopPredicate: Predicate,
                                generate: (Block) => Unit) : Unit = {
    val innerBlock = OpenCLAST.Block(Vector.empty)
    block += OpenCLAST.WhileLoop(loopPredicate, body = innerBlock)
    generate(innerBlock)
  }

  private def generateConditional(block: Block,
                                  switchPredicate: Predicate,
                                  genTrueBranch: (Block) => Unit,
                                  genFalseBranch: (Block) => Unit ) : Unit = {
    val trueBlock = OpenCLAST.Block(Vector.empty)
    val falseBlock = OpenCLAST.Block(Vector.empty)
    block += OpenCLAST.Conditional(switchPredicate, trueBody = trueBlock, falseBody = falseBlock)
    genTrueBranch(trueBlock)
    genFalseBranch(falseBlock)
  }
  private def generateUserFunCall(u: UserFun,
                                  call: FunCall,
                                  block: Block): Block = {
    // Handle vector assignments for vector types
    val mem = OpenCLMemory.asOpenCLMemory(call.mem)
    block += generateStoreNode(block, mem, call.t, call.view,
      generateFunCall(call, generateLoadNodes(block, call.args: _*)))

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
  private def generateStoreNode(block: Block,
                                mem: OpenCLMemory,
                                currentType: Type,
                                view: View,
                                value: OclAstNode): OclAstNode = {
    val originalType = varDecls(mem.variable)
    if (Type.haveSameValueTypes(originalType, currentType)) {
      OpenCLAST.Assignment(
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
              offset = OpenCLAST.Expression(
                          ArithExpr.substitute(ViewPrinter.emit(view),
                                               replacementsWithFuns) / vt.len))
      }
    }
  }

  private def generateLoadNodes(block: Block, args: Expr*): List[OclAstNode] = {
    args.map(generateLoadNode(block, _)).toList
  }

  private def generateLoadNode(block: Block, arg: Expr): OclAstNode = {
    val mem = OpenCLMemory.asOpenCLMemory(arg.mem)
    generateLoadNode(mem.variable, mem.addressSpace, arg.t, arg.view)
  }

  private def generateLoadNode(v: Var,
                               addressSpace: OpenCLAddressSpace,
                               currentType: Type,
                               view: View): OpenCLAST.OclAstNode = {
    val originalType = varDecls(v)
    if (Type.haveSameValueTypes(originalType, currentType)) {
      accessNode(v, addressSpace, view)
    } else {
      (originalType, currentType) match {
        // originally a scalar type, but now a vector type
        //  => emit cast
        case (st:ScalarType, vt:VectorType)  if Type.isEqual(st, vt.scalarT) =>
          OpenCLAST.Cast(OpenCLAST.VarRef(v), st)

        // originally an array of scalar values in global memory,
        // but now a vector type
        //  => emit vload
        case (at: ArrayType, vt: VectorType)
          if Type.isEqual(Type.getValueType(at), vt.scalarT)
          && ( addressSpace == GlobalMemory || addressSpace == LocalMemory ) =>

            OpenCLAST.Load(
              OpenCLAST.VarRef(v), vt,
              offset = OpenCLAST.Expression(
                          ArithExpr.substitute(ViewPrinter.emit(view),
                                               replacementsWithFuns) / vt.len))

        // originally an array of vector values in private memory,
        // but now a scalar type
        //  => emit load from components
        case (at: ArrayType, st: ScalarType)
          if Type.getValueType(at).isInstanceOf[VectorType]
          && Type.haveSameBaseTypes(at, st)
          && (addressSpace == PrivateMemory) =>

          // val n = Type.getValueType(at).asInstanceOf[VectorType].len
          val arraySuffix = arrayAccessPrivateMem(v, view)
          val componentSuffix = componentAccessVectorVar(v, view)
          OpenCLAST.VarRef(v, suffix = arraySuffix + componentSuffix)
      }
    }
  }

  /**
   * Create an access node(i.e. of type VarRef) for variable v based on the
   * given address space and view
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
        val index = ArithExpr.substitute(ViewPrinter.emit(view),
                                         replacementsWithFuns)
        OpenCLAST.VarRef(v, arrayIndex = OpenCLAST.Expression(index))

      case PrivateMemory =>
        OpenCLAST.VarRef(v, suffix = arrayAccessPrivateMem(v, view))
    }
  }

  /**
   * Generating the suffix appended to emulate an array access in private memory
   * @param v The varaible to access
   * @param view The view describing the access
   * @return A string of the form '_index' where index is the computed
   *         array index. The index must be computable at compile time.
   */
  private def arrayAccessPrivateMem(v: Var, view: View): String = {
    val i = {
      val originalType = varDecls(v)
      val valueType = Type.getValueType(originalType)
      valueType match {
        case _:ScalarType | _:TupleType => ViewPrinter.emit(view)
        // if the original value type is a vector:
        //   divide index by vector length
        case _:VectorType =>
          val length = Type.getLength(Type.getValueType(originalType))
          ViewPrinter.emit(view) / length
      }
    }
    // Compute the index ...
    val index = ArithExpr.substitute(i,replacements).eval
    // ... and append it
    "_" + openCLCodeGen.toString(index)
  }

  /**
   * Create a string representing for a component access into a vector variable
   * @param v The variable to access. Must have been declared with a vector type
   * @param view The view to access this variable
   * @return OpenCL code for accessing v, e.g.: v.s0
   */
  private def componentAccessVectorVar(v: Var, view: View): String = {
    val i = {
      val originalType = varDecls(v)
      val valueType = Type.getValueType(originalType)
      valueType match {
        case _:VectorType =>
          val length = Type.getLength(Type.getValueType(originalType))
          ViewPrinter.emit(view) % length
      }
    }
    // Compute the index ...
    val index = ArithExpr.substitute(i,replacements).eval
    // ... and append it
    ".s" + openCLCodeGen.toString(index)
  }

  /**
   * An access to a variable as a value, i.e. a direct access by name.
   * @param v The variable to access
   * @return A VarRef node wrapping `v`
   */
  private def valueAccessNode(v: Var): OpenCLAST.VarRef = {
    OpenCLAST.VarRef(v)
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
