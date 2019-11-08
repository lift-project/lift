package backends.spatial.accel.generator

import backends.spatial.accel.ir.ast.SpatialAccelAST
import backends.spatial.accel.ir.ast.SpatialAccelAST._
import backends.spatial.accel.ir.pattern.{AbstractSpFold, MapSeq, SpFold, SpForeach, SpMemFold, SpPipeFold, SpPipeMemFold, SpSeqFold, SpSeqMemFold, toArgOut, toDRAM, toReg, toSRAM}
import backends.spatial.common.{Printer, SpatialAST}
import backends.spatial.common.SpatialAST.{ExprBasedFunction, SpIfThenElse, SpParamDecl, SpatialCode}
import backends.spatial.common.generator.SpatialArithmeticMethod
import backends.spatial.common.ir.ast.SpatialBuiltInFun
import backends.spatial.common.ir.view.{ArrayAddressor, Index, Slice, SpatialViewPrinter}
import backends.spatial.common.ir.{AddressSpaceCollection, ArgOutMemory, ContextualMemoryCollection, DRAMMemory, HostAllocatedMemory, LiteralMemory, RegMemory, SRAMMemory, SpatialAddressSpace, SpatialMemory, SpatialMemoryCollection, SpatialNullMemory, UndefAddressSpace}
import core.generator.GenericAST._
import ir._
import ir.ast.{AbstractMap, Array2DFromUserFunGenerator, Array3DFromUserFunGenerator, ArrayAccess, ArrayFromUserFunGenerator, Concat, Expr, FPattern, Filter, FunCall, Gather, Get, Head, Join, Lambda, Map, Pad, PadConstant, Param, RewritingGuidePost, Scatter, Slide, Split, Tail, Transpose, TransposeW, Tuple, Unzip, UserFun, Value, VectorizeUserFun, Zip, asScalar, asVector, debug}
import ir.view.{View, ViewConstant}
import lift.arithmetic._
import opencl.generator.PerformLoopOptimisation

import scala.collection.{immutable, mutable}

object AccelGenerator {

  def apply(f: Lambda, allTypedMemories: ContextualMemoryCollection): ExprBlock =
    (new SpatialGenerator(allTypedMemories)).generate(f)
}

class SpatialGenerator(allTypedMemories: ContextualMemoryCollection) {

  type ValueTable = immutable.Map[ArithExpr, ArithExpr]
  type SymbolTable = immutable.Map[Var, Type]

  private var replacementsOfIteratorsWithValues: ValueTable = immutable.Map.empty
  private var replacementsOfIteratorsWithValuesWithFuns: ValueTable = immutable.Map.empty

  private val scope: mutable.Stack[FunCall] = mutable.Stack()

  private var userFunSignatures: Set[UserFunSignature] = Set()

  def generate(f: Lambda): ExprBlock = {
    // Initialise the block
    val accelBlock = MutableExprBlock(Vector.empty)

    // Find and generate user function signatures
    userFunSignatures = generateUserFunctions(f.body)

    userFunSignatures.foreach(accelBlock += _.createFunctionDeclaration)

    // Generate the main part of the block
    generate(f.body, accelBlock)

    // TODO: Perform common subexpression elimination

    // Return the result
    accelBlock.toExprBlock
  }

  /**
   * Generates current expression inside block.
   *
   * @param expr The expression to generate
   * @param block The AST block to write into
   * @param returnValue Determines whether a return of memory written to by the current node is
   *                  required at the end of generated code
   *                  TODO: replace this "manual" flag with inferring the decision based on the future
   *                        trait marking Lift-Spatial nodes as potential expressions that do not require
   *                        memory return
   */
  private def generate(expr: Expr, block: MutableExprBlock, returnValue: Boolean = false): Unit = {
    var returnRequired = returnValue
    assert(expr.t != UndefType)

    // Generate arguments
    expr match {
      case f: FunCall => f.args.foreach(generate(_, block))
      case _          =>
    }

    expr match {
      case call: FunCall =>

        scope.push(call)

        declareMemoryIfRequired(expr, block)

        call.f match {
          case _: AbstractMap | _: SpForeach =>
            call.f match {
              case _: Map             =>
              case m: MapSeq          => generateMapSeqCall(m, call, block)
              case sf: SpForeach      => generateForeachCall(sf, call, block)
              case _                  => throw new NotImplementedError()
            }

            // If the size of the input array is not known in the type, it is not
            // in the type of the output array either. Therefore, we have to copy
            // it from the input's header to the output's header.
            propagateDynamicArraySize(call, block)

          case sf: SpFold             => generateFoldCall(sf, call, block)
          case smf: SpMemFold         => generateFoldCall(smf, call, block)

          case u: UserFun             => generateUserFunCall(u, call, block)

          case fp: FPattern           => generate(fp.f.body, block)
          case l: Lambda              => generate(l.body, block)

          case toReg(_) | toArgOut(_) | toSRAM(_) | toDRAM(_) |
               Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
               Split(_) | Join() | Slide(_, _) | Zip(_) | Concat(_) | Tuple(_) | Filter() |
               Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Pad(_, _, _) | PadConstant(_, _, _) |
               ArrayAccess(_) | debug.PrintType(_) | debug.PrintTypeInConsole(_) | debug.AssertType(_, _) |
               RewritingGuidePost(_)  =>

          case _                      => throw new NotImplementedError()
        }

        scope.pop()

      case v: Value             => generateValue(v, block)
      case _: Param             =>
      case _  => throw new NotImplementedError()
    }

    if (returnRequired)
      (block: MutableExprBlock) += generateLoadNode(SpatialMemory.asSpatialMemory(expr.mem), expr.t, expr.outputView)
  }

  private def propagateDynamicArraySize(call: FunCall, block: MutableExprBlock): Unit = {
    call.t match {
      case ArrayTypeWS(_, _) =>
      case _                 =>
        val inSizeView = call.args.head.view.size()
        // the output is accessed the same way as the input but the variable name has to be replaced
        val outSizeView = inSizeView.replaced(call.args.head.mem.variable, call.mem.variable)
        val accessType = Type.getBaseType(call.t)

        (block: MutableExprBlock) += AssignmentExpression(
          to = accessNode(call.mem, call.addressSpace, accessType, outSizeView),
          value = getArraySize(SpatialMemory.asSpatialMemory(call.args.head.mem), inSizeView)
        )
    }
  }

  /**
   * Generate an access to the size of an array (handle arrays of tuples)
   */
  private def getArraySize(mem: SpatialMemory, view: View): ExpressionT = {
    mem match {
      case SpatialMemoryCollection(subMemories, _) =>
        val sizes = subMemories.zipWithIndex.map(p =>
          getArraySize(p._1, view.get(p._2))
        )
        sizes.reduce((x, y) => FunctionCall("min", List(x, y)))
      case _                                      =>
        SpatialViewPrinter.emit(view, addressSpace = mem.addressSpace)
    }
  }

  private def generateStatement(block: MutableExprBlock,
                                indexVar: Var,
                                generateBody: (MutableExprBlock) => Unit, init: ArithExpression): Unit = {
    // one iteration
    (block: MutableExprBlock) += Comment("iteration count is exactly 1, no loop emitted")
    val innerBlock = MutableExprBlock(Vector.empty, encapsulated = false)

    innerBlock += SpatialVarDecl(v = indexVar, t = backends.spatial.common.ir.Int,
      init = Some(init), addressSpace = RegMemory)
    generateBody(innerBlock)

    (block: MutableExprBlock) += innerBlock
  }

  private def generateIfStatement(block: MutableExprBlock, indexVar: Var, generateBody: (MutableExprBlock) => Unit,
                                  init: ArithExpression, stop: ArithExpr): Unit = {
    (block: MutableExprBlock) += Comment("iteration count is exactly 1 or less, no loop emitted")
    val innerBlock = MutableExprBlock(Vector.empty, encapsulated = false)
    innerBlock += SpatialVarDecl(v = indexVar, t = backends.spatial.common.ir.Int,
      init = Some(init), addressSpace = RegMemory)
    (block: MutableExprBlock) += SpIfThenElse(
      BinaryExpression(init, BinaryExpressionT.Operator.<, ArithExpression(stop)),
      trueBody = innerBlock, falseBody = MutableExprBlock())
    generateBody(innerBlock)
  }

  /**
   * TODO: update description
   * Declares memory all conditions are met:
   * 1. The memory needs materialising
   * 1.1. Memories not in the collection allTypedMemories do not need materialisation
   * 1.2. Memories in allTypedMemories with .toBeMaterialised == false do not need materialisation --
   *      they include body memories of FPatterns that just return the values
   * 2. The memory hasn't been declared yet
   * 3. The memory is not in DRAM address space -- those memories will be allocated by host
   */
  private def declareMemoryIfRequired(expr: Expr,
                                      block: MutableExprBlock,
                                      init: Option[AstNode] = None): Unit = {
    val mem = expr.mem.asInstanceOf[SpatialMemory]
    val memoryNeedsMaterialising =
      allTypedMemories.contains(mem) &&
        mem.addressSpace != LiteralMemory

    if (memoryNeedsMaterialising &&
      !allTypedMemories(mem).declared &&
      !mem.addressSpace.isInstanceOf[HostAllocatedMemory]) {

      (block: MutableExprBlock) += SpatialVarDecl(mem.variable, mem.t, init, mem.addressSpace, mem.bufferHazard)

      allTypedMemories(mem).declared = true
    }
  }

  private def generateValue(v: Value, block: MutableExprBlock): Unit = {
    declareMemoryIfRequired(v, block, init = Some(SpatialCode(v.value)))
  }

  private def generateMapSeqCall(m: MapSeq,
                                 call: FunCall,
                                 block: MutableExprBlock): Unit = {
    (block: MutableExprBlock) += Comment("map_seq")
    generateForeach(m, block, call.args.head, m.loopVar, generate(m.f.body, _), m.shouldUnroll)
    (block: MutableExprBlock) += Comment("end map_seq")
  }
  
  private def generateForeachCall(sf: SpForeach,
                                  call: FunCall,
                                  block: MutableExprBlock): Unit = {
    (block: MutableExprBlock) += Comment("SpForeach")
    generateForeach(sf, block, call.args.head, sf.loopVar, generate(sf.f.body, _), sf.shouldUnroll)
    (block: MutableExprBlock) += Comment("end SpForeach")
  }

  private def generateFoldCall(asf: AbstractSpFold,
                               call: FunCall,
                               block: MutableExprBlock): Unit = {
    // TODO: add unrolled Fold generation
    // TODO: optimise loop generation. See OpenCLGenerator.generateOptimizedForLoopRepresentations

    val innerMapBlock = MutableExprBlock(Vector.empty, encapsulated = false)
    val innerReduceBlock = MutableExprBlock(Vector.empty, encapsulated = false)

    val accumOrInit = call.args.head

    val accumOrInitNode = valueAccessNode(accumOrInit.mem)

    val counter = List(Counter(
      ArithExpression(getRangeAdd(asf.mapLoopVar).start),
      ArithExpression(getRangeAdd(asf.mapLoopVar).stop),
      ArithExpression(getRangeAdd(asf.mapLoopVar).step),
      asf match {
        case _: Piped => Some(ArithExpression(asf.factor))
        case _ => None
      }))
    val iterVars = List(CVar(asf.mapLoopVar))

    (block: MutableExprBlock) += (asf match {
      case _: SpSeqFold     => SpatialAccelAST.Fold(SequentialSchedule(), accumOrInitNode, counter, iterVars,
                                                    innerMapBlock, innerReduceBlock)
      case _: SpPipeFold    => SpatialAccelAST.Fold(PipeSchedule(), accumOrInitNode, counter, iterVars,
                                                    innerMapBlock, innerReduceBlock)
      case _: SpSeqMemFold  => SpatialAccelAST.MemFold(SequentialSchedule(), accumOrInitNode, counter, iterVars,
                                                       innerMapBlock, innerReduceBlock)
      case _: SpPipeMemFold => SpatialAccelAST.MemFold(PipeSchedule(), accumOrInitNode, counter, iterVars,
                                                       innerMapBlock, innerReduceBlock)
      case _                => throw new AccelGeneratorException(s"Unknown flavour of Fold: $asf")
    })

    generate(asf.fMap.body, innerMapBlock, returnValue = true)
    generate(asf.fReduce.body, innerReduceBlock)
  }

  private def generateForeach(pattern: Pattern,
                              block: MutableExprBlock,
                              array: Expr,
                              indexVar: Var,
                              generateBody: (MutableExprBlock) => Unit,
                              needUnroll: Boolean = false): Unit = {
    // if we need to unroll (e.g. because of access to register memory)
    if (needUnroll) generateForeachUnrolled(block, indexVar, generateBody)
    else {
      if (PerformLoopOptimisation())
        generateOptimizedForeachRepresentations(pattern, block, array, indexVar, generateBody)
      else
        generateDefaultForeachRepresentation(pattern, block, array, indexVar, generateBody)
    }
  }

  private def generateForeachUnrolled(block: MutableExprBlock,
                                      indexVar: Var,
                                      generateBody: (MutableExprBlock) => Unit): Unit = {
    val range = getRangeAdd(indexVar)
    val iterationCount = getIterationCount(range)

    if (iterationCount > 0) {
      (block: MutableExprBlock) += Comment("unroll")

      for (i <- 0 until iterationCount) {
        replacementsOfIteratorsWithValues = replacementsOfIteratorsWithValues.updated(indexVar, i)
        val j: ArithExpr = range.min match {
          case _: SpatialArithmeticMethod => range.min + range.step * i
          case _                          => i
        }
        replacementsOfIteratorsWithValuesWithFuns = replacementsOfIteratorsWithValuesWithFuns.updated(indexVar, j)
        generateBody(block)
      }
      // cleanup
      replacementsOfIteratorsWithValues = replacementsOfIteratorsWithValues - indexVar
      replacementsOfIteratorsWithValuesWithFuns = replacementsOfIteratorsWithValuesWithFuns - indexVar

      (block: MutableExprBlock) += Comment("end unroll")
    } else
      throw new AccelGeneratorException(s"Trying to unroll loop, but iteration count is $iterationCount.")
  }

  private def generateOptimizedForeachRepresentations(pattern: Pattern,
                                                      block: MutableExprBlock,
                                                      argArray: Expr,
                                                      indexVar: Var,
                                                      generateBody: (MutableExprBlock) => Unit): Unit = {
    val range = getRangeAdd(indexVar)
    val init = ArithExpression(range.start)
    lazy val parFactor = pattern.asInstanceOf[Piped].factor

    argArray.t match {
      case _: ArrayType =>
        range.numVals match {
          case Cst(0) => (block: MutableExprBlock) += Comment("iteration count is 0, no loop emitted")

          case Cst(1) =>
            if (pattern.isInstanceOf[Piped]) (block: MutableExprBlock) += Comment(s"Ignored parallel factor $parFactor")

            generateStatement(block, indexVar, generateBody, init)

          // TODO: See TestInject.injectExactlyOneIterationVariable
          // TODO: M / 128 is not equal to M /^ 128 even though they print to the same C code
          case _ if range.start.min.min == Cst(0) &&
            ArithExpr.substituteDiv(range.stop) == ArithExpr.substituteDiv(range.step) =>
            if (pattern.isInstanceOf[Piped]) (block: MutableExprBlock) += Comment(s"Ignored parallel factor $parFactor")
            generateStatement(block, indexVar, generateBody, init)

          // TODO: See TestOclFunction.numValues and issue #62
          case _ if range.start.min.min == Cst(0) && range.stop == Cst(1) =>
            if (pattern.isInstanceOf[Piped]) (block: MutableExprBlock) += Comment(s"Ignored parallel factor $parFactor")
            generateIfStatement(block, indexVar, generateBody, init, range.stop)

          case _ if range.numVals.min == Cst(0) && range.numVals.max == Cst(1) =>
            if (pattern.isInstanceOf[Piped]) (block: MutableExprBlock) += Comment(s"Ignored parallel factor $parFactor")
            generateIfStatement(block, indexVar, generateBody, init, range.stop)

          case _ =>
            generateDefaultForeachRepresentation(pattern, block, argArray, indexVar, generateBody)
        }
      case _            => throw new NotImplementedError() // should never get there
    }
  }

  private def generateDefaultForeachRepresentation(pattern: Pattern,
                                                   block: MutableExprBlock,
                                                   argArray: Expr,
                                                   indexVar: Var,
                                                   generateBody: (MutableExprBlock) => Unit): Unit = {

    val innerBlock = MutableExprBlock(Vector.empty, encapsulated = false)

    (block: MutableExprBlock) += SpatialAccelAST.Foreach(
      counter = List(Counter(
        ArithExpression(getRangeAdd(indexVar).start),
        ArithExpression(getRangeAdd(indexVar).stop),
        ArithExpression(getRangeAdd(indexVar).step),
        pattern match {
          case pipedPattern: Piped => Some(ArithExpression(pipedPattern.factor))
          case _ => None
        })),
      iterVars = List(indexVar),
      body = innerBlock)

    generateBody(innerBlock)
  }

  private def generateUserFunCall(u: UserFun,
                                  call: FunCall,
                                  block: MutableExprBlock): MutableExprBlock = {
    val sMem = SpatialMemory.asSpatialMemory(call.mem)
    // Sanity check -- the memory being written to has to have been collected
    assert(allTypedMemories.contains(sMem))

    val funcall_node = generateFunCall(call, generateLoadNodes(call.args: _*))

    if (!allTypedMemories(sMem).inImplicitWriteScope(scope)) {

      // Generate store
      (block: MutableExprBlock) += generateStoreNode(
        targetMem = sMem, targetView = call.outputView, writeType = call.t,
        srcAddressSpace = call.addressSpace, srcNode = funcall_node)

    } else
      // Generate return
      (block: MutableExprBlock) += funcall_node

    block
  }

  @scala.annotation.tailrec
  private def generateFunCall(expr: Expr,
                              args: List[AstNode]): FunctionCall = {
    expr match {
      case call: FunCall => call.f match {
        case uf: UserFun          => FunctionCall(UserFunSignature.getUniqueName(uf, call, userFunSignatures), args)
        case l: Lambda            => generateFunCall(l.body, args)
        case _                    => throw new NotImplementedError()
      }
      case _ => throw new NotImplementedError()
    }
  }

  /** Traverses f and collects all user functions */
  private def generateUserFunctions(expr: Expr): Set[UserFunSignature] = {
    Expr.visitWithState(Set[UserFunSignature]())(expr, (expr, set) =>
      expr match {
        case call: FunCall                      => call.f match {
          case _: SpatialBuiltInFun  => set
          case uf: UserFun           =>
            val ufSignature = UserFunSignature(uf, call)
            if (!set.contains(ufSignature)) {
              val nonOverloadableFuns = set.filter(ufSignature.cannotBeOverloadedWith)
              if (nonOverloadableFuns.nonEmpty) {
                if (nonOverloadableFuns.size == 1)
                  nonOverloadableFuns.head.uniqueName += "_0"
                ufSignature.uniqueName += "_" + nonOverloadableFuns.size.toString
              }
            }

            set + ufSignature
          case vec: VectorizeUserFun => throw new NotImplementedError()
          case _                     => set
        }
        case ArrayFromUserFunGenerator(uf, _)   => throw new NotImplementedError()
        case Array2DFromUserFunGenerator(uf, _) => throw new NotImplementedError()
        case Array3DFromUserFunGenerator(uf, _) => throw new NotImplementedError()
        case _                                  => set
      })
  }

  /**
   * Generate a simple or vector store
   */
  private def generateStoreNode(targetMem: SpatialMemory,
                                targetView: View,
                                writeType: Type,
                                srcAddressSpace: SpatialAddressSpace,
                                srcNode: AstNode): StatementT = {
    // The val is intentionally lazy to avoid computing an addressor in cases where the targetMem is accessed
    // through a reference to its variable might be impossible
    lazy val targetNode = accessNode(targetMem, targetMem.addressSpace, writeType, targetView)

    if (srcAddressSpace == targetMem.addressSpace) targetMem.addressSpace match {
      case RegMemory    => RegAssignmentExpression(to = targetNode, srcNode)
      case ArgOutMemory => throw new AccelGeneratorException(
                              "Cannot generate a store from ArgOut to ArgOut because ArgOut is write-only")
      case _            => AssignmentExpression(to = targetNode, srcNode)
    }

    else (srcAddressSpace, targetMem.addressSpace) match {
      case (DRAMMemory, SRAMMemory)     => SpLoad(src = srcNode, target = VarSlicedRef(targetMem.variable))
      case (SRAMMemory, DRAMMemory)     => SpStore(src = srcNode, target = targetNode)
      case (RegMemory, DRAMMemory)      => AssignmentExpression(to = targetNode, srcNode)
      case (RegMemory, SRAMMemory)      => AssignmentExpression(to = targetNode, srcNode)
      case (RegMemory, ArgOutMemory)    => RegAssignmentExpression(to = targetNode, srcNode)
      case (LiteralMemory, RegMemory)   => RegAssignmentExpression(to = targetNode, srcNode)

      case _ => throw new AccelGeneratorException(
        s"Don't know how to store a value from $srcAddressSpace in ${targetMem.addressSpace}")
    }
  }

  private def generateLoadNodes(args: Expr*): List[AstNode] = {
    args.map(arg => {
      val mem = SpatialMemory.asSpatialMemory(arg.mem)
      generateLoadNode(mem, arg.t, arg.view)
    }).toList
  }

  private def generateLoadNode(mem: SpatialMemory, t: Type, view: View): ExpressionT = {
    if (allTypedMemories(mem).inImplicitReadScope(scope))
      // This memory is is implicitly read from. If we are trying to load it, that means we are
      // printing an anonymous function definition (e.g. "{ add(_, _) }"), so we'll print placeholders
      SpatialAST.Placeholder()
    else mem match {
      case coll: SpatialMemoryCollection =>
        // we want to generate a load for a tuple constructed by a corresponding view (i.e. zip)
        if (!t.isInstanceOf[TupleType])
          throw new AccelGeneratorException(s"Found a SpatialMemoryCollection for var: " +
            s"${mem.variable}, but corresponding type: $t is " +
            s"not a tuple.")
        val tt = t.asInstanceOf[TupleType]

        var args: Vector[AstNode] = Vector()
        for (i <- (coll.subMemories zip tt.elemsT).indices) {
          args = args :+ generateLoadNode(coll.subMemories(i), tt.elemsT(i), view.get(i))
        }

        StructConstructor(t = tt, args = args) // TODO: check functional correctness for Spatial

      // A SpatialNullMemory object indicates that the view is not backed by memory and will directly return a value
      case SpatialNullMemory => SpatialViewPrinter.emit(view, replacementsOfIteratorsWithValuesWithFuns)

      case sMem: SpatialMemory if sMem.addressSpace == ArgOutMemory =>
        throw new IllegalArgumentException(s"Cannot read from the write-only ArgOut memory $sMem")

      // not a memory collection: the default case
      case _ => accessNode(mem, mem.addressSpace, t, view)
    }
  }

  private def getRangeAdd(indexVar: Var) = {
    indexVar.range match {
      case r: RangeAdd => r
      case _           =>
        throw new AccelGeneratorException("Cannot handle range for the loop: " + indexVar.range)
    }
  }

  private def getIterationCount(range: RangeAdd): Int = {
    try {
      range.numVals.enforceSimplification.eval
    } catch {
      case NotEvaluableException()      =>
        throw new AccelGeneratorException("Trying to unroll loop, but iteration count could " +
          "not be determined statically.")
      case NotEvaluableToIntException() =>
        throw new AccelGeneratorException("Trying to unroll loop, " +
          "but iteration count is larger than scala.Int.MaxValue.")
    }
  }


  /**
   * Create an access node (i.e. of type NDVarSlicedRef) for mem based on the
   * given address space and view
   *
   * @param mem          The memory to access
   * @param addressSpace The address space, i.e. DRAM, SRAM, Reg
   * @param view         The view to access var `v`
   * @return An NDVarSlicedRef node accessing `v` as described in `view`.
   */
  private def accessNode(mem: Memory,
                         addressSpace: SpatialAddressSpace,
                         accessType: Type,
                         view: View): ExpressionT = {
    addressSpace match {
      case SRAMMemory | DRAMMemory | RegMemory | ArgOutMemory =>
        // allTypedMemories(v).mem.t is the original type, i.e. the type of the data stored in allTypedMemories(v).mem
        // allTypedMemories(v).writeT is the write type of the memory producer (UserFun / Value)
        allTypedMemories(mem).mem.t match {
          case _: ArrayType                 => arrayAccessNode(mem, addressSpace, view)
          case _: ScalarType | _: TupleType => valueAccessNode(mem)
          case _                            =>
            throw new TypeException(allTypedMemories(mem).mem.t, "A known type", null)
        }

      case LiteralMemory =>
        view match {
          case ViewConstant(value, t) =>
            // Make sure the value is well-formed
            try { t match {
              case backends.spatial.common.ir.Int => value.value.toInt
              case backends.spatial.common.ir.Float => value.value.toFloat
              case _ => throw new AccelGeneratorException(s"Unknown value type $t")
            }} catch {
              case _: java.lang.NumberFormatException =>
                throw new AccelGeneratorException("Cannot convert \"" + value + "\" to a number")
            }
            // Return the value as a string
            SpatialCode(value.value)

          case _ =>
            throw new IllegalArgumentException(s"Expected ViewConstant as a view of the literal memory $mem. Got $view")
        }

      case UndefAddressSpace | AddressSpaceCollection(_) =>
        throw new IllegalArgumentException(s"Cannot access data in $addressSpace")
    }
  }


  /**
   * Accessing mem as an array
   *
   * @param mem          The memory to access
   * @param addressSpace The address space `v` lives in
   * @param view         The view describing the access
   * @return An VarRef node accessing `v` as described in `view`.
   */
  private def arrayAccessNode(mem: Memory,
                              addressSpace: SpatialAddressSpace,
                              view: View): ExpressionT = {
    addressSpace match {
      case SRAMMemory | DRAMMemory =>
        SpatialViewPrinter.emit(view, replacementsOfIteratorsWithValuesWithFuns, addressSpace)

      case RegMemory | ArgOutMemory =>
        val arrayAccessExpression = SpatialViewPrinter.emit(view, replacementsOfIteratorsWithValuesWithFuns, addressSpace)
        arrayAccessExpression match {
          case _: VarIdxRef | _: VarSlicedRef =>
            VarIdxRef(mem.variable, suffix = Some(arrayAccessRegisterMem(mem, view)))
          case e: ExpressionT  => e
        }

      case UndefAddressSpace | AddressSpaceCollection(_) =>
        throw new IllegalArgumentException(s"Cannot load data from $addressSpace")
    }
  }

  /**
   * Generates the string suffix appended to the register variable name for access to
   * emulated array in register memory.
   * emulated_reg_array(5, 1::3, 4) => "_5__1_2__4"
   *
   * @param mem The memory to access
   * @param view The view describing the access
   * @return A string of the form '_indices' where indices is the sliced multidimensional
   *         array index. The indices must be computable at compile time.
   */
  private def arrayAccessRegisterMem(mem: Memory, view: View): String = {
    // Compute the indices ...
    val indices = arrayAccessRegisterMemIndex(mem, view)
    // ... and append it
    "_" + indices.map(_.map(Printer.toString(_)).mkString("_")).mkString("__")
  }

  /**
   * Generates integers characterising one access to memory.
   * Produces a two-dimensional list of indices. The outer dimension contains
   * one element per dimension of the accessed memory; the inner dimension contains
   * one element per index in a slice.
   * For example:
   * arr(5, 2::3::11, 7, 1::3) => [[5], [2, 5, 8], [7], [1, 2]]
   *
   * @param mem The memory to access
   * @param view Access view
   * @return A two-dimensional list of indices
   */
  private def arrayAccessRegisterMemIndex(mem: Memory, view: View): List[List[Int]] = {

    val typeInMem = allTypedMemories(mem).typeInMem

    // Get the addressors as if we were accessing a multidimensional array in memory and
    // convert them from Spatial AST to Lift IR
    val addressors: List[ArrayAddressor] = typeInMem match {

      case _: ScalarType | _: TupleType =>
        val arrayAccessExpression = SpatialViewPrinter.emit(view, replacementsOfIteratorsWithValues, RegMemory)
        arrayAccessExpression match {
          case VarSlicedRef(_, _, Some(addr)) => addr.map {
            case idx: ArrIndex => Index(idx)
            case slice: ArrSlice => Slice(slice)
          }

          case x => throw new MatchError(s"Expected a NDVarSlicedRef with addressors, but got $x.")
        }

      case _: VectorType => throw new NotImplementedError()

      case ArrayType(_) =>

        SpatialViewPrinter.emit(view, replacementsOfIteratorsWithValues, RegMemory) match {
          case VarSlicedRef(_, _, Some(addr)) => addr.map {
            case idx: ArrIndex => Index(idx)
            case slice: ArrSlice => Slice(slice)
          }
          case x => throw new MatchError(s"Expected a NDVarSlicedRef, but got $x.")
        }

      case NoType | UndefType =>
        throw new TypeException(typeInMem, "A valid defined type", null)
    }

    // Replace where necessary and evaluate to Int
    val addrWithReplacements: List[List[Int]] = try {
      addressors.map(addr => {
        val sbstAddr = addr.visitAndRebuild(ArithExpr.substitute(_, replacementsOfIteratorsWithValues))

        sbstAddr.eval()
      })
    } catch {
      case NotEvaluableException() =>
        throw new AccelGeneratorException(s"Could not access register array, as addressor $addressors could " +
          s"not be evaluated statically (given these replacementsOfIteratorsWithValues: $replacementsOfIteratorsWithValues)")
      case NotEvaluableToIntException() =>
        throw new AccelGeneratorException(s"Could not access register array, as addressor $addressors is " +
          s"larger than scala.Int.MaxValue (given these replacementsOfIteratorsWithValues: $replacementsOfIteratorsWithValues)")
    }

    val memTypeLengths = Type.getLengths(allTypedMemories(mem).typeInMem)

    if (!memTypeLengths.forall(_.isEvaluable))
      throw new AccelGeneratorException(s"Register memory length has to be evaluable, but found $memTypeLengths")

    if (!addrWithReplacements.zip(memTypeLengths).forall{ case (addr, memLen) => addr.max < memLen.eval })
      throw new AccelGeneratorException(s"Out of bounds access to $mem with $addrWithReplacements")

    addrWithReplacements
  }


  /**
   * An access to a variable as a value, i.e. a direct access by name.
   *
   * @param mem The memory to access
   * @return A VarRef node wrapping `v`
   */
  private def valueAccessNode(mem: Memory): VarSlicedRef = {
    VarSlicedRef(mem.variable)
  }
}