package backends.spatial.accel.generator

import backends.spatial.accel.ir.ast.SpatialAccelAST
import backends.spatial.accel.ir.ast.SpatialAccelAST._
import backends.spatial.accel.ir.pattern.{AbstractSpFold, MapSeq, SpFold, SpForeach, SpMemFold, toDRAM, toReg, toSRAM}
import backends.spatial.common.{Printer, SpatialAST}
import backends.spatial.common.SpatialAST.{ExprBasedFunction, SpIfThenElse, SpParamDecl, SpatialCode}
import backends.spatial.common.generator.SpatialArithmeticMethod
import backends.spatial.common.ir.ast.SpatialBuiltInFun
import backends.spatial.common.ir.view.{ArrayAddressor, Index, Slice, SpatialViewPrinter}
import backends.spatial.common.ir.{AddressSpaceCollection, DRAMMemory, RegMemory, SRAMMemory, SpatialAddressSpace, SpatialMemory, SpatialMemoryCollection, SpatialNullMemory, TypedMemoryCollection, UndefAddressSpace}
import core.generator.GenericAST._
import ir._
import ir.ast.{AbstractMap, Array2DFromUserFunGenerator, Array3DFromUserFunGenerator, ArrayAccess, ArrayFromUserFunGenerator, Concat, Expr, FPattern, Filter, FunCall, Gather, Get, Head, Join, Lambda, Map, Pad, PadConstant, Param, RewritingGuidePost, Scatter, Slide, Split, Tail, Transpose, TransposeW, Tuple, Unzip, UserFun, Value, VectorizeUserFun, Zip, asScalar, asVector, debug}
import ir.view.View
import lift.arithmetic._
import opencl.generator.PerformLoopOptimisation

import scala.collection.immutable

object AccelGenerator {

  def apply(f: Lambda, allTypedMemories: TypedMemoryCollection): ExprBlock =
    (new SpatialGenerator(allTypedMemories)).generate(f)

  def createFunctionDefinition(uf: UserFun,
                               argAddressSpaces: Seq[AddressSpace],
                               retAddressSpace: AddressSpace): ExprBasedFunction = {
    val block = MutableExprBlock()
    if (uf.tupleTypes.length == 1)
      throw new NotImplementedError()
    else uf.tupleTypes.zipWithIndex.foreach({ case (x, i) =>
      throw new NotImplementedError()
    })

    block += SpatialCode(uf.body)
    ExprBasedFunction(
      name = uf.name,
      ret = uf.outT,
      addressSpace = retAddressSpace,
      params = (uf.inTs, uf.paramNames, argAddressSpaces).
        zipped.map((t, n, as) => SpParamDecl(n, t, as)).toList,
      body = block)
  }
}

class SpatialGenerator(allTypedMemories: TypedMemoryCollection) {

  type ValueTable = immutable.Map[ArithExpr, ArithExpr]
  type SymbolTable = immutable.Map[Var, Type]

  private var replacementsOfIteratorsWithValues: ValueTable = immutable.Map.empty
  private var replacementsOfIteratorsWithValuesWithFuns: ValueTable = immutable.Map.empty

  def generate(f: Lambda): ExprBlock = {
    // Initialise the block
    val accelBlock = MutableExprBlock(Vector.empty)

    // Find and generate user functions
    generateUserFunctions(f.body).foreach(accelBlock += _)

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
   * @param memReturn Determines whether a return of memory written to by the current node is
   *                  required at the end of generated code
   *                  TODO: replace this "manual" flag with inferring the decision based on the future
   *                        trait marking Lift-Spatial nodes as potential expressions that do not require
   *                        memory return
   */
  private def generate(expr: Expr, block: MutableExprBlock, memReturn: Boolean = false): Unit = {
    var returnRequired = memReturn
    assert(expr.t != UndefType)

    // Generate arguments
    expr match {
      case f: FunCall => f.args.foreach(generate(_, block))
      case _          =>
    }

    expr match {
      case call: FunCall =>

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

          case sf: SpFold             => generateFoldCall(sf, call, block); returnRequired = false
          case smf: SpMemFold         => generateFoldCall(smf, call, block); returnRequired = false

          case u: UserFun             => generateUserFunCall(u, call, block)

          case fp: FPattern           => generate(fp.f.body, block)
          case l: Lambda              => generate(l.body, block)

          case toReg(_) | toSRAM(_) | toDRAM(_) |
               Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
               Split(_) | Join() | Slide(_, _) | Zip(_) | Concat(_) | Tuple(_) | Filter() |
               Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Pad(_, _, _) | PadConstant(_, _, _) |
               ArrayAccess(_) | debug.PrintType(_) | debug.PrintTypeInConsole(_) | debug.AssertType(_, _) |
               RewritingGuidePost(_)  =>

          case _                      => throw new NotImplementedError()
        }

      case v: Value             => generateValue(v, block)
      case _: Param             =>
      case _  => throw new NotImplementedError()
    }

    if (returnRequired && allTypedMemories(expr.mem).materialised)
      (block: MutableExprBlock) += accessNode(expr.mem.variable, expr.addressSpace, expr.t, expr.view)
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
          to = accessNode(call.mem.variable, call.addressSpace, accessType, outSizeView),
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
    val innerBlock = MutableExprBlock(Vector.empty)

    innerBlock += SpatialVarDecl(v = indexVar, t = backends.spatial.common.ir.Int,
      init = Some(init), addressSpace = RegMemory)
    generateBody(innerBlock)

    (block: MutableExprBlock) += innerBlock
  }

  private def generateIfStatement(block: MutableExprBlock, indexVar: Var, generateBody: (MutableExprBlock) => Unit,
                                  init: ArithExpression, stop: ArithExpr): Unit = {
    (block: MutableExprBlock) += Comment("iteration count is exactly 1 or less, no loop emitted")
    val innerBlock = MutableExprBlock(Vector.empty)
    innerBlock += SpatialVarDecl(v = indexVar, t = backends.spatial.common.ir.Int,
      init = Some(init), addressSpace = RegMemory)
    (block: MutableExprBlock) += SpIfThenElse(
      BinaryExpression(init, BinaryExpressionT.Operator.<, ArithExpression(stop)),
      trueBody = innerBlock, falseBody = MutableExprBlock())
    generateBody(innerBlock)
  }

  /**
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
    val memoryNeedsMaterialising = allTypedMemories.contains(mem) && allTypedMemories(mem).materialised

    if (memoryNeedsMaterialising &&
      !allTypedMemories(mem).declared &&
      mem.addressSpace != DRAMMemory) {

      (block: MutableExprBlock) += SpatialVarDecl(mem.variable, mem.t, init, mem.addressSpace)

      allTypedMemories(mem).declared = true
    }
  }

  private def generateValue(v: Value, block: MutableExprBlock): Unit = {
    declareMemoryIfRequired(v, block, init = Some(SpatialCode(v.value)))
  }

  // MapSeq
  private def generateMapSeqCall(m: MapSeq,
                                 call: FunCall,
                                 block: MutableExprBlock): Unit = {
    (block: MutableExprBlock) += Comment("map_seq")
    generateForeach(block, call.args.head, m.loopVar, Cst(1), generate(m.f.body, _), m.shouldUnroll)
    (block: MutableExprBlock) += Comment("end map_seq")
  }
  
  private def generateForeachCall(sf: SpForeach,
                                  call: FunCall,
                                  block: MutableExprBlock): Unit = {
    (block: MutableExprBlock) += Comment("foreach")
    generateForeach(block, call.args.head, sf.loopVar, sf.factor, generate(sf.f.body, _), sf.shouldUnroll)
    (block: MutableExprBlock) += Comment("end foreach")
  }

  private def generateFoldCall(asf: AbstractSpFold,
                               call: FunCall,
                               block: MutableExprBlock): Unit = {
    // TODO: add unrolled Fold generation
    // TODO: optimise loop generation. See OpenCLGenerator.generateOptimizedForLoopRepresentations

    val innerMapBlock = MutableExprBlock(Vector.empty, encapsulated = false)
    val innerReduceBlock = MutableExprBlock(Vector.empty, encapsulated = false)

    val accumulator = call.args.head

    val accum = valueAccessNode(accumulator.mem.variable)
    val counter = List(Counter(
      ArithExpression(getRangeAdd(asf.mapLoopVar).start),
      ArithExpression(getRangeAdd(asf.mapLoopVar).stop),
      ArithExpression(getRangeAdd(asf.mapLoopVar).step),
      ArithExpression(asf.factor)))
    val iterVars = List(CVar(asf.mapLoopVar))

    // TODO: Confirm that we don't need to generate anything outside fMap body
    (block: MutableExprBlock) += (asf match {
      case _: SpFold => SpatialAccelAST.Fold(accum, counter, iterVars, innerMapBlock, innerReduceBlock)
      case _: SpMemFold => SpatialAccelAST.MemFold(accum, counter, iterVars, innerMapBlock, innerReduceBlock)
    })

    generate(asf.fMap.body, innerMapBlock, memReturn = true)
    generate(asf.fReduce.body, innerReduceBlock)
  }

  private def generateForeach(block: MutableExprBlock,
                              array: Expr,
                              indexVar: Var,
                              parFactor: ArithExpr,
                              generateBody: (MutableExprBlock) => Unit,
                              needUnroll: Boolean = false): Unit = {
    // if we need to unroll (e.g. because of access to register memory)
    if (needUnroll) generateForeachUnrolled(block, indexVar, generateBody)
    else {
      if (PerformLoopOptimisation())
        generateOptimizedForeachRepresentations(block, array, indexVar, parFactor, generateBody)
      else
        generateDefaultForeachRepresentation(block, array, indexVar, parFactor, generateBody)
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

  private def generateOptimizedForeachRepresentations(block: MutableExprBlock,
                                                      argArray: Expr,
                                                      indexVar: Var,
                                                      parFactor: ArithExpr,
                                                      generateBody: (MutableExprBlock) => Unit): Unit = {
    val range = getRangeAdd(indexVar)
    val init = ArithExpression(range.start)

    argArray.t match {
      case _: ArrayType =>
        range.numVals match {
          case Cst(0) => (block: MutableExprBlock) += Comment("iteration count is 0, no loop emitted")

          case Cst(1) =>
            if (parFactor != Cst(1)) (block: MutableExprBlock) += Comment(s"Ignored parallel factor $parFactor")

            generateStatement(block, indexVar, generateBody, init)

          // TODO: See TestInject.injectExactlyOneIterationVariable
          // TODO: M / 128 is not equal to M /^ 128 even though they print to the same C code
          case _ if range.start.min.min == Cst(0) &&
            ArithExpr.substituteDiv(range.stop) == ArithExpr.substituteDiv(range.step) =>
            if (parFactor != Cst(1)) (block: MutableExprBlock) += Comment(s"Ignored parallel factor $parFactor")
            generateStatement(block, indexVar, generateBody, init)

          // TODO: See TestOclFunction.numValues and issue #62
          case _ if range.start.min.min == Cst(0) && range.stop == Cst(1) =>
            if (parFactor != Cst(1)) (block: MutableExprBlock) += Comment(s"Ignored parallel factor $parFactor")
            generateIfStatement(block, indexVar, generateBody, init, range.stop)

          case _ if range.numVals.min == Cst(0) && range.numVals.max == Cst(1) =>
            if (parFactor != Cst(1)) (block: MutableExprBlock) += Comment(s"Ignored parallel factor $parFactor")
            generateIfStatement(block, indexVar, generateBody, init, range.stop)

          case _ =>
            generateDefaultForeachRepresentation(block, argArray, indexVar, parFactor, generateBody)
        }
      case _            => throw new NotImplementedError() // should never get there
    }
  }

  private def generateDefaultForeachRepresentation(block: MutableExprBlock,
                                                   argArray: Expr,
                                                   indexVar: Var,
                                                   parFactor: ArithExpr,
                                                   generateBody: (MutableExprBlock) => Unit): Unit = {
    // TODO: add unrolled Foreach generation
    // TODO: optimise loop generation. See OpenCLGenerator.generateOptimizedForLoopRepresentations

    val innerBlock = MutableExprBlock(Vector.empty, encapsulated = false)

    (block: MutableExprBlock) += SpatialAccelAST.Foreach(
      counter = List(Counter(
        ArithExpression(getRangeAdd(indexVar).start),
        ArithExpression(getRangeAdd(indexVar).stop),
        ArithExpression(getRangeAdd(indexVar).step),
        ArithExpression(parFactor))),
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

    if (allTypedMemories(sMem).materialised && !allTypedMemories(sMem).implicitlyWrittenTo) {

      // Generate store
      (block: MutableExprBlock) += generateStoreNode(
        targetMem = sMem, targetType = call.t, targetView = call.outputView,
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
        case uf: UserFun          => FunctionCall(uf.name, args)
        case l: Lambda            => generateFunCall(l.body, args)
        case _                    => throw new NotImplementedError()
      }
      case _             => throw new NotImplementedError()
    }
  }

  /** Traverse f and print all user functions */
  private def generateUserFunctions(expr: Expr): Seq[DeclarationT] = {
    case class UserFunSignature(uf: UserFun,
                                argSpatialTypes: Seq[String],
                                argAss: Seq[AddressSpace],
                                retAs: AddressSpace) {
      // Defines equality using Spatial types, not Lift types because Spatial types also express memory spaces.
      // Implement the following logic:
      // If arguments are scalars, than only Lift types have to be the same for equality:
      // ((Float, SRAM), (Float, SRAM)) == ((Float, SRAM), (Float, DRAM))
      // If arguments are arrays, than both Lift types and Lift address spaces must be the same for equality
      // (([Float], SRAM), ([Float], SRAM)) != (([Float], SRAM), ([Float], DRAM))
      override def equals(obj: Any): Boolean = obj match {
        case that: UserFunSignature => uf == that.uf && argSpatialTypes == that.argSpatialTypes && retAs == that.retAs
        case _ => false
      }
    }

    // Collect user functions
    val userFuns = Expr.visitWithState(Set[UserFunSignature]())(expr, (expr, set) =>
      expr match {
        case call: FunCall                      => call.f match {
          case _: SpatialBuiltInFun  => set
          case uf: UserFun           => set + UserFunSignature(uf,
                                          call.args.map(arg => Printer.toString(arg.t, arg.addressSpace)),
                                          call.args.map(_.addressSpace), call.addressSpace)
          case vec: VectorizeUserFun => throw new NotImplementedError()
          case _                     => set
        }
        case ArrayFromUserFunGenerator(uf, _)   => throw new NotImplementedError()
        case Array2DFromUserFunGenerator(uf, _) => throw new NotImplementedError()
        case Array3DFromUserFunGenerator(uf, _) => throw new NotImplementedError()
        case _                                  => set
      })

    userFuns.toSeq.map {
      case UserFunSignature(uf, _, argAs, retAs) => AccelGenerator.createFunctionDefinition(uf, argAs, retAs) }
  }

  /**
   * Generate a simple or vector store
   */
  private def generateStoreNode(targetMem: SpatialMemory,
                                targetType: Type,
                                targetView: View,
                                srcAddressSpace: SpatialAddressSpace,
                                srcNode: AstNode): StatementT = {
    // Sanity check -- make sure this is the store of the producer of the associated memory
    assert(targetType == allTypedMemories(targetMem).writeT)

    val targetNode = accessNode(targetMem.variable, targetMem.addressSpace, targetType, targetView)

    if (srcAddressSpace == targetMem.addressSpace)
      AssignmentExpression(to = targetNode, srcNode)

    else (srcAddressSpace, targetMem.addressSpace) match {
      case (DRAMMemory, SRAMMemory) => SpLoad(src = srcNode, target = VarSlicedRef(targetMem.variable))

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
    if (!allTypedMemories(mem).materialised || allTypedMemories(mem).implicitlyReadFrom)
      // This memory is not materialised or is implicitly read from. If we are trying to load it, that means we are
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

      // not a memory collection: the default case
      case _ =>
        accessNode(mem.variable, mem.addressSpace, t, view)
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
   * Create an access node (i.e. of type NDVarSlicedRef) for variable v based on the
   * given address space and view
   *
   * @param v            The variable to access
   * @param addressSpace The address space, i.e. DRAM, SRAM, Reg
   * @param view         The view to access var `v`
   * @return An NDVarSlicedRef node accessing `v` as described in `view`.
   */
  private def accessNode(v: Var,
                         addressSpace: SpatialAddressSpace,
                         accessType: Type,
                         view: View): ExpressionT = {
    addressSpace match {
      case SRAMMemory | DRAMMemory | RegMemory =>
        // allTypedMemories(v).mem.t is the original type, i.e. the type of the data stored in allTypedMemories(v).mem
        // allTypedMemories(v).writeT is the write type of the memory producer (UserFun / Value)
        allTypedMemories(v).mem.t match {
          case _: ArrayType                 => arrayAccessNode(v, addressSpace, view)
          case _: ScalarType | _: TupleType => valueAccessNode(v)
          case _                            =>
            throw new TypeException(allTypedMemories(v).mem.t, "A known type", null)
        }

      case UndefAddressSpace | AddressSpaceCollection(_) =>
        throw new IllegalArgumentException(s"Cannot access data in $addressSpace")
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
                              addressSpace: SpatialAddressSpace,
                              view: View): ExpressionT = {
    addressSpace match {
      case SRAMMemory | DRAMMemory =>
        SpatialViewPrinter.emit(view, replacementsOfIteratorsWithValuesWithFuns, addressSpace)

      case RegMemory =>
        val x = SpatialViewPrinter.emit(view, replacementsOfIteratorsWithValuesWithFuns, addressSpace)
        x match {
          // TODO add sliced ref and unrolled register array access
          case _: VarIdxRef | _: VarSlicedRef =>
            VarIdxRef(v, suffix = Some(arrayAccessRegisterMem(v, view)))
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
   * @param v    The variable to access
   * @param view The view describing the access
   * @return A string of the form '_indices' where indices is the sliced multidimensional
   *         array index. The indices must be computable at compile time.
   */
  private def arrayAccessRegisterMem(v: Var, view: View): String = {
    // Compute the indices ...
    val indices = arrayAccessRegisterMemIndex(v, view)
    // ... and append it
    "_" + indices.map(_.map(Printer.toString(_)).mkString("_")).mkString("__")
  }

  /**
   * Generates integers charecterising one access to memory.
   * Produce a two-dimensional list of indices. The outer dimension contains
   * one element per dimension of the accessed memory; the inner dimension contains
   * one element per index in a slice.
   * For example:
   * arr(5, 2::3::11, 7, 1::3) => [[5], [2, 5, 8], [7], [1, 2]]
   *
   * @param v The variable referring to the memory being accessed
   * @param view Access view
   * @return A two-dimensional list of indices
   */
  private def arrayAccessRegisterMemIndex(v: Var, view: View): List[List[Int]] = {

    def asIndices(addr: List[AddressorT], errorMsg: String): List[Index] = {
      addr.map {
        case idx: ArrIndex => Index(idx)
        case _ => throw new AccelGeneratorException(errorMsg)
      }
    }

    val valueType = allTypedMemories(v).writeT

    // Get the addressors as if we were accessing a multidimensional array in memory and
    // convert them from Spatial AST to Lift IR
    val addressors: List[ArrayAddressor] = valueType match {

      case _: ScalarType | _: TupleType =>
        val a = SpatialViewPrinter.emit(view, replacementsOfIteratorsWithValues, RegMemory)
        a match {
          case VarSlicedRef(_, _, addr) =>
            asIndices(addr.get, errorMsg = "A scalar type can only be accessed using an array.")

          case x => throw new MatchError(s"Expected a NDVarSlicedRef, but got $x.")
        }

      case _: VectorType => throw new NotImplementedError()

      case ArrayType(_) =>

        SpatialViewPrinter.emit(view, replacementsOfIteratorsWithValues, RegMemory) match {
          case VarSlicedRef(_, _, addr) => addr.get.map {
            case idx: ArrIndex => Index(idx)
            case slice: ArrSlice => Slice(slice)
          }
          case x => throw new MatchError(s"Expected a NDVarSlicedRef, but got $x.")
        }

      case NoType | UndefType =>
        throw new TypeException(valueType, "A valid defined type", null)
    }

    // Replace where necessary and evaluate to Int
    val addrWithReplacements: List[List[Int]] = try {
      addressors.map(addr => {
        val sbstAddr = addr.visitAndRebuild(ArithExpr.substitute(_, replacementsOfIteratorsWithValues))

        sbstAddr.eval()
      })
    } catch {
      case NotEvaluableException()      =>
        throw new AccelGeneratorException(s"Could not access private array, as addressor $addressors could " +
          s"not be evaluated statically (given these replacementsOfIteratorsWithValues: $replacementsOfIteratorsWithValues)")
      case NotEvaluableToIntException() =>
        throw new AccelGeneratorException(s"Could not access private array, as addressor $addressors is " +
          s"larger than scala.Int.MaxValue (given these replacementsOfIteratorsWithValues: $replacementsOfIteratorsWithValues)")
    }

    val memTypeLengths = Type.getLengths(allTypedMemories(v).writeT)

    if (!memTypeLengths.forall(_.isEvaluable))
      throw new AccelGeneratorException(s"Private memory length has to be evaluable, but found $memTypeLengths")

    if (!addrWithReplacements.zip(memTypeLengths).forall{ case (addr, memLen) => addr.max < memLen.eval })
      throw new AccelGeneratorException(s"Out of bounds access to $v with $addrWithReplacements")

    addrWithReplacements
  }


  /**
   * An access to a variable as a value, i.e. a direct access by name.
   *
   * @param v The variable to access
   * @return A VarRef node wrapping `v`
   */
  private def valueAccessNode(v: Var): VarSlicedRef = {
    VarSlicedRef(v)
  }
}