package backends.spatial.accel.generator

import backends.spatial.accel.generator.SpatialAccelAST.{AddressorT, ArrIndex, ArrSlice, Counter, NDVarSlicedRef, SpatialVarDecl}
import backends.spatial.accel.ir.pattern.SpMemFold
import backends.spatial.common.Printer
import backends.spatial.common.SpatialAST.SpatialCode
import backends.spatial.common.ir.view.{ArrayAddressor, Index, Slice}
import backends.spatial.common.ir.{AddressSpaceCollection, CollectTypedSpatialMemory, DRAMMemory, RegMemory, SRAMMemory, SpatialAddressSpace, SpatialMemory, SpatialMemoryCollection, SpatialNullMemory, TypedMemoryCollection, TypedSpatialMemory, UndefAddressSpace}
import core.generator.GenericAST.{ArithExpression, AssignmentExpression, AstNode, Block, Comment, ExpressionT, FunctionCall, MutableBlock, MutableExprBlock, StructConstructor, VarRef}
import ir.ast.{AbstractMap, Expr, FunCall, Lambda, Map, Param, UserFun, Value}
import ir.view.{View, ViewPrinter}
import ir.{ArrayType, ArrayTypeWS, Memory, NoType, ScalarType, TupleType, Type, TypeException, UndefType, VectorType}
import lift.arithmetic._

import scala.collection.immutable

object AccelGenerator {

  type ValueTable = immutable.Map[ArithExpr, ArithExpr]
  type SymbolTable = immutable.Map[Var, Type]

//  TODO: fill replacements
  private var replacements: ValueTable = immutable.Map.empty
  private var replacementsWithFuns: ValueTable = immutable.Map.empty
  private var allTypedMemories: TypedMemoryCollection = _

  def apply(f: Lambda): Block = {
    // Initialise the block
    val accelBlock = MutableExprBlock(Vector.empty)

    // Generate user functions

    // Collect typed memories
    allTypedMemories = CollectTypedSpatialMemory(f)


    // TODO: Collect dynamic memory that's shared between the host and accel


//     TODO(maybe): Collect value (literal) memories
//
//     TODO(maybe): Collect dynamically and statically allocated typed memories
//
//     Declare static buffers inside the scope
//     Declare value buffers inside the scope
//     Declare private variables
    // TODO: Do not declare buffers in the beginning of the block.
    //  Declare them in proper inner scopes

//     Generate the main part of the block
    generate(f.body, accelBlock)

    // Perform common subexpression elimination

    // Return the result
  }


  private def generate(expr: Expr, block: MutableExprBlock): Unit = {
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
          case am: AbstractMap =>
            am match {
              case _: Map             =>
              case _                  => throw new NotImplementedError()
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
                (block: MutableExprBlock) += AssignmentExpression(
                  to = accessNode(call.mem.variable, call.addressSpace, outSizeView),
                  value = getArraySize(SpatialMemory.asSpatialMemory(call.args.head.mem), inSizeView)
                )
            }

          case smf: SpMemFold         => generateSpMemFold(smf, call, block)

          case u: UserFun             => generateUserFunCall(u, call, block)


          case _                      => throw new NotImplementedError()
        }

      case v: Value             => generateValue(v, block)
      case _: Param             =>
      case _  => throw new NotImplementedError()
    }
  }

  private def declareMemoryIfRequired(expr: Expr,
                                      block: MutableExprBlock,
                                      init: Option[AstNode] = None): Unit = {
    if (!allTypedMemories(expr.mem).declared) {
      val sMem = expr.mem.asInstanceOf[SpatialMemory]

      (block: MutableExprBlock) += SpatialVarDecl(sMem.variable, sMem.t, init, sMem.addressSpace)
    }
  }

  private def generateValue(v: Value, block: MutableExprBlock): Unit = {
    declareMemoryIfRequired(v, block, init = Some(SpatialCode(v.value)))
  }

  private def generateSpMemFold(smf: SpMemFold,
                                call: FunCall,
                                block: MutableExprBlock): Unit = {
    val innerMapBlock = MutableExprBlock(Vector.empty)
    val innerReduceBlock = MutableExprBlock(Vector.empty)

    (block: MutableExprBlock) += Comment("SpMemFold")

    val accumulator = call.args.head

    (block: MutableExprBlock) += SpatialAccelAST.Reduce(
      accum = valueAccessNode(accumulator.mem.variable),
      counter = List(Counter(
        ArithExpression(getRangeAdd(smf.mapLoopVar).start),
        ArithExpression(getRangeAdd(smf.mapLoopVar).stop),
        ArithExpression(smf.stride),
        ArithExpression(smf.factor))
      ),
      // TODO: Confirm that we don't need to generate anything outside fMap body
      mapFun = innerMapBlock,
      reduceFun = innerReduceBlock)

    generate(smf.fMap.body, innerMapBlock)
    generate(smf.fReduce.body, innerReduceBlock)

    (block: MutableExprBlock) += Comment("end reduce_seq")
  }

  private def generateUserFunCall(u: UserFun,
                                  call: FunCall,
                                  block: MutableExprBlock): MutableExprBlock = {
    val funcall_node = generateFunCall(call, generateLoadNodes(call.args: _*))
//    val store_node = generateStoreNode(mem, call.t, call.outputView, funcall_node)
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

        case _ => throw new NotImplementedError()
      }
      case _             => throw new NotImplementedError()
    }
  }

  /**
   * Generate a simple or vector store.
   */
  private def generateStoreNode(mem: SpatialMemory,
                                targetType: Type,
                                view: View,
                                value: AstNode): ExpressionT = {
    // Sanity check -- make sure this is the store of the producer of the associated memory
    assert(targetType == allTypedMemories(mem).writeT)

    AssignmentExpression(to = accessNode(mem.variable, mem.addressSpace, targetType, view), value)
  }

  private def generateLoadNodes(args: Expr*): List[AstNode] = {
    args.map(arg => {
      val mem = SpatialMemory.asSpatialMemory(arg.mem)
      generateLoadNode(mem, arg.t, arg.view)
    }).toList
  }

  private def generateLoadNode(mem: SpatialMemory, t: Type, view: View): ExpressionT = {
    mem match {
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
      case SpatialNullMemory => ViewPrinter.emit(view)
        // TODO: finish
    }
  }

  private def getRangeAdd(indexVar: Var) = {
    indexVar.range match {
      case r: RangeAdd => r
      case _           =>
        throw new AccelGeneratorException("Cannot handle range for the loop: " + indexVar.range)
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
      case SRAMMemory | DRAMMemory =>
        // allTypedMemories(v).mem.t is the original type, i.e. the type of the data stored in allTypedMemories(v).mem
        // allTypedMemories(v).writeT is the write type of the memory producer (UserFun / Value)
        val originalType = allTypedMemories(v).mem.t
        originalType match {
          case _: ArrayType                                 => arrayAccessNode(v, addressSpace, view)
          case _: ScalarType | _: VectorType | _: TupleType => valueAccessNode(v)
          case NoType | UndefType                           =>
            throw new TypeException(originalType, "A valid type", null)
        }

      case RegMemory =>
        allTypedMemories.intermediates(RegMemory).find(m => m.mem.variable == v) match {
          case Some(typedMemory) => typedMemory.mem.t match {
            case _: ArrayType                                 => arrayAccessNode(v, addressSpace, view)
            case _: ScalarType | _: VectorType | _: TupleType => valueAccessNode(v)
            case NoType | UndefType                           =>
              throw new TypeException(typedMemory.mem.t, "A valid type", null)
          }
          case _  => valueAccessNode(v)
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
                              addressSpace: SpatialAddressSpace,
                              view: View): ExpressionT = {
    addressSpace match {
      case SRAMMemory | DRAMMemory =>
        ViewPrinter.emit(view, replacementsWithFuns, addressSpace)

      case RegMemory =>
        ViewPrinter.emit(view, replacementsWithFuns, addressSpace) match {
          case VarRef(_, _, _) =>
            VarRef(v, suffix = Some(arrayAccessRegisterMem(v, view)))
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
        ViewPrinter.emit(view, replacements, RegMemory) match {
          case NDVarSlicedRef(_, _, addr) =>
            asIndices(addr.get, errorMsg = "A scalar type can only be accessed using an array.")

          case x => throw new MatchError(s"Expected a NDVarSlicedRef, but got $x.")
        }

      case _: VectorType => throw new NotImplementedError()

      case ArrayType(_) =>

        ViewPrinter.emit(view, replacements, RegMemory) match {
          case NDVarSlicedRef(_, _, addr) => addr.get.map {
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
        val sbstAddr = addr.visitAndRebuild(ArithExpr.substitute(_, replacements))

        sbstAddr.eval()
      })
    } catch {
      case NotEvaluableException()      =>
        throw new AccelGeneratorException(s"Could not access private array, as addressor $addressors could " +
          s"not be evaluated statically (given these replacements: $replacements)")
      case NotEvaluableToIntException() =>
        throw new AccelGeneratorException(s"Could not access private array, as addressor $addressors is " +
          s"larger than scala.Int.MaxValue (given these replacements: $replacements)")
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
  private def valueAccessNode(v: Var): NDVarSlicedRef = {
    NDVarSlicedRef(v)
  }
}
