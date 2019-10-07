package backends.spatial.accel.generator

import backends.spatial.accel.generator.SpatialAccelAST.Counter
import backends.spatial.accel.ir.pattern.SpMemFold
import backends.spatial.common.ir.{SpatialMemory, SpatialMemoryCollection, SpatialNullMemory}
import core.generator.GenericAST.{ArithExpression, AstNode, Block, Comment, ExpressionT, FunctionCall, MutableExprBlock, StructConstructor, VarRef}
import ir.ast.{Expr, FunCall, Lambda, UserFun}
import ir.view.{View, ViewPrinter}
import ir.{TupleType, Type}
import lift.arithmetic.{RangeAdd, Var}

object AccelGenerator {
  def apply(f: Lambda): Block = {
    // Initialise the block
    val accelBlock = MutableExprBlock(Vector.empty)

    // Generate user functions

    // Collect dynamic memory that's shared between the host and accel


    // TODO inside the body:
//     Collect value (literal) memories
//
//     Collect dynamically and statically allocated typed memories
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
      case call: FunCall  => call.f match {
        case smf: SpMemFold         => generateSpMemFold(smf, call, block)

        case u: UserFun            => generateUserFunCall(u, call, block)


        case _                      => throw new NotImplementedError()
      }
      case _              => throw new NotImplementedError()
    }
  }

  private def generateSpMemFold(smf: SpMemFold,
                                call: FunCall,
                                block: MutableExprBlock): Unit = {
    val innerMapBlock = MutableExprBlock(Vector.empty)
    val innerReduceBlock = MutableExprBlock(Vector.empty)

    (block: MutableExprBlock) += Comment("SpMemFold")

    val accumulator = call.args.head

    // Declare and initialise accumulator
    generate(accumulator, block)

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
          throw new AccelGeneratorException(s"Found a OpenCLMemoryCollection for var: " +
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
   * An access to a variable as a value, i.e. a direct access by name.
   *
   * @param v The variable to access
   * @return A VarRef node wrapping `v`
   */
  private def valueAccessNode(v: Var): VarRef = {
    VarRef(v)
  }
}
