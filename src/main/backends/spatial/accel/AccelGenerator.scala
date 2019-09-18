package backends.spatial.accel

import _root_.ir.ast.{Expr, FunCall, Lambda}
import backends.spatial.host.SpatialHostAST.AccelScope
import core.generator.GenericAST.{Block, Comment, MutableBlock}
import ir.UndefType
import opencl.ir.pattern.ReduceSeq

object AccelGenerator {
  def apply(f: Lambda): Block = {
    // Initialise the block
    val accelBlock = MutableBlock(Vector.empty, global = true)

    // Generate user functions

    // Collect dynamic memory that's shared between the host and accel


    // TODO inside the body:
//     Collect value (literal) memories
//
//     Collect dynamically and statically allocated typed memories
//
//     Declare static buffers inside the scope
//
//     Declare value buffers inside the scope
//
//     Declare private variables
//
//     Generate the main part of the block
    generate(f.body, accelBlock)

    // Perform common subexpression elimination

    // Return the result
  }


  private def generate(expr: Expr, block: MutableBlock): Unit = {
    assert(expr.t != UndefType)

    // Generate arguments
    expr match {
      case f: FunCall => f.args.foreach(generate(_, block))
      case _          =>
    }

    expr match {
      case call: FunCall  => call.f match {
        case r: ReduceSeq         => generateReduceSeqCall(r, call, block)

        case _                    => throw new NotImplementedError()
      }
      case _              => throw new NotImplementedError()
    }
  }

  private def generateReduceSeqCall(r: ReduceSeq,
                                    call: FunCall,
                                    block: MutableBlock): Unit = {
    val innerBlock = MutableBlock(Vector.empty)
    (block: MutableBlock) += Comment("reduce_seq")

    if (r.shouldUnroll) {
      // TODO
    }

    // TODO: <NAUMS: last stop>

    (block: MutableBlock) += Comment("end reduce_seq")
  }
}
