package backends.spatial.accel

import backends.common.Compiler
import core.generator.GenericAST.{Block, MutableBlock}
import ir.UndefType
import ir.ast.Lambda

object AccelCompiler extends Compiler {
  def apply(f: Lambda): String = {
    if (f.body.t == UndefType)
      this.typeCheck(f)

    // Infer address spaces

    // Ranges and counts

    // Memory allocation

    // Loop unrolling

    // Barrier elimination

    // Check if lambda is legal

    // Collect typed memory

    // Build biew

    // Generate code
    AccelGenerator(f)
  }
}
