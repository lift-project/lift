package backends.spatial.accel

import backends.common.Compiler
import backends.spatial.runtime.SpatialMemoryAllocator
import core.generator.GenericAST.{Block, MutableBlock}
import _root_.ir.UndefType
import _root_.ir.ast.Lambda
import backends.spatial.ir.InferSpatialAddressSpace

object AccelCompiler extends Compiler {
  def apply(f: Lambda): String = {
    if (f.body.t == UndefType)
      this.typeCheck(f)

    // Infer address spaces
    InferSpatialAddressSpace(f)

    // Ranges and counts

    // Memory allocation
    allocateMemory(f)

    // Loop unrolling

    // Barrier elimination

    // Check if lambda is legal

    // Build biew

    // Generate code
    AccelGenerator(f)
  }

  def allocateMemory(lambda: Lambda): Unit = {
    // Allocate memory
    SpatialMemoryAllocator(f)
    // Remove redundant memory
  }
}
