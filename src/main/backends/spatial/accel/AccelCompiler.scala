package backends.spatial.accel

import _root_.ir.UndefType
import _root_.ir.ast.Lambda
import backends.common.Compiler
import backends.spatial.accel.generator.AccelGenerator
import backends.spatial.common.ir.{InferSpatialAddressSpace, SpatialMemoryAllocator}

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

    // Build view
    View(f)

    // Generate code
    AccelGenerator(f)
  }

  def allocateMemory(lambda: Lambda): Unit = {
    // Allocate memory
    SpatialMemoryAllocator(f)
    // Remove redundant memory
  }
}
