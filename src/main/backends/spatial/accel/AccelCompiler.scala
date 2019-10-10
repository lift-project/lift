package backends.spatial.accel

import _root_.ir.UndefType
import _root_.ir.ast.Lambda
import backends.common.Compiler
import backends.spatial.accel.generator.AccelGenerator
import backends.spatial.common.ir.view.View
import backends.spatial.common.ir.{CollectTypedSpatialMemory, InferSpatialAddressSpace, SpatialMemoryAllocator}

/**
 * The compiler performs all the passes over the AST that populate it with new
 * information such as types, address spaces, memories and loop unrolling.
 * It then passes the baton to the generator that produces the code using the
 * information inferred by the compiler.
 */
object AccelCompiler extends Compiler {
  def apply(f: Lambda): String = {
    // Check types
    if (f.body.t == UndefType)
      this.typeCheck(f)

    // Infer address spaces
    InferSpatialAddressSpace(f)

    // TODO: Ranges and counts

    // Allocate memory
    SpatialMemoryAllocator(f)

    // Collect typed memories
    val allTypedMemories = CollectTypedSpatialMemory(f)

    // Loop unrolling
    ShouldUnroll(f, allTypedMemories)

    // TODO: Barrier elimination

    // TODO: Check if lambda is legal

    // Build view
    View(f)

    // Generate code
    AccelGenerator(f, allTypedMemories)
  }
}
