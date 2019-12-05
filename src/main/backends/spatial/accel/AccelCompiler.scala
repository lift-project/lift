package backends.spatial.accel

import _root_.ir.UndefType
import _root_.ir.ast.Lambda
import backends.spatial.accel.generator.AccelGenerator
import backends.spatial.common.ir.view.SpatialView
import backends.spatial.common.ir.{CollectContextualSpatialMemory, ContextualMemoryCollection, InferSpatialAddressSpace, SpatialMemoryAllocator}
import core.generator.GenericAST.ExprBlock
import _root_.ir.TypeChecker

/**
 * The compiler performs all the passes over the AST that populate it with new
 * information such as types, address spaces, memories and loop unrolling.
 * It then passes the baton to the generator that produces the code using the
 * information inferred by the compiler.
 */
object AccelCompiler {
  def apply(f: Lambda): (ExprBlock, ContextualMemoryCollection) = {
    // Check types
    f.params.foreach(p => assert(p.t != UndefType))
    if (f.body.t == UndefType)
      TypeChecker(f)

    // Infer address spaces
    InferSpatialAddressSpace(f)

    // Estimate loop ranges and counts
    RangesAndCountsSp(f, collection.Map())

    // Allocate memory
    SpatialMemoryAllocator(f)

    // Collect contextualised memories
    val allTypedMemories = CollectContextualSpatialMemory(f)

    // Loop unrolling
    ShouldUnroll(f, allTypedMemories)
    // TODO: Check if lambda is legal

    // Build view
    SpatialView(f)

    // Generate code
    val block = AccelGenerator(f, allTypedMemories)

    (block, allTypedMemories)
  }
}
