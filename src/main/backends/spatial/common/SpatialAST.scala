package backends.spatial.common

import backends.spatial.common.ir.SpatialAddressSpace
import core.generator.GenericAST.{AstNode, RawCodeT}

object SpatialAST {
  trait SpatialAddressSpaceOperator {
    val addressSpace: SpatialAddressSpace
  }

  /** Inline native code block. Used for Value
   *
   * @param code Native code to insert
   */
  case class SpatialCode(code: String, pre1: String = "", pre2: String ="", post1: String ="", post2: String = "") extends RawCodeT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }
}
