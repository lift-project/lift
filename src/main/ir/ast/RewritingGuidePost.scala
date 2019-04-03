package ir.ast

import ir.Type
import ir.interpreter.Interpreter.ValueMap

/**
  * A "guide post" primitive to use for guiding the rewriter as a temporary and hacky alternative to detecting
  * generic patterns.
  * This an identity primitive; it should be rewritten away before attempting to compile the expression.
  */
case class RewritingGuidePost(name: String) extends Pattern(arity = 1) {
  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    args.head
  }
}
