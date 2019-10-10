package backends.spatial.common

import _root_.ir.Type
import backends.spatial.common.ir.SpatialAddressSpace
import core.generator.GenericAST._
import core.generator.PrettyPrinter._

object SpatialAST {
  trait SpatialAddressSpaceOperator {
    val addressSpace: SpatialAddressSpace
  }


  trait ExprBasedFunctionT extends DeclarationT {
    def name: String

    def ret: Type

    def params: List[ParamDeclT]

    def body: MutableExprBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        // visit the function object
        (visitFun(_, this)) |>
        // visit the parameters
        (params.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        }) |>
        // visit the body
        (body.visit(_)(visitFun))
    }

    override def print(): Doc = {
      // print the name
      "def" <+> name <>
        // print parameters
        "(" <>
        intersperse(params.map(_.print())) <>
        ")" <> ":" <+>
        // print the return type
        Printer.toString(ret) <+> "=" <+>
        // print the body
        bracket("{", body.print(), "}")

    }
  }

  case class ExprBasedFunction(name: String, ret: Type, params: List[ParamDeclT],
                               body: MutableExprBlockT) extends ExprBasedFunctionT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      Function(name, ret, params.map(_.visitAndRebuild(pre, post).asInstanceOf[ParamDeclT]),
        body.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      params.foreach(_.visitBy(pre, post))
      body.visitBy(pre, post)
    }
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
