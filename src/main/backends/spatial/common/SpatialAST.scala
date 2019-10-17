package backends.spatial.common

import _root_.ir.Type
import ir.SpatialAddressSpace
import core.generator.GenericAST._
import core.generator.PrettyPrinter._

object SpatialAST {
  trait SpatialAddressSpaceOperator {
    val addressSpace: SpatialAddressSpace
  }

  /*
  Parameter declaration
    */
  trait SpParamDeclT extends DeclarationT {
    val name: String
    val t: Type
    def addressSpace: SpatialAddressSpace

    override def print(): Doc = name <> ":" <+> Printer.toString(t, addressSpace)
  }

  case class SpParamDecl(name: String, t: Type, addressSpace: SpatialAddressSpace) extends SpParamDeclT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit): Unit = {}
  }


  trait ExprBasedFunctionT extends DeclarationT {
    val name: String
    val ret: Type
    val addressSpace: SpatialAddressSpace
    val params: List[SpParamDeclT]
    val body: MutableExprBlockT

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
        ")" <+>
        // TODO: Until we don't fix UserFun return types, let Scala infer the type itself.
        // The problem is in "aDRAM >> toSRAM(idArray())" =>
        // "def id(a: DRAM1[Float]): SRAM1[Float]" (the return type should be DRAM1 instead)
        /*<> ":" <+>
          // print the return type
          Printer.toString(ret, addressSpace)*/
        "=" <+>
        // print the body
        body.print()
    }
  }

  case class ExprBasedFunction(name: String, ret: Type, addressSpace: SpatialAddressSpace,
                               params: List[SpParamDeclT], body: MutableExprBlockT)
    extends ExprBasedFunctionT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      ExprBasedFunction(name, ret, addressSpace, params.map(_.visitAndRebuild(pre, post).asInstanceOf[SpParamDeclT]),
        body.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
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

  /*
  An If-then-else sequence
   */
  trait SpIfThenElseT extends StatementT with ExpressionT {
    val cond: ExpressionT
    val trueBody: MutableExprBlockT
    val falseBody: MutableExprBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, cond)) |>
        (visitFun(_, trueBody)) |>
        (visitFun(_, falseBody))
    }

    override def print(): Doc = {
      text("if (") <> cond.print <> ")" <> trueBody.print <>
        (if (falseBody != MutableBlock()) {
          text(" else ") <> falseBody.print()
        } else {
          empty
        })
    }
  }

  case class SpIfThenElse(cond: ExpressionT,
                          trueBody: MutableExprBlockT,
                          falseBody: MutableExprBlockT) extends SpIfThenElseT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      SpIfThenElse(cond.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        trueBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT],
        falseBody.visitAndRebuild(pre, post).asInstanceOf[MutableExprBlockT])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      cond.visitBy(pre, post)
      trueBody.visitBy(pre, post)
      falseBody.visitBy(pre, post)
    }
  }
}
