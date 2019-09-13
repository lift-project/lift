package backends.spatial.host

import core.generator.GenericAST.{AstNode, Block, BlockMember, BlockT, StatementT}
import core.generator.PrettyPrinter._

object HostAST {

  case class AccelScope(body: BlockT) extends StatementT {

    override def print(): Doc = "Accel" <+> bracket("{", body.print(), "}")

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = body.visitBy(pre, post)

    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode =
      AccelScope(body.visitAndRebuild(pre, post).asInstanceOf[BlockT])
  }

}
