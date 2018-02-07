package generic.ast

import generic.ast.GenericAST._

case class AstPrinter(ast: AstNode) {
  def apply() : String = {
    val ctx = new PrintContext
    ast.print(ctx)
    ctx.sb.toString()
  }
}

/**
  * Stateful printing context used while traversing/printing the AST
  *
  * Similar to the abstract "Printer" class used elsewhere, but passed as an
  * argument, and invoked rather than being used
  */
class PrintContext {
  /** Output stream */
  val sb: StringBuilder = new StringBuilder
  /** Current indentation (depth of scope) */
  var indent: Int = 0
  val tabSize = 2

  /** Print out the string by just appending it to the list, without a
    * newline or tabs*/
  def +=(s: String) : Unit = {
    sb ++= s
  }

  /** Print the given string and create an indented new line */
  def ++=(s: String) : Unit = {
    sb ++= tab() + s + "\n"
  }

  /** Start a block by indenting */
  def unary_+() : Unit = {
    indent += 1
  }

  /** End a block by un-indenting */
  def unary_-() : Unit = {
    indent -= 1
  }

  def endln() : Unit = {
    sb ++= "\n"
  }

  /** Insert the correct indentation */
  def tab(): String = {
    lazy val whiteSpace: String = " " * tabSize
    whiteSpace * indent
  }
}
