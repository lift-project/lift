package generic.ast

import generic.ast.GenericAST._
import PrettyPrinter._

case class AstPrinter(ast: AstNode) {
  def apply(): String = {
    val ctx = new PrintContext

    def time[R](name: String, block: => R): R = {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()
      println(s"Elapsed time for $name: " + ((t1 - t0) * 1e-6) + "ms")
      result
    }

    // Time how long it takes to run the method
    time("printStatefully", ast.printStatefully(ctx))
    val testPrint = time("statefulToString", ctx.sb.toString())
    println(testPrint)

    val wadlerAST = time("wadlerPrint", ast.print())
    val result = time("layout", layout(wadlerAST))

    val sb = new StringBuilder()
    val result1 = time("layoutS", layoutS(wadlerAST))

    result1
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
    * newline or tabs */
  def +=(s: String): Unit = {
    sb ++= s
  }

  /** Print the given string and create an indented new line */
  def ++=(s: String): Unit = {
    sb ++= tab() + s + "\n"
  }

  /** Start a block by indenting */
  def unary_+(): Unit = {
    indent += 1
  }

  /** End a block by un-indenting */
  def unary_-(): Unit = {
    indent -= 1
  }

  def newln(): Unit = {
    sb ++= "\n" ++ tab()
  }

  //  def newln() : Unit = {
  //    sb ++= "\n"
  //  }

  /** Insert the correct indentation */
  def tab(): String = {
    lazy val whiteSpace: String = " " * tabSize
    whiteSpace * indent
  }
}
