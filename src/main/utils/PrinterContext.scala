package utils

import ir._
import ir.view.{AccessVar, CastedPointer}
import lift.arithmetic._
import opencl.generator.{NotPrintableExpression, OclFunction}

class PrintContext {
  /** Output stream */
  val sb: StringBuilder = new StringBuilder
  /** Current indentation (depth of scope) */
  var indent: Int = 0
  val tabSize = 2



  def print(s: String): Unit = {
    sb ++= s
  }

  /** Print the given string and create an indented new line */
  def println(s: String): Unit = {
    sb ++= s + "\n" + tab()
  }

  /** Create a block between braces. */
  def printBlock(code: => Unit): Unit = {
    indent += 1
    println("{")
    code
    indent -= 1
    moveCursorBack(tabSize)
    print("}")
  }

  /** Insert the correct indentation */
  def tab(): String = {
    lazy val whiteSpace: String = " " * tabSize
    whiteSpace * indent
  }

  /** Move cursor back by given size. Used to fix indentation */
  def moveCursorBack(size: Int): Unit = {
    for (_ <- 1 to size) {
      if (sb.last.isWhitespace) {
        sb.deleteCharAt(sb.size - 1)
      }
    }
  }
}
