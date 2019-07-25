package opencl.executor

import ir.ast.Lambda
import lift.arithmetic._

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox

/**
 * Uses Scala reflection API to evaluate a String containing Scala code
 * This object assumes that the given String evaluates to on object of Type Lambda
 *
 * This object can be used to interface with other languages, like C, or compile source code stored in a file
 */
object Eval {
  def apply(code: String): Lambda = {
    eval(code).asInstanceOf[Lambda]
  }

  def getMethod(code:String): Seq[ArithExpr] => Lambda = {
    eval(code).asInstanceOf[Seq[ArithExpr] => Lambda]
  }

  def eval(code: String): Any = {
    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val tb = mirror.mkToolBox()
    val tree = tb.parse(s"""
                           |import arithmetic._
                           |import lift.arithmetic._
                           |import lift.arithmetic.simplifier._
                           |import ir._
                           |import ir.ast._
                           |import opencl.ir._
                           |import opencl.ir.pattern._
                           |import opencl.ir.ast._
                           |import opencl.generator.NDRange
                           |$code
                         """.stripMargin)
    tb.eval(tree)
  }
}

