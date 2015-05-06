package opencl.executor

import ir.Lambda

/**
 * Uses an existing framework from Twitter to evaluate a String containing Scala code.
 * This object assumes that the given String evaluates to on object of Type Lambda
 *
 * This object can be used to interface with other languages, like C, or compile source code stored in a file
 */
object Eval {
  def apply(code: String): Lambda = {
    val imports = """
                    |import arithmetic._
                    |import ir._
                    |import opencl.ir._
                    |
                  """.stripMargin
    com.twitter.util.Eval[Lambda](imports ++ code)
  }
}