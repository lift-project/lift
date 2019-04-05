package opencl.executor

import core.generator.GenericAST.{Block, MutableBlock}
import ir.TypeChecker
import ir.ast.Lambda
import lift.arithmetic.{?, ArithExpr}
import opencl.generator.{NDRange, OpenCLGenerator, Verbose}

import scala.collection.immutable

/**
 * This object provides an interface for compiling a lambda object into OpenCL code
 */
object Compile {
  /**
   * Evaluates the given string under the assumption that the string defines a lambda and compiles it afterwards.
   * Returns a pair consisting of the compiled OpenCL code and the lambda evaluated from the input string.
   */
  def apply(code: String): String = {
    val f = Eval(code)
    apply(f)
  }

  /**
   * Compiles the given lambda without any information about the local of global size
   */
  def apply(f: Lambda): String = apply(f, ?, ?, ?)

  /**
   * Compiles the given lambda with the given local sizes but without any information about the global size
   */
  def apply(f: Lambda,
            localSize1: ArithExpr, localSize2: ArithExpr, localSize3: ArithExpr): String =
    apply(f, localSize1, localSize2, localSize3, ?, ?, ?, immutable.Map())

  def apply(f: Lambda, localSizes: NDRange, globalSizes: NDRange): String =
    apply(f,
      localSizes(0), localSizes(1), localSizes(2),
      globalSizes(0), globalSizes(1), globalSizes(2), immutable.Map())

  /**
   * Compiles the given lambda with the given local and global size.
   * All arithmetic expressions can be specified to be unknown using the ? notation
   */
  def apply(f: Lambda,
            localSize0: ArithExpr, localSize1: ArithExpr, localSize2: ArithExpr,
            globalSize1: ArithExpr, globalSize2: ArithExpr, globalSize3: ArithExpr,
            valueMap: immutable.Map[ArithExpr, ArithExpr]): String = {
    // 1. type check
    TypeChecker(f)

    // 2. generate OpenCL kernel
    val kernel = OpenCLGenerator.generate(f,
      NDRange(localSize0, localSize1, localSize2),
      NDRange(globalSize1, globalSize2, globalSize3), valueMap)

    // 3. print and return kernel code
    if (Verbose()) {
      println("Kernel code:")
      println(kernel)
    }
    kernel
  }

  def !!(f: Lambda, localSize: NDRange = NDRange(1,1,1), globalSize : NDRange = NDRange(1,1,1) ) : Block = {

    val res:MutableBlock = OpenCLGenerator !! (f,
      localSize,
      globalSize, immutable.Map())

    res.toBlock

  }

}
