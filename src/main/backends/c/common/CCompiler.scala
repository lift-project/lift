package backends.c.common

import backends.c.common.common_cast.CbackendCAST.SourceFile
import backends.c.common.loop_var_inference.LoopVarInference
import backends.c.common.memory_management.{FinalMemoryAllocationAnalysis, InferHostMemoryAddressSpace, MemoryAllocator}
import backends.c.common.printer.CASTPrinter
import core.generator.GenericAST.{Block, CVarWithType}
import backends.c.common.view.{InputView, OutputView}
import backends.common.Compiler
import ir.{TypeChecker, UndefType}
import ir.ast.{Expr, Lambda}
import lift.arithmetic.ArithExpr
import opencl.ir.OpenCLAddressSpace

import scala.language.postfixOps

trait CCompiler extends Compiler {

  def lowerIR2CAST(lambda: Lambda,
                   memoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)],
                   path: String,
                   files: List[String],
                   func_name: String
                  ) : List[SourceFile]

  final def castPrinter(files : List[SourceFile]) : Unit = {

    CASTPrinter(files)

  }

  //compile a lambda
  def compile(lambda: Lambda, path: String, files: List[String], func_name : String = "execute"): Unit = {

    println("1.compiler called")


    import scala.sys.process._
    (s"mkdir -p $path") !

    typeCheck(lambda)
    memorySpaceInference(lambda)
    loopVarInference(lambda)
    memoryAlloc(lambda)
    val finalMemoryAllocated = finalMemoryAllocationAnalysis(lambda)
    inputView(lambda)
    outputView(lambda)

    val listOfSourceFiles = lowerIR2CAST(lambda, finalMemoryAllocated, path, files, func_name)

    castPrinter(listOfSourceFiles)

    println("n.compiler done")

  }

}
