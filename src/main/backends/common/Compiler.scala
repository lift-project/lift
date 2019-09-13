package backends.common

import backends.c.common.common_cast.CbackendCAST.SourceFile
import backends.c.common.loop_var_inference.LoopVarInference
import backends.c.common.memory_management.{FinalMemoryAllocationAnalysis, InferHostMemoryAddressSpace, MemoryAllocator}
import backends.c.common.printer.CASTPrinter
import backends.c.common.view.{InputView, OutputView}
import core.generator.GenericAST.CVarWithType
import ir.ast.{Expr, Lambda}
import ir.{TypeChecker, UndefType}
import lift.arithmetic.ArithExpr
import opencl.ir.OpenCLAddressSpace

import scala.language.postfixOps

trait Compiler {

  //no need to override typeCheck,
  //as every backend-specific one are also IRNode,
  //thus amenable to type check
  final def typeCheck(lambda:Lambda) : Unit = {

    println("2. common type check")

    lambda.params.foreach(p => assert(p.t != UndefType))
    TypeChecker(lambda)

    lambda visitBy {
      case e: Expr =>
        assert(e.t != UndefType)
      case _ =>
    }

  }
  def memorySpaceInference(lambda:Lambda) : Unit = {

    println("3. common infer memory space")

    InferHostMemoryAddressSpace(lambda)

  }

  def loopVarInference(lambda:Lambda) : Unit = {

    println("4. common infer loop var in trait")

    LoopVarInference(lambda)

  }

  def memoryAlloc(lambda:Lambda) : Unit = {

    println("5. common memory alloc in trait")

    MemoryAllocator(lambda)

  }

  def finalMemoryAllocationAnalysis(lambda: Lambda) : Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace) ] = {

    println("6. common final memory allocation analaysis in trait")

    FinalMemoryAllocationAnalysis(lambda)

  }

  def inputView(lambda:Lambda) : Unit = {

    println("7. common input view in trait")

    InputView(lambda)

  }

  def outputView(lambda:Lambda) : Unit = {

    println("8. common output view in trait")

    OutputView(lambda)

  }

  // compile a lambda
  def compile(lambda: Lambda, path: String, files: List[String], func_name : String = "execute"): Unit

}
