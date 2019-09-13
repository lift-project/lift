package backends.c.mpi

import backends.c.common.CCompiler
import backends.c.common.common_cast.CbackendCAST.SourceFile
import backends.c.mpi.lowering.LowerIR2MPICAST
import core.generator.GenericAST.CVarWithType
import ir.ast.Lambda
import lift.arithmetic.ArithExpr
import opencl.ir.OpenCLAddressSpace


object MPICompiler extends CCompiler {

  override def inputView(lambda: Lambda): Unit = {

    println("7. overrided input view by sdh called")

    backends.c.mpi.view.InputView(lambda)


  }

  override def outputView(lambda: Lambda): Unit = {

    println("8. overrided output view by sdh called")

    backends.c.mpi.view.OutputView(lambda)

  }

  override def lowerIR2CAST(lambda: Lambda,
                            memoryDeclaredInSignature: Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)],
                            path: String,
                            files: List[String],
                            func_name: String
                           ): List[SourceFile] = {

    assert(files.length == 1, "There should be exactly one file name passed")

    List(new SourceFile(path, files(0), LowerIR2MPICAST(lambda, memoryDeclaredInSignature) ) )
  }

}
