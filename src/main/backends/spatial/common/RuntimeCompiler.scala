package backends.spatial.common

import _root_.ir.ast.Lambda
import backends.spatial.common.ir.ContextualMemoryCollection
import core.generator.AstPrinter
import core.generator.GenericAST.ExprBlock

object RuntimeCompiler {

  case class CompiledAccelLambda(lambda: Lambda,
                                 generatedBlock: ExprBlock,
                                 intermediateBuffers: ContextualMemoryCollection)

  def apply(lambda: Lambda): String = {

    /*************** Type checks ***************/
//    backends.spatial.host.HostCompiler.typeCheck(lambda)


    /*************** Accelerator blocks compilation ***************/

    val acceleratorLambdas = AcceleratorLambdaCollector(lambda)

    val compiledAcceleratorLambdas = acceleratorLambdas.map(accelLambda => {
      val (block, typedMemoryCollection) = backends.spatial.accel.AccelCompiler(accelLambda)

      CompiledAccelLambda(accelLambda, block,  typedMemoryCollection)
    })

    val printedLambdas = compiledAcceleratorLambdas.map(lambda => AstPrinter(lambda.generatedBlock)())

    /*************** Host code compilation ***************/
    // TODO

    printedLambdas.mkString("")
  }
}
