package backends.spatial.common

import _root_.ir.ast.Lambda
import backends.spatial.common.ir.TypedMemoryCollection
import core.generator.AstPrinter
import core.generator.GenericAST.ExprBlock

object RuntimeCompiler {

  case class CompiledAccelLambda(lambda: Lambda,
                                 generatedBlock: ExprBlock,
                                 intermediateBuffers: TypedMemoryCollection)

  def apply(lambda: Lambda): String = {

    /*************** Type checks ***************/
//    backends.spatial.host.HostCompiler.typeCheck(lambda)


    /*************** Accelerator blocks compilation ***************/

    val acceleratorLambdas = AcceleratorLambdaCollector(lambda)

    val compiledAcceleratorLambdas = acceleratorLambdas.map(accelLambda => {
      val (block, typedMemoryCollection) = backends.spatial.accel.AccelCompiler(accelLambda)

      CompiledAccelLambda(accelLambda, block,  typedMemoryCollection)
    })

    println(compiledAcceleratorLambdas.map(lambda => AstPrinter(lambda.generatedBlock)()))

    /*************** Host code compilation ***************/

    "" // TODO: return printed code
  }
}
