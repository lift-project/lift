package backends.spatial.runtime

import core.generator.GenericAST
import ir.ast.Lambda
import opencl.ir.TypedOpenCLMemory

object RuntimeBuilder {

  case class CompiledAccelLambda(lambda: Lambda,
                                 generatedBlock: GenericAST.Block,
                                 intermediateBuffers: TypedOpenCLMemory) // TODO: replace with TypedSpatialMemory

  def apply(lambda: Lambda): String = {

    /*************** Type checks ***************/
//    backends.spatial.host.HostCompiler.typeCheck(lambda)


    /*************** Accelerator blocks compilation ***************/

    val acceleratorLambdas = AcceleratorLambdaCollector(lambda)

    val compiledAcceleratorLambdas = acceleratorLambdas.map(accelLambda => {
      val block = spatial.generator.Compile(accelLambda)
      val intermediateBuffers = CollectTypedSpatialMemory(accelLambda)

      CompiledAccelLambda(accelLambda, block, intermediateBuffers)
    })
//    println(acceleratorLambdas)

    /*************** Host code compilation ***************/

    "" // TODO: return printed code
  }
}
