package backends.spatial.global

import ir.ast.Lambda

object RuntimeCompiler {
  def apply(lambda: Lambda): String = {
    // Compile Accel

    val accelLambdas = AcceleratableLambdaCollector(lambda)

    println(accelLambdas)

    // Compile Host code

    "" // TODO: return printed code
  }
}
