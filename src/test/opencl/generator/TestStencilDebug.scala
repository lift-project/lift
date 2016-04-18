package opencl.generator

import ir.ast.Pad
import opencl.executor.Executor
import org.junit.{AfterClass, BeforeClass, Test}

object TestStencilDebug {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestStencilDebug extends TestStencil {

  @Test def debugGroupBigClampPaddedData2D() = {
    val neighbours = Array(-2 , -1, 0, 1, 2)
    val data2D = Array.tabulate(10, 10) { (i, j) => i * 10.0f + j }
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(neighbours, boundary, scalaBoundary, data2D)
  }

  @Test def debugGroupMirrorPaddedData2D() = {
    val neighbours = Array(-1, 0, 1)
    val boundary = Pad.Boundary.Mirror
    val scalaBoundary = scalaMirror

    runCombinedPadGroupTest(neighbours, boundary, scalaBoundary)
  }

  @Test def debugGroupWrapPaddedData2D() = {
    val neighbours = Array(-1, 0, 1)
    val boundary = Pad.Boundary.Wrap
    val scalaBoundary = scalaWrap

    runCombinedPadGroupTest(neighbours, boundary, scalaBoundary)
  }
}
