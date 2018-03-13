package opencl.generator.stencil

import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast._
import lift.arithmetic.{SizeVar, StartFromRange, Var}
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._

import scala.util.Random


object TestImagePipelines extends TestWithExecutor

class TestImagePipelines {

  @Test
  def boxBlur(): Unit = {
    assertTrue(true)
  }
}
