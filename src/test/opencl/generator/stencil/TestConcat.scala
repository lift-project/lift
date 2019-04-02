package opencl.generator.stencil

import ir.ArrayTypeWSWC
import ir.ast.{ConcatFunction, UserFun, fun}
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._

object TestConcat extends TestWithExecutor


class TestConcat
{

  /** 1D **/
  @Test
  def bringTogetherTwoMapSeq1D(): Unit = {


    val input = Array.fill[Float](2)(1.0f)
    val gold = Array(2.0f,2.0f,4.0f,4.0f)

    // mapseq over array of 1s and multiply by 2
    // mapseq over array of 1s and add 3

    val mult2 = UserFun("mult2", "x", "{ return x*2; }", Float, Float)
    val add3 = UserFun("add3", Array("x"), "{ return x+3; }", Seq(Float), Float)

    val fred = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (input) =>
       ConcatFunction(MapSeq(mult2) $ input, MapSeq(add3) $ input)
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](fred,input)
    assertArrayEquals(gold, output, 0.1f)

  }


}


