package backends.spatial.generator

import backends.spatial.accel.ir.add
import backends.spatial.accel.ir.pattern.{MapSeq, ReduceSeq}
import backends.spatial.common.ir._
import backends.spatial.host
import backends.{Backend, spatial}
import ir.ast.{Join, Lambda, Split, Value, fun}
import ir.ArrayType
import lift.arithmetic.SizeVar
import org.junit.Assert.assertEquals
import org.junit.{AfterClass, Test}

object ReduceSeqTest {
  @AfterClass def after(): Unit = {
    Backend.setOpenCL()
  }
}

class ReduceSeqTest {

  /**
   * This example tests generating code that reuses a buffer by updating in place
   */
  @Test
  def test(): Unit = {

    Backend.setSpatial()

    val a = SizeVar("a")
    val b = SizeVar("b")

    def layerLambda: Lambda = fun(
      ArrayType(ArrayType(Float, b), a),
      matrix =>
        Join() o
        ReduceSeq(fun((acc, matrix_) =>

            MapSeq(fun(row =>
              MapSeq(fun(Float, el =>
                add(el, Value("3.0f", Float))
              )) $ row
            )) $ matrix_

          ),
          init = matrix
        ) o Split(a) $ matrix
    )

    val runTimeLambda: Lambda = fun(
      ArrayType(ArrayType(Float, b), a), matrix =>
        host.ir.ast.AccelFun(layerLambda)(matrix)
    )

    val generatedSpatial = spatial.common.RuntimeCompiler(runTimeLambda)
    println(generatedSpatial)

    val expectedOutCode =
      """{
        |    def add(x: Float, y: Float): Float = {
        |        x + y
        |    }
        |    // reduce_seq
        |    // iteration count is exactly 1, no loop emitted
        |    val v_i_6 = Reg[Int](0)
        |    // map_seq
        |    Sequential.Foreach(0 until v_a_0 by 1) { (v_i_7) =>
        |        // map_seq
        |        Sequential.Foreach(0 until v_b_1 by 1) { (v_i_8) =>
        |            v__9(v_i_7, v_i_8) = add(v__9(v_i_7, v_i_8), 3.0f)
        |        }
        |        // end map_seq
        |    }
        |    // end map_seq
        |    // end reduce_seq
        |}""".stripMargin

    val cleanedGeneratedSpatial = cleanSpatialProgram(generatedSpatial)
    val cleanedExpectedOutCode = cleanSpatialProgram(expectedOutCode)

    assertEquals(cleanedExpectedOutCode, cleanedGeneratedSpatial)
  }
}
