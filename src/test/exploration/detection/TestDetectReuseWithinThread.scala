package exploration.detection

import exploration.detection.DetectReuseWithinThread._
import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

class TestDetectReuseWithinThread {

  @Test
  def gesummvIntroduceReuse(): Unit = {
    val v_K_0 = Var("K", StartFromRange(1))
    val v_N_1 = Var("N", StartFromRange(1))
    val v__2 = Var("", StartFromRange(1))
    val v__3 = Var("", StartFromRange(1))

    val idTuple2_float_float = UserFun("idTuple2_float_float", Array("x"), """|{ return x; }""".stripMargin, Seq(TupleType(Float, Float)), TupleType(Float, Float))
    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    val mult = UserFun("mult", Array("l", "r"), """|{ return l * r; }""".stripMargin, Seq(Float, Float), Float)
    val f = fun(ArrayType(ArrayType(Float, v_K_0), v_N_1), ArrayType(ArrayType(Float, v_K_0), v_N_1), ArrayType(Float, v_K_0), Float, Float,(p_0, p_1, p_2, p_3, p_4) => FunCall(Join(), FunCall(Join(), FunCall(MapWrg(0)(fun((p_5) => FunCall(TransposeW(), FunCall(MapSeq(fun((p_6) => FunCall(toGlobal(fun((p_7) => FunCall(MapLcl(0)(fun((p_8) => FunCall(add, FunCall(mult, FunCall(Get(0), p_8), p_3), FunCall(mult, FunCall(Get(1), p_8), p_4)))), p_7))), p_6))), FunCall(ReduceSeq(fun((p_9, p_10) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_11) => FunCall(ReduceSeq(fun((p_12, p_13) => FunCall(idTuple2_float_float, FunCall(Tuple(2), FunCall(add, FunCall(Get(0), p_12), FunCall(mult, FunCall(Get(0), p_13), FunCall(Get(1), p_13))), FunCall(add, FunCall(Get(1), p_12), FunCall(mult, FunCall(Get(2), p_13), FunCall(Get(1), p_13))))))), FunCall(Get(0), p_11), FunCall(Zip(3), FunCall(Get(1), p_11), FunCall(Get(1), p_10), FunCall(Get(2), p_11))))), FunCall(Zip(3), p_9, FunCall(Get(0), p_10), FunCall(Get(2), p_10)))))), FunCall(MapLcl(0)(fun((p_14) => FunCall(idTuple2_float_float, p_14))), Value("{ 0.0f, 0.0f }", ArrayType(TupleType(Float, Float), v__2))), FunCall(Zip(3), FunCall(Transpose(), FunCall(Map(fun((p_15) => FunCall(Split(v__3), FunCall(Get(0), p_15)))), p_5)), FunCall(Split(v__3), p_2), FunCall(Transpose(), FunCall(Map(fun((p_16) => FunCall(Split(v__3), FunCall(Get(1), p_16)))), p_5)))))))), FunCall(Split(v__2), FunCall(Zip(2), p_0, p_1))))))

    val locations = findStrategicLocations(f)
    assertEquals(0, locations.length)
  }

  @Test
  def partialReduceWithReorderNoRace(): Unit = {
    val locations = findStrategicLocations(gemvPartialReduceWithReorderNoRace)
    assertEquals(0, locations.length)
  }

  @Test
  def introduceReuse(): Unit = {
    val locations = findStrategicLocations(gemvIntroduceReuse)
    assertEquals(0, locations.length)
  }

  @Test
  def oneDRegBlock(): Unit = {
    val locations = findStrategicLocations(mm1DBlocked)
    assertEquals(1, locations.length)
  }

  @Test
  def twoDRegBlock(): Unit = {
    val locations = findStrategicLocations(mm2DBlocked)
    assertEquals(4, locations.length)
  }

  @Test
  def plainTiling(): Unit = {
    val locations = findStrategicLocations(mmTiled)
    assertEquals(0, locations.length)
  }

  @Test
  def tilingAndOneDRegBlock(): Unit = {
    val locations = findStrategicLocations(mmTiled1DBlocked)
    assertEquals(1, locations.length)
  }

  @Test
  def tilingAndTwoDRegBlock(): Unit = {
    val locations = findStrategicLocations(mmTiled2DBlocked)
    assertEquals(4, locations.length)
  }
}
