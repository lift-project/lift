package rewriting

import java.io.DataInputStream
import java.nio.{ByteBuffer, ByteOrder}

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

object TestRewriteMriQ extends TestWithExecutor

class TestRewriteMriQ {

  private def bytesToFloats(bytes: Array[Byte]) = {
    val byteBuffer = ByteBuffer.wrap(bytes)
    val floatBuffer = byteBuffer.asFloatBuffer()
    byteBuffer.order(ByteOrder.LITTLE_ENDIAN)

    val floatArray = Array.ofDim[Float](floatBuffer.capacity())

    for (i <- floatArray.indices) {
      floatArray(i) = byteBuffer.getFloat(i * 4)
    }

    floatArray
  }

  private def floatArrayFromResource(name: String): Array[Float] = {
    val inputStream = this.getClass.getClassLoader.getResourceAsStream(name)
    val availableBytes = inputStream.available()
    val byteArray = Array.ofDim[Byte](availableBytes)

    val dataInputStream = new DataInputStream(inputStream)
    dataInputStream.readFully(byteArray)
    dataInputStream.close()

    bytesToFloats(byteArray)
  }

  private val x = floatArrayFromResource("mriq/xVals.bin")
  private val y = floatArrayFromResource("mriq/yVals.bin")
  private val z = floatArrayFromResource("mriq/zVals.bin")
  private val k = floatArrayFromResource("mriq/kVals.bin")

  private val Qr = floatArrayFromResource("mriq/qrVals.bin")
  private val Qi = floatArrayFromResource("mriq/qiVals.bin")

  private val xNum = x.length
  private val kNum = k.length / 4

  private val gold = (Qr, Qi).zipped.flatMap((a, b) => Seq(a,b))

  private val qFun = UserFun("computeQ",
    Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag", "acc"),
    """{
      |    #define PIx2 6.2831853071795864769252867665590058f
      |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
      |    acc._0 = acc._0 + PhiMag * cos(expArg);
      |    acc._1 = acc._1 + PhiMag * sin(expArg);
      |
      |    return acc;
      |}""".
      stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float, TupleType
    (Float, Float)),
    TupleType(Float, Float))

  private val mapFun = UserFun("mapFun",
    Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag"),
    """{
      |    #define PIx2 6.2831853071795864769252867665590058f
      |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
      |    Tuple2_float_float bla = { PhiMag * cos(expArg), PhiMag * sin(expArg) };
      |    return  bla;
      |}""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float), TupleType(Float, Float))

  private val reduceFun = UserFun("reduceFun",
    Array("x", "y"),
    """{
          | x._0 += y._0;
          | x._1 += y._1;
          | return x;
        }""".stripMargin,
    Seq(TupleType(Float, Float), TupleType(Float, Float)), TupleType(Float, Float))

  private val xSize = SizeVar("X")
  private val kSize = SizeVar("K")

  @Test
  def computeQ(): Unit = {

    // TODO: Break into ReduceSeq o Map? or Map() o Reduce() o Map
    // Similar pattern to kmeans or gemv/nbody


    // TODO: Qr & Qi are outputs initialised to 0.0f
    // TODO: Whole kernel split into 3 invokations??
    val f = fun(
      ArrayType(Float, xSize),
      ArrayType(Float, xSize),
      ArrayType(Float, xSize),
      ArrayType(Float, xSize),
      ArrayType(Float, xSize),
      ArrayType(TupleType(Float, Float, Float, Float), kSize),
      (x, y, z, Qr, Qi, kvalues) =>
        Map(\(t =>
          ReduceSeq(\((acc, p) =>
            qFun(t._0, t._1, t._2, p._0, p._1, p._2, p._3, acc)
          ), Tuple(t._3, t._4)) $ kvalues
        )) $ Zip(x, y, z, Qr, Qi)
    )

    val f0 = Rewrite.applyRuleAtId(f, 0, Rules.addIdForMapParam)
    val f1 = Rewrite.applyRuleAtId(f0, 8, Rules.implementOneLevelOfId)
    val f2 = Rewrite.applyRuleAtId(f1, 21, Rules.implementIdAsDeepCopy)
    val f3 = Rewrite.applyRuleAtId(f2, 18, Rules.implementIdAsDeepCopy)
    val f4 = Rewrite.applyRuleAtId(f3, 15, Rules.implementIdAsDeepCopy)
    val f5 = Rewrite.applyRuleAtId(f4, 12, Rules.dropId)
    val f6 = Rewrite.applyRuleAtId(f5, 9, Rules.dropId)

    val f7 = Rewrite.applyRuleAtId(f6, 24, Rules.tupleToStruct)
    val f8 = Lower.lowerNextLevelWithRule(f7, Rules.mapGlb)
    val f9 = Rewrite.applyRuleAtId(f8, 22, Rules.addIdAfterReduce)
    val f10 = Rewrite.applyRuleAtId(f9, 22, Rules.globalMemory)
    val f11 = Rewrite.applyRuleAtId(f10, 49, Rules.implementIdAsDeepCopy)
    val f12 = Rewrite.applyRuleAtId(f11, 25, Rules.privateMemory)
    val f13 = Rewrite.applyRuleAtId(f12, 19, Rules.privateMemory)
    val f14 = Rewrite.applyRuleAtId(f13, 16, Rules.privateMemory)
    val f15 = Rewrite.applyRuleAtId(f14, 13, Rules.privateMemory)

    val (output: Array[Float], _) =
      Execute()(f15, x, y, z, Array.fill(xNum)(0.0f), Array.fill(xNum)(0.0f), k)

    assertArrayEquals(gold, output, 0.001f)

  }

  @Test
  def mriqIntroduceReuse(): Unit = {
    val f = fun(
      ArrayType(Float, xSize),
      ArrayType(Float, xSize),
      ArrayType(Float, xSize),
      ArrayType(TupleType(Float, Float, Float, Float), kSize),
      (x, y, z, kValues) =>
        Map(\(t =>
          Reduce(reduceFun, Value("{ 0.0f, 0.0f}", TupleType(Float, Float))) o
            Map(\(k => mapFun(t._0, t._1, t._2, k._0, k._1, k._2, k._3))) $ kValues
        )) $ Zip(x, y, z)
    )

    val f0 = Rewrite.applyRuleAtId(f, 0, Rules.splitJoin(64))
    val f1 = Rewrite.applyRuleAtId(f0, 7, Rules.mapFission)
    val f2 = Rewrite.applyRuleAtId(f1, 7, MacroRules.introduceReuseFromMap(64))
    val f3 = Rewrite.applyRuleAtId(f2, 11, MacroRules.introduceReuseFromMap(64))

    val mappings = EnabledMappings(
      global0 = false, global01 = false, global10 = false,
      global012 = false, global210 = false,
      group0 = true, group01 = false, group10 = false)

    val lowered = Lower.mapCombinations(f3, mappings).head

    val (output: Array[Float], _) =
      Execute()(lowered, x, y, z, k)

    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def mriQ2(): Unit = {

    val mapFun = UserFun("mapFun",
      Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag"),
      """{
        |    #define PIx2 6.2831853071795864769252867665590058f
        |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
        |    Tuple2_float_float bla = { PhiMag * cos(expArg), PhiMag * sin(expArg) };
        |    return  bla;
        |}""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float), TupleType(Float, Float))

    val reduceFun = UserFun("reduceFun",
      Array("x", "y"),
      """{
          | x._0 += y._0;
          | x._1 += y._1;
          | return x;
        }""".stripMargin,
      Seq(TupleType(Float, Float), TupleType(Float, Float)), TupleType(Float, Float))

    val computeQ = fun(
      ArrayType(Float, xSize),
      ArrayType(Float, xSize),
      ArrayType(Float, xSize),
      ArrayType(TupleType(Float, Float, Float, Float), kSize),
      (x, y, z, kValues) =>
        MapGlb(\(t =>
          toGlobal(MapSeq(idFF))  o
            ReduceSeq(reduceFun, Value("{ 0.0f, 0.0f}", TupleType(Float, Float))) o
            MapSeq(\(k => mapFun(t._0, t._1, t._2, k._0, k._1, k._2, k._3))) $ kValues
        )) $ Zip(x, y, z)
    )

    val (output: Array[Float], _) =
      Execute()(computeQ, x, y, z, k)

    assertArrayEquals(gold, output, 0.001f)
  }
}
