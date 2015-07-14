package exploration

import arithmetic.Var
import ir._
import ir.UserFunDef._
import opencl.executor._
import opencl.ir.CompositePatterns._
import opencl.ir._
import org.junit.{Test, AfterClass, BeforeClass}
import org.junit.Assert._

object TestDerivingTiling {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }
}

class TestDerivingTiling {

  @Test
  def mapsOnly(): Unit = {
    val nSize = 16
    val mSize = 16
    val tileSizeM = 4
    val tileSizeN = 2

    val matrix = Array.fill(nSize, mSize)(util.Random.nextInt(nSize*mSize).toFloat)
    val gold = matrix.flatMap(_.map(_ + 1))

    val N = Var("N")
    val M = Var("M")

    // Starting expression
    def f = fun(
      ArrayType(ArrayType(Float, M), N),
      input => MapGlb(MapSeq(plusOne)) $ input
    )

    val (output: Array[Float], _) = Execute(1, nSize)(f, matrix)
    assertArrayEquals(gold, output, 0.0f)

    // split-join
    def f1 = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Join() o MapGlb(MapSeq(MapSeq(plusOne))) o Split(tileSizeN) $ input
    )

    val (output1: Array[Float], _) = Execute(1, nSize)(f1, matrix)
    assertArrayEquals(gold, output1, 0.0f)

    // Transpose both sides
    def f2 = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Join() o MapGlb(TransposeW() o MapSeq(MapSeq(plusOne)) o Transpose()) o Split(tileSizeN) $ input
    )

    val (output2: Array[Float], _) = Execute(1, nSize)(f2, matrix)
    assertArrayEquals(gold, output2, 0.0f)

    // split-join
    def f3 = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Join() o MapGlb(TransposeW() o Join() o MapSeq(MapSeq(MapSeq(plusOne))) o Split(tileSizeM) o Transpose()) o Split(tileSizeN) $ input
    )

    val (output3: Array[Float], _) = Execute(1, nSize)(f3, matrix)
    assertArrayEquals(gold, output3, 0.0f)

    // Transpose both sides
    def f4 = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Join() o MapGlb(TransposeW() o Join() o MapSeq(TransposeW() o MapSeq(MapSeq(plusOne)) o Transpose()) o Split(tileSizeM) o Transpose()) o Split(tileSizeN) $ input
    )

    val (output4: Array[Float], _) = Execute(1, nSize)(f4, matrix)
    assertArrayEquals(gold, output4, 0.0f)

    // Map fission, pull out splits, joins and transposes
    def f5 = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Join() o Map(TransposeW() o Join() o Map(TransposeW())) o
        MapGlb(MapSeq(MapSeq(MapSeq(plusOne))))
        o Map(Map(Transpose()) o Split(tileSizeM) o Transpose()) o Split(tileSizeN) $ input
    )

    val (output5: Array[Float], _) = Execute(1, nSize)(f5, matrix)
    assertArrayEquals(gold, output5, 0.0f)

    // Replace with predefined
    def f6 = fun(
      ArrayType(ArrayType(Float, M), N),
      input => Untile() o
        MapGlb(MapSeq(MapSeq(MapSeq(plusOne))))
        o Tile(tileSizeN, tileSizeM) $ input
    )

    val (output6: Array[Float], _) = Execute(1, nSize)(f6, matrix)
    assertArrayEquals(gold, output6, 0.0f)
  }
}
