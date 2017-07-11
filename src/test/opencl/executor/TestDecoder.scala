package opencl.executor

import java.nio.{ByteBuffer, ByteOrder}

import ir._
import lift.arithmetic.ArithExpr
import lift.arithmetic.ArithExpr.Math.Max
import opencl.generator.AlignArrays
import opencl.ir.{Bool, Double, Float, Float4, Int}
import org.junit.Assert.{assertArrayEquals, assertEquals}
import org.junit.{AfterClass, BeforeClass, Test}

class TestDecoder {
  import TestDecoder._

  @Test
  def scalars(): Unit = {
    val int = mkBuffer(Int.size)
    int.putInt(42)
    assertEquals(42, Decoder.decode[Int](Int, int))

    val float = mkBuffer(Float.size)
    float.putFloat(42f)
    assertEquals(42f, Decoder.decode[Float](Float, float), 0f)

    val double = mkBuffer(Double.size)
    double.putDouble(42d)
    assertEquals(42d, Decoder.decode[Double](Double, double), 0d)

    val bTrue = mkBuffer(1)
    val bFalse = mkBuffer(1)
    bTrue.put(1.toByte)
    bFalse.put(0.toByte)
    assertEquals(true, Decoder.decode[Boolean](Bool, bTrue))
    assertEquals(false, Decoder.decode[Boolean](Bool, bFalse))
  }

  @Test
  def tuples(): Unit = {
    val tt1 = TupleType(Int, Double)
    val buf1 = mkBuffer(2 * max(Int.size, Double.size))
    buf1.putInt(0, 42)
    buf1.putDouble(8, 12345.6789)
    assertEquals((42, 12345.6789d), Decoder.decode[(Int, Double)](tt1, buf1))

    val tt2 = TupleType(Float, TupleType(Bool, Int))
    val buf2 = mkBuffer(3 * max(Float.size, Bool.size, Int.size))
    buf2.putFloat(0, 42.42f)
    buf2.put(4, 1.toByte)
    buf2.putInt(8, 989898)
    assertEquals((42.42f, (true, 989898)), Decoder.decode[(Float, (Boolean, Int))](tt2, buf2))

    val tt3 = TupleType(Bool, Float4)
    val buf3 = mkBuffer(5 * max(Bool.size, Type.getAllocatedSize(Float)))
    buf3.put(0, 0.toByte)
    buf3.position(4)
    buf3.asFloatBuffer().put(Array(.1f, .2f, .3f, .4f))
    assertEquals((false, Vector(.1f, .2f, .3f, .4f)), Decoder.decode[(Boolean, Vector[Float])](tt3, buf3))
  }

  @Test
  def arrayWithHeader(): Unit = {
    val data = Array(42d, 77d, 0.43d)
    val ty = ArrayTypeWC(Double, 10) // The capacity (can be anything ≥ 3)

    val buffer = mkBuffer(10 * Double.size + Int.size)
    buffer.putInt(3)
    buffer.asDoubleBuffer().put(data)

    assertArrayEquals(data, Decoder.decode[Vector[Double]](ty, buffer).toArray, 0d)
  }

  @Test
  def array2DWithHeaders(): Unit = {
    val array2D = Array(Array(23.5d, .8d), Array(0d, 1d, 3d, 4d, 5d), Array(99.3d, 42d, 8d))
    val ty = ArrayTypeWC(ArrayTypeWC(Double, 7), 5)
    val buf = mkBuffer(5 * (7 * Double.size.eval + Int.size.eval) + Int.size.eval)

    buf.putInt(array2D.length)
    for (arr <- array2D) {
      val before = buf.position()
      buf.putInt(arr.length)
      buf.asDoubleBuffer().put(arr)
      buf.position(before + 7 * Double.size.eval + Int.size.eval)
    }

    assertEquals(
      array2D.toVector.map(_.toVector),
      Decoder.decode[Vector[Vector[Double]]](ty, buf)
    )
  }

  @Test
  def array2DWithOffsets(): Unit = {
    val array2D = Array(Array(23.5d, .8d), Array(0d, 1d, 3d, 4d, 5d), Array(99.3d, 42d, 8d))
    val ty = ArrayTypeWC(ArrayType(Double), 4)
    val buf = mkBuffer(
      Int.size.eval * (1 + 4) // Header + offsets
      + array2D.map(_.length * Double.size.eval + 2 * Int.size.eval).sum // Nested arrays
    )

    // Header and offsets
    buf.putInt(array2D.length)
    var offset = Int.size.eval * (1 + 4)
    for (arr <- array2D) {
      buf.putInt(offset)
      offset += (arr.length * Double.size + 2 * Int.size).eval
    }
    buf.position((1 + 4) * Int.size.eval)

    for (arr <- array2D) {
      val before = buf.position()
      buf.putInt(arr.length)
      buf.putInt(arr.length)
      buf.asDoubleBuffer().put(arr)
      buf.position(before + (2 * Int.size + arr.length * Double.size).eval)
    }

    assertEquals(
      array2D.toVector.map(_.toVector),
      Decoder.decode[Vector[Vector[Double]]](ty, buf)
    )
  }
}

object TestDecoder {
  private var alignArrays: Boolean = _
  val endianness = ByteOrder.LITTLE_ENDIAN

  @BeforeClass
  def before(): Unit = {
    alignArrays = AlignArrays()
    AlignArrays(false)
  }

  @AfterClass
  def after(): Unit = {
    AlignArrays(alignArrays)
  }

  def max(exprs: ArithExpr*): ArithExpr = exprs.reduce(Max)

  /**
   * Shorthand to create a new buffer with the correct endianness
   * @param size an arith expr (for convenience) storing the size to allocate in bytes.
   */
  def mkBuffer(size: ArithExpr): ByteBuffer = {
    val buffer = ByteBuffer.allocate(size.eval)
    buffer.order(endianness)
    buffer
  }
}
