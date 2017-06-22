package opencl.executor

import java.nio.{ByteBuffer, ByteOrder}

import ir.{ArrayType, ArrayTypeWC, ScalarType, Type}
import opencl.ir.{Bool, Double, Float, Int}
import org.junit.Assert.assertArrayEquals
import org.junit.Test

import scala.reflect.ClassTag

class TestEncoder {
  import TestEncoder._

  @Test
  def encodeFull1D(): Unit = {
    val size = 16
    val tyCon = (st: ScalarType) => ArrayType(st, size)

    val (bArray, iArray, fArray, dArray) = get1DData(size)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, size)

    assertBufferEquals(toBuffer(fArray), fEncoder.encode(fArray))
    assertBufferEquals(toBuffer(iArray), iEncoder.encode(iArray))
    assertBufferEquals(toBuffer(dArray), dEncoder.encode(dArray))
    assertBufferEquals(toBuffer(bArray), bEncoder.encode(bArray))
  }

  @Test
  def encodeRagged1D(): Unit = {
    val capacity = 32
    val size = 17
    val allocatedSize = capacity + 1
    val tyCon = (st: ScalarType) => ArrayTypeWC(st, capacity)

    val (bArray, iArray, fArray, dArray) = get1DData(size)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocatedSize)

    def gold[T](array: Array[T], baseSize: Int): ByteBuffer = {
      val buffer = ByteBuffer.allocate(allocatedSize * baseSize)
      // Header
      buffer.order(endianness)
      putInt(size, baseSize, buffer)
      buffer.position(baseSize)
      // Data
      val data = toBuffer(array)
      data.position(0)
      buffer.put(data)
    }

    assertBufferEquals(gold(fArray, 4), fEncoder.encode(fArray))
    assertBufferEquals(gold(iArray, 4), iEncoder.encode(iArray))
    assertBufferEquals(gold(dArray, 8), dEncoder.encode(dArray))
    assertBufferEquals(gold(bArray, 1), bEncoder.encode(bArray))
  }

  @Test
  def encodeFull2D(): Unit = {
    val (sizeX, sizeY) = (3, 7)
    val allocSize = sizeX * sizeY
    val tyCon = (st: ScalarType) => ArrayType(ArrayType(st, sizeY), sizeX)

    val (bArray, iArray, fArray, dArray) = get2DData(sizeX, sizeY)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocSize)

    assertBufferEquals(toBuffer(fArray.flatten), fEncoder.encode(fArray))
    assertBufferEquals(toBuffer(iArray.flatten), iEncoder.encode(iArray))
    assertBufferEquals(toBuffer(dArray.flatten), dEncoder.encode(dArray))
    assertBufferEquals(toBuffer(bArray.flatten), bEncoder.encode(bArray))
  }

  @Test
  def encodeRagged2DPadding(): Unit = {
    val capX = 16
    val capY = 8
    val sizeX = 13
    val allocSize = 1 + capX * (1 + capY)
    val tyCon = (st: ScalarType) => ArrayTypeWC(ArrayTypeWC(st, capY), capX)

    val (bArray, iArray, fArray, dArray) = get2DRaggedData(sizeX, (1, capY))
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocSize)

    def gold[T: ClassTag](array2D: Array[Array[T]], baseSize: Int): ByteBuffer = {
      val buffer = ByteBuffer.allocate(allocSize * baseSize)
      buffer.order(endianness)

      putInt(sizeX, baseSize, buffer)
      buffer.position(baseSize)

      array2D.foreach {
        (arr: Array[T]) =>
          val start = buffer.position()
          putInt(arr.length, baseSize, buffer)
          buffer.position(start + baseSize)
          val encodedRow = toBuffer(arr)
          encodedRow.position(0)
          buffer.put(encodedRow)
          buffer.position(start + (1 + capY) * baseSize)
      }

      buffer
    }

    assertBufferEquals(gold(fArray, 4), fEncoder.encode(fArray))
    assertBufferEquals(gold(iArray, 4), iEncoder.encode(iArray))
    assertBufferEquals(gold(dArray, 8), dEncoder.encode(dArray))
    assertBufferEquals(gold(bArray, 1), bEncoder.encode(bArray))
  }

  @Test
  def encodeRagged2DOffsets(): Unit = {
    val capX = 8
    val sizeX = 5
    val yBounds = (1, 32)
    val tyCon = (st: ScalarType) => ArrayTypeWC(ArrayType(st), capX)
    // This allocated size is chosen based on our knowledge of the data, this
    // information cannot be retrieved from the type.
    // See the ScalaDoc comment at the top of `OpenCLMemoryAllocator.scala`
    val allocSize = 1 + capX + capX * (2 + yBounds._2)

    val (bArray, iArray, fArray, dArray) = get2DRaggedData(sizeX, yBounds)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocSize)

    def gold[T: ClassTag](array2D: Array[Array[T]], baseSize: Int): ByteBuffer = {
      val buffer = ByteBuffer.allocate(allocSize * baseSize)
      buffer.order(endianness)

      putInt(sizeX, baseSize, buffer)
      array2D.map(_.length + 2).scan(capX)(_ + _).slice(0, array2D.length).zipWithIndex.foreach {
        case (ofs, idx) =>
          buffer.position((1 + idx) * baseSize)
          putInt(ofs, baseSize, buffer)
      }
      buffer.position((1 + capX) * baseSize)

      array2D.foreach {
        (arr: Array[T]) =>
          val start = buffer.position()
          // Header
          putInt(arr.length, baseSize, buffer)
          buffer.position(start + baseSize)
          putInt(arr.length, baseSize, buffer)
          buffer.position(start + 2 * baseSize)
          // Content
          val encodedRow = toBuffer(arr)
          encodedRow.position(0)
          buffer.put(encodedRow)
          buffer.position(start + (2 + arr.length) * baseSize)
      }

      buffer
    }

    assertBufferEquals(gold(fArray, 4), fEncoder.encode(fArray))
    assertBufferEquals(gold(iArray, 4), iEncoder.encode(iArray))
    assertBufferEquals(gold(dArray, 8), dEncoder.encode(dArray))
    assertBufferEquals(gold(bArray, 1), bEncoder.encode(bArray))
  }
}

object TestEncoder {
  val endianness = ByteOrder.LITTLE_ENDIAN

  /** Instantiate 4 encoders for the 4 supported scalar types */
  def getEncoders(tyCon: ScalarType => ArrayType, allocSize: Int): (Encoder, Encoder, Encoder, Encoder) = (
    new Encoder(tyCon(Bool), allocSize * 1),
    new Encoder(tyCon(Int), allocSize * 4),
    new Encoder(tyCon(Float), allocSize * 4),
    new Encoder(tyCon(Double), allocSize * 8)
  )

  // ---
  // Random data generators
  // ---

  def get1DData(size: Int): (Array[Boolean], Array[Int], Array[Float], Array[Double]) = (
    Array.fill(size)(util.Random.nextBoolean()),
    Array.fill(size)(util.Random.nextInt()),
    Array.fill(size)(util.Random.nextFloat()),
    Array.fill(size)(util.Random.nextDouble())
  )

  def get2DData(sizeX: Int, sizeY: Int): (Array[Array[Boolean]], Array[Array[Int]],
                                           Array[Array[Float]], Array[Array[Double]]) = (
    Array.fill(sizeX, sizeY)(util.Random.nextBoolean()),
    Array.fill(sizeX, sizeY)(util.Random.nextInt()),
    Array.fill(sizeX, sizeY)(util.Random.nextFloat()),
    Array.fill(sizeX, sizeY)(util.Random.nextDouble())
  )

  def get2DRaggedData(sizeX: Int, yBounds: (Int, Int)): (Array[Array[Boolean]], Array[Array[Int]],
                                                         Array[Array[Float]], Array[Array[Double]]) = {
    val (minY, maxY) = yBounds
    assert(minY <= maxY)  // Sanity check
    val sizesY = Array.fill(sizeX)(minY + util.Random.nextInt(maxY - minY))
    (
      sizesY.map(Array.fill(_)(util.Random.nextBoolean())),
      sizesY.map(Array.fill(_)(util.Random.nextInt())),
      sizesY.map(Array.fill(_)(util.Random.nextFloat())),
      sizesY.map(Array.fill(_)(util.Random.nextDouble()))
    )
  }

  // ---
  // Some helper functions for working with buffers
  // ---

  def sizeOf(x: Any): Int = x match {
    case _: Boolean => 1
    case _: Int => 4
    case _: Float => 4
    case _: Double => 8
  }

  def putInt(n: Int, baseSize: Int, buffer: ByteBuffer): Unit = {
    baseSize match {
      case 1 => buffer.put(n.toByte)
      case 4 => buffer.asIntBuffer().put(n)
      case 8 => buffer.asLongBuffer().put(n.toLong)
    }
  }

  def toBuffer(array: Array[_]): ByteBuffer = {
    val buffer = ByteBuffer.allocate(array.length * sizeOf(array.head))
    buffer.order(endianness)
    buffer.position(0)
    array match {
      case ab: Array[Boolean] => buffer.put(ab.map(b => (if (b) 1 else 0).toByte))
      case ai: Array[Int] => buffer.asIntBuffer().put(ai)
      case af: Array[Float] => buffer.asFloatBuffer().put(af)
      case ad: Array[Double] => buffer.asDoubleBuffer().put(ad)
    }
    buffer
  }

  def assertBufferEquals(leftBuf: ByteBuffer, rightBuf: ByteBuffer): Unit = {
    assertArrayEquals(leftBuf.array, rightBuf.array)
  }
}
