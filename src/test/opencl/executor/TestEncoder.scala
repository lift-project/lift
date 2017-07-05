package opencl.executor

import java.nio.{ByteBuffer, ByteOrder}

import ir.{ArrayType, ArrayTypeWC, ScalarType, Type}
import opencl.ir.{Bool, Double, Float, Int}
import org.junit.Assert.assertArrayEquals
import org.junit.{AfterClass, BeforeClass, Test}

import scala.reflect.ClassTag

class TestEncoder {
  import TestEncoder._

  @Test
  def encodeFull1D(): Unit = {
    val size = 16
    val tyCon = (st: ScalarType) => ArrayType(st, size)

    val (bArray, iArray, fArray, dArray) = get1DData(size)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, size * _)

    assertBufferEquals(toBuffer(fArray), fEncoder.encode(fArray))
    assertBufferEquals(toBuffer(iArray), iEncoder.encode(iArray))
    assertBufferEquals(toBuffer(dArray), dEncoder.encode(dArray))
    assertBufferEquals(toBuffer(bArray), bEncoder.encode(bArray))
  }

  @Test
  def encodeRagged1D(): Unit = {
    val capacity = 32
    val size = 17
    val allocatedSize = (baseSize: Int) => sizeOfInt + capacity * baseSize
    val tyCon = (st: ScalarType) => ArrayTypeWC(st, capacity)

    val (bArray, iArray, fArray, dArray) = get1DData(size)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocatedSize)

    def gold[T](array: Array[T], baseSize: Int): ByteBuffer = {
      val buffer = mkBuffer(allocatedSize(baseSize))
      // Header: just the size
      buffer.asIntBuffer().put(size)
      buffer.position(sizeOfInt)
      // Data: raw array
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
    val allocSize = (baseSize: Int) => sizeX * sizeY * baseSize
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
    val allocSize = (baseSize: Int) => sizeOfInt + capX * (sizeOfInt + capY * baseSize)
    val tyCon = (st: ScalarType) => ArrayTypeWC(ArrayTypeWC(st, capY), capX)

    val (bArray, iArray, fArray, dArray) = get2DRaggedData(sizeX, (1, capY))
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocSize)

    def gold[T: ClassTag](array2D: Array[Array[T]], baseSize: Int): ByteBuffer = {
      val buffer = mkBuffer(allocSize(baseSize))

      // Header: just the size
      buffer.asIntBuffer().put(sizeX)
      buffer.position(sizeOfInt)

      // Body: a flattened version of the 2D array padded with zeros
      array2D.foreach {
        (arr: Array[T]) =>
          val start = buffer.position()
          buffer.asIntBuffer().put(arr.length)
          buffer.position(start + sizeOfInt)
          val encodedRow = toBuffer(arr)
          encodedRow.position(0)
          buffer.put(encodedRow)
          buffer.position(start + sizeOfInt + capY * baseSize)
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
    val allocSize = (baseSize: Int) =>
      sizeOfInt * (1 + capX) + capX * (2 * sizeOfInt + yBounds._2 * baseSize)

    val (bArray, iArray, fArray, dArray) = get2DRaggedData(sizeX, yBounds)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocSize)

    def gold[T: ClassTag](array2D: Array[Array[T]], baseSize: Int): ByteBuffer = {
      val buffer = mkBuffer(allocSize(baseSize))

      // Header: just the size
      buffer.asIntBuffer().put(sizeX)
      // Offsets: `capX` integer values storing the offset in bytes between the
      //          beginning of the outer array and the beginning of each inner
      //          array.
      var ofs = sizeOfInt * (1 + capX)
      array2D.map(_.length).zipWithIndex.foreach {
        case (len, idx) =>
          buffer.position((1 + idx) * sizeOfInt)
          buffer.asIntBuffer().put(ofs)
          ofs += len * baseSize + 2 * sizeOfInt
      }
      buffer.position((1 + capX) * sizeOfInt)

      // A flattened version of the 2D array with *NO* padding.
      array2D.foreach {
        (arr: Array[T]) =>
          val start = buffer.position()
          // Header
          buffer.asIntBuffer().put(Array(arr.length, arr.length))
          buffer.position(start + 2 * sizeOfInt)
          // Content
          val encodedRow = toBuffer(arr)
          encodedRow.position(0)
          buffer.put(encodedRow)
          buffer.position(start + 2 * sizeOfInt + arr.length * baseSize)
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
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }

  lazy val endianness: ByteOrder = if (Executor.isLittleEndian) ByteOrder.LITTLE_ENDIAN else ByteOrder.BIG_ENDIAN
  lazy val sizeOfInt: Int = Type.getAllocatedSize(Int).evalInt

  /** Instantiate 4 encoders for the 4 supported scalar types */
  def getEncoders(tyCon: ScalarType => ArrayType, allocSize: Int => Int): (Encoder, Encoder, Encoder, Encoder) = (
    new Encoder(tyCon(Bool), allocSize(1)),
    new Encoder(tyCon(Int), allocSize(4)),
    new Encoder(tyCon(Float), allocSize(4)),
    new Encoder(tyCon(Double), allocSize(8))
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

  def toBuffer(array: Array[_]): ByteBuffer = {
    val buffer = mkBuffer(array.length * sizeOf(array.head))
    array match {
      case ab: Array[Boolean] => buffer.put(ab.map(b => (if (b) 1 else 0).toByte))
      case ai: Array[Int] => buffer.asIntBuffer().put(ai)
      case af: Array[Float] => buffer.asFloatBuffer().put(af)
      case ad: Array[Double] => buffer.asDoubleBuffer().put(ad)
    }
    buffer
  }

  /** Shorthand */
  def mkBuffer(size: Int): ByteBuffer = {
    val buffer = ByteBuffer.allocate(size)
    buffer.order(endianness)
    buffer
  }

  def assertBufferEquals(leftBuf: ByteBuffer, rightBuf: ByteBuffer): Unit = {
    assertArrayEquals(leftBuf.array, rightBuf.array)
  }
}
