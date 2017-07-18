package opencl.executor

import java.nio.{ByteBuffer, ByteOrder}

import ir._
import opencl.generator.AlignArrays
import opencl.ir.{Bool, Double, Float, Int}
import org.junit.Assert.assertArrayEquals
import org.junit.{AfterClass, BeforeClass, Test}

import scala.reflect.ClassTag

class TestEncoder {
  import TestEncoder._

  @Test
  def encodeTuple(): Unit = {
    val buf = mkBuffer(12)
    buf.put(0, 1.toByte)
    buf.putInt(4, 42)
    buf.putFloat(8, 13.13f)

    val encoder = new Encoder(TupleType(Bool, TupleType(Int, Float)), 12)
    assertBufferEquals(buf, encoder.encode((true, (42, 13.13f))))
  }

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

    def gold[T](array: Vector[T], baseSize: Int): ByteBuffer = {
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

    def gold[T: ClassTag](array2D: Vector[Vector[T]], baseSize: Int): ByteBuffer = {
      val buffer = mkBuffer(allocSize(baseSize))

      // Header: just the size
      buffer.asIntBuffer().put(sizeX)
      buffer.position(sizeOfInt)

      // Body: a flattened version of the 2D array padded with zeros
      array2D.foreach {
        (arr: Vector[T]) =>
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

    def gold[T: ClassTag](array2D: Vector[Vector[T]], baseSize: Int): ByteBuffer = {
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
        (arr: Vector[T]) =>
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

  @Test
  def encode4D(): Unit = {
    val ty = ArrayType(ArrayType(ArrayType(ArrayType(Int), 2), 2), 3)
    val array =
      Vector(
        Vector(
          Vector(
            Vector(1, 2, 3), Vector(4, 5)
          ),
          Vector(
            Vector(6, 7), Vector(8, 9, 10, 11)
          )
        ),
        Vector(
          Vector(
            Vector(12), Vector(13, 14, 15)
          ),
          Vector(
            Vector(16, 17, 18, 19), Vector(20, 21)
          )
        ),
        Vector(
          Vector(
            Vector(22), Vector(23, 24)
          ),
          Vector(
            Vector(25, 26), Vector(27, 28, 29, 30, 31)
          )
        )
      )
    val gold = Vector(
      3 * 4, 28 * 4, 52* 4,
        // Element 0
        2 * 4, 13 * 4,
          // Element 0,0
          2 * 4, 7 * 4,
          3, 3, 1, 2, 3,   2, 2, 4, 5,
          // Element 0,1
          2 * 4, 6 * 4,
          2, 2, 6, 7,   4, 4, 8, 9, 10, 11,
        // Element 1
        2 * 4, 12 * 4,
          // Element 1,0
          2 * 4, 5 * 4,
          1, 1, 12,   3, 3, 13, 14, 15,
          // Element 1,1
          2 * 4, 8 * 4,
          4, 4, 16, 17, 18, 19,   2, 2, 20, 21,
        // Element 2
        2 * 4, 11 * 4,
          // Element 2,0
          2 * 4, 5 * 4,
          1, 1, 22,   2, 2, 23, 24,
          // Element 2,1
          2 * 4, 6 * 4,
          2, 2, 25, 26,   5, 5, 27, 28, 29, 30, 31
    )

    val encoder = new Encoder(ty, gold.length * 4)
    assertBufferEquals(toBuffer(gold), encoder.encode(array))
  }
}

object TestEncoder {
  private var alignArrays: Boolean = _

  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
    alignArrays = AlignArrays()
    AlignArrays(false)
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
    AlignArrays(alignArrays)
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

  def get1DData(size: Int): (Vector[Boolean], Vector[Int], Vector[Float], Vector[Double]) = (
    Vector.fill(size)(util.Random.nextBoolean()),
    Vector.fill(size)(util.Random.nextInt()),
    Vector.fill(size)(util.Random.nextFloat()),
    Vector.fill(size)(util.Random.nextDouble())
  )

  def get2DData(sizeX: Int, sizeY: Int): (Vector[Vector[Boolean]], Vector[Vector[Int]],
                                           Vector[Vector[Float]], Vector[Vector[Double]]) = (
    Vector.fill(sizeX, sizeY)(util.Random.nextBoolean()),
    Vector.fill(sizeX, sizeY)(util.Random.nextInt()),
    Vector.fill(sizeX, sizeY)(util.Random.nextFloat()),
    Vector.fill(sizeX, sizeY)(util.Random.nextDouble())
  )

  def get2DRaggedData(sizeX: Int, yBounds: (Int, Int)): (Vector[Vector[Boolean]], Vector[Vector[Int]],
                                                         Vector[Vector[Float]], Vector[Vector[Double]]) = {
    val (minY, maxY) = yBounds
    assert(minY <= maxY)  // Sanity check
    val sizesY = Vector.fill(sizeX)(minY + util.Random.nextInt(maxY - minY))
    (
      sizesY.map(Vector.fill(_)(util.Random.nextBoolean())),
      sizesY.map(Vector.fill(_)(util.Random.nextInt())),
      sizesY.map(Vector.fill(_)(util.Random.nextFloat())),
      sizesY.map(Vector.fill(_)(util.Random.nextDouble()))
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

  def toBuffer(vector: Vector[_]): ByteBuffer = {
    val buffer = mkBuffer(vector.length * sizeOf(vector.head))
    vector.head match {
      case _: Boolean =>
        val array = vector.asInstanceOf[Vector[Boolean]].toArray
        buffer.put(array.map(b => (if (b) 1 else 0).toByte))
      case _: Int =>
        val array = vector.asInstanceOf[Vector[Int]].toArray
        buffer.asIntBuffer().put(array)
      case _: Float =>
        val array = vector.asInstanceOf[Vector[Float]].toArray
        buffer.asFloatBuffer().put(array)
      case _: Double =>
        val array = vector.asInstanceOf[Vector[Double]].toArray
        buffer.asDoubleBuffer().put(array)
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
