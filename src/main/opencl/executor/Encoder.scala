package opencl.executor

import java.nio.{ByteBuffer, ByteOrder}

import ir._
import opencl.generator.AlignArrays
import opencl.ir._

/**
 * A tool to encode scala arrays into byte buffers.
 * One instance of this class can encode several arrays but they must all match
 * the type given in the constructor *and* fit into the specified allocated.
 *
 * The reason why this allocated space cannot always be retrieved from the type
 * is explained in the ScalaDoc comment at the top of `OpenCLMemoryAllocator.scala`
 *
 * @param arrayType Lift type of the values to be encoded
 * @param sizeof Allocated size in bytes
 */
class Encoder(arrayType: ArrayType, sizeof: Int) {
  /**
   * Encode an array that matches the type (and allocated size) of the
   * `Encoder` instance into a byte buffer.
   */
  def encode(array: Array[_]): ByteBuffer = {
    val buffer = ByteBuffer.allocate(sizeof)
    buffer.position(0)
    buffer.order(endianness)
    putArray(arrayType, array, buffer)
    buffer
  }

  /**
   * Main encoding function. Recursively encode the array `array` of type `ty`
   * and write it into `buffer` at its current position.
   */
  private def putArray(ty: ArrayType, array: Array[_], buffer: ByteBuffer): Unit = {
    val beforeHeader = buffer.position()
    val afterHeader = beforeHeader + ty.headerSize * sizeOfInt

    // Header of the array
    val header = Array.fill(ty.headerSize)(array.length)
    putIntegers(header, buffer)

    // Content of the array
    val capacity = getCapacityOr(ty, array.length)
    ty.elemT match {
      case st: ScalarType => putScalarArray(st, capacity, array, buffer)

      case tt: TupleType => tt.elemsT.head match {
        case st: ScalarType => putScalarArray(st, capacity * tt.elemsT.length, array, buffer)
        case other => throw new NotImplementedError(s"Encoding of tuples of $other")
      }

      case VectorType(st, vLen) =>
        val array2D = array.asInstanceOf[Array[Array[_]]]
        array2D.foreach {
          arr => putScalarArray(st, vLen.evalInt, arr, buffer)
        }
        buffer.position(afterHeader + capacity * vLen.evalInt * baseSize)

      case elemT: ArrayType =>
        val array2D = array.asInstanceOf[Array[Array[_]]]
        if (elemT.hasFixedAllocatedSize) {
          // "Plain" array
          array2D.foreach(putArray(elemT, _, buffer))
          val sizeOfElem = Type.getAllocatedSize(elemT).evalInt
          buffer.position(afterHeader + capacity * sizeOfElem)
        } else {
          // Ragged array: we store offsets between the header and the actual data
          val ofsSize = capacity * sizeOfInt
          val offsets = ByteBuffer.allocate(ofsSize)
          offsets.order(endianness)
          buffer.position(afterHeader + ofsSize)
          array2D.foldLeft(buffer.position() - beforeHeader)((ofs, arr) => {
            putArray(elemT, arr, buffer)
            putIntegers(Array(ofs), offsets)
            buffer.position() - beforeHeader
          })
          val endPos = buffer.position()
          buffer.position(afterHeader)
          offsets.position(0)
          buffer.put(offsets)
          buffer.position(endPos)
        }

      case NoType | UndefType => throw new IllegalArgumentException(s"Cannot encode $ty")
    }
  }

  // ---
  // Some private helper functions
  // ---
  
  /**
   * Wrapper on the top of `ByteBuffer.put` that interprets a byte buffer as a
   * int buffer, put `values` in it and increment the position.
   */
  private def putIntegers(values: Array[Int], buffer: ByteBuffer): Unit = {
    val before = buffer.position()
    buffer.asIntBuffer().put(values)
    buffer.position(before + sizeOfInt * values.length)
  }
  
  /**
   * Wrapper on the top of `ByteBuffer.put` that interprets the buffer as a
   * buffer of the appropriate type depending on the provided `ScalarType`,
   * put `array` in it and increment the position.
   */
  private def putScalarArray(st: ScalarType, capacity: Int,
                             array: Array[_], buffer: ByteBuffer): Unit = {
    val before = buffer.position()
    st match {
      case Int => buffer.asIntBuffer().put(array.asInstanceOf[Array[Int]])
      case Float => buffer.asFloatBuffer().put(array.asInstanceOf[Array[Float]])
      case Double => buffer.asDoubleBuffer().put(array.asInstanceOf[Array[Double]])
      case Bool =>
        val data = array.asInstanceOf[Array[Boolean]].map(b => if (b) 1 else 0)
        buffer.put(data.map(_.toByte))
    }
    buffer.position(before + baseSize * capacity)
  }
  
  /**
   * Shorthand to get the capacity of an array of fall back on its length if
   * the capacity is not in the type.
   */
  private def getCapacityOr(ty: ArrayType, fallback: Int): Int = ty match {
    case c: Capacity => c.capacity.evalInt
    case _ => fallback
  }

  private lazy val endianness = if (Executor.isLittleEndian) ByteOrder.LITTLE_ENDIAN else ByteOrder.BIG_ENDIAN
  private lazy val baseType = Type.getBaseScalarType(arrayType)
  private lazy val baseSize = Type.getAllocatedSize(baseType).eval
  private lazy val sizeOfInt = if (AlignArrays()) baseSize else Int.size.eval
}
