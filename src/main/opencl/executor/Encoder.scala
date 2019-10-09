package opencl.executor

import java.nio.{ByteBuffer, ByteOrder}

import ir.Type.size_t
import ir._
import opencl.ir._

/**
 * A tool to encode scala values into byte buffers.
 * One instance of this class can encode several values but they must all match
 * the type given in the constructor *and* fit into the specified allocated space.
 *
 * The reason why this allocated space cannot always be retrieved from the type
 * is explained in the ScalaDoc comment at the top of `OpenCLMemoryAllocator.scala`
 *
 * @param mainTy Lift type of the values to be encoded
 * @param sizeof Allocated size in bytes
 */
class Encoder(mainTy: Type, sizeof: Long) {
  /**
   * Encode a Scala value that matches the type (and allocated size) of the
   * `Encoder` instance into a byte buffer.
   */
  def encode(any: Any): ByteBuffer = {
    // FIXME
    assert(sizeof <= scala.Int.MaxValue)
    val buffer = ByteBuffer.allocate(sizeof.toInt)
    buffer.position(0)
    buffer.order(endianness)
    encodeAny(mainTy, any, buffer)
    buffer
  }

  /**
   * Main function: encode any supported type.
   *
   * This function (and all the specialized methods below) always set the
   * buffer position to the next position to write to after their computation.
   */
  private def encodeAny(ty: Type, value: Any, buffer: ByteBuffer): Unit = {
    (ty, value) match {
      case (st: ScalarType, _) => encodeScalar(st, value, buffer)
      case (vt: VectorType, v: Array[_]) => encodeVector(vt, v, buffer)
      case (at: ArrayType, vector: Array[_]) => encodeArray(at, vector, buffer)
      case (tt: TupleType, tuple: Product) => encodeTuple(tt, tuple, buffer)
      case _ => throw new EncodingError(ty, value)
    }
  }

  private def encodeScalar(st: ScalarType, value: Any, buffer: ByteBuffer): Unit = {
    (st, value) match {
      case (Bool, b: Boolean) => buffer.put((if (b) 1 else 0).toByte)
      case (Int, i: Int) => buffer.putInt(i)
      case (Float, f: Float) => buffer.putFloat(f)
      case (Double, d: Double) => buffer.putDouble(d)
      case _ => throw new EncodingError(st, value)
    }
  }

  private def encodeVector(vt: VectorType, vector: Array[_], buffer: ByteBuffer): Unit = {
    vector.foreach(encodeScalar(vt.scalarT, _, buffer))
  }

  private def encodeArray(at: ArrayType, vector: Array[_], buffer: ByteBuffer): Unit = {
    val beforeHeader = buffer.position()
    val afterHeader = beforeHeader + at.headerLength * alignment
    def align(value: Int): Int = {
      if (baseSize < alignment && at.headerLength != 0)
        ((value + alignment - 1) / alignment) * alignment // pad at the end
      else value
    }

    // Header of the array
    val header = Array.fill(at.headerLength)(vector.length)
    header.foreach(size => putHeaderValue(size, buffer))

    // Content of the array
    val capacity = getCapacityOr(at, vector.length)
    val elemT = at.elemT
    if (elemT.hasFixedAllocatedSize) {
      // "Plain" array
      vector.foreach(encodeAny(elemT, _, buffer))
      val contentSize = align(capacity * Type.getAllocatedSize(elemT).eval)
      buffer.position(afterHeader + contentSize)
    } else {
      // Ragged array: we store offsets between the header and the actual data
      val ofsSize = capacity * alignment
      val offsets = ByteBuffer.allocate(ofsSize)
      offsets.order(endianness)
      buffer.position(afterHeader + ofsSize)
      vector.foldLeft(buffer.position() - beforeHeader)((ofs, vec) => {
        encodeAny(elemT, vec, buffer)
        putHeaderValue(ofs, offsets)
        buffer.position() - beforeHeader
      })
      val endPos = buffer.position()
      offsets.position(0)
      buffer.position(afterHeader)
      buffer.put(offsets)
      buffer.position(align(endPos))
    }
  }

  private def encodeTuple(tt: TupleType, tuple: Product, buffer: ByteBuffer): Unit = {
    val (baseSize, _) = tt.alignment

    def encodeTupleElt(tyAndValue: (Type, Any)): Unit = {
      val (ty, value) = tyAndValue
      val before = buffer.position()
      encodeAny(ty, value, buffer)
      ty match {
        case innerTt: TupleType =>
          val (_, count) = innerTt.alignment
          buffer.position(before + (baseSize * count).eval)
        case ScalarType(_, _) =>
          buffer.position(before + baseSize.eval)
        case VectorType(_, len) =>
          buffer.position(before + (baseSize * len).eval)
        case _ =>
      }
    }

    (tt.elemsT zip tuple.productIterator.toSeq).foreach(encodeTupleElt)
  }

  // ---
  // Some private helper functions
  // ---

  /**
   * Put a header value in the buffer and set the position to the next place
   * where to write
   */
  private def putHeaderValue(value: Int, buffer: ByteBuffer): Unit = {
    val before = buffer.position()
    size_t match {
      case Int => buffer.putInt(value)
      case _ => throw new NotImplementedError(size_t.toString)
    }
    buffer.position(before + alignment)
  }

  /**
   * Shorthand to get the capacity of an array of fall back on its length if
   * the capacity is not in the type.
   */
  private def getCapacityOr(ty: ArrayType, fallback: Int): Int = ty match {
    case c: Capacity => c.capacity.eval
    case _ => fallback
  }

  private lazy val endianness = if (Executor.isLittleEndian) ByteOrder.LITTLE_ENDIAN else ByteOrder.BIG_ENDIAN
  private lazy val baseSize = Type.getAllocatedSize(Type.getBaseType(mainTy)).eval
  private lazy val alignment = Math.max(baseSize, size_t.size.eval)
}

class EncodingError(ty: Type, value: Any) extends IllegalArgumentException(s"Cannot encode value $value with type $ty")