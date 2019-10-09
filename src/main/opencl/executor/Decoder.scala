package opencl.executor

import java.nio.{ByteBuffer, ByteOrder}

import ir.Type.size_t
import ir._
import lift.arithmetic.ArithExpr
import opencl.ir.{Bool, Double, Float, Int}


class Decoder(mainType: Type) {
  import Decoder.DecodeTypes._

  private lazy val baseSize = Type.getAllocatedSize(Type.getBaseType(mainType)).eval
  private lazy val alignment = Math.max(baseSize, size_t.size.eval)

  /**
   * Main function: decode any supported type.
   *
   * This function (and all the specialized methods below) always set the
   * buffer position to the next position to read from after their computation.
   */
  private def decodeAny[T](ty: Type, buffer: ByteBuffer)(hint: DecodeType[T]): T = {
    // Note: IntelliJ IDEA thinks this pattern matching does not type-check but it does.
    //       See: https://youtrack.jetbrains.com/issue/SCL-9888
    //       And: https://youtrack.jetbrains.com/issue/SCL-3170
    (hint, ty) match {
      // Scalars
      case (BOOL(), Bool) => buffer.get() != 0.toByte
      case (INT(), Int) => buffer.getInt()
      case (FLOAT(), Float) => buffer.getFloat()
      case (DOUBLE(), Double) => buffer.getDouble()

      // Vectors
      case (VECTOR(elt), VectorType(st, len)) => Vector.fill(len.evalInt)(decodeAny(st, buffer)(elt))

      // Arrays
      case (VECTOR(elt), at: ArrayType) => decodeArray(at, buffer)(elt)

      // Tuples
      case (T1(a1), tt@TupleType(t1)) =>
        val (baseSize, _) = tt.alignment
        Tuple1(decodeTupleElt(baseSize, t1, buffer)(a1))
      case (T2(a1, a2), tt@TupleType(t1, t2)) =>
        val (baseSize, _) = tt.alignment
        (decodeTupleElt(baseSize, t1, buffer)(a1), decodeTupleElt(baseSize, t2, buffer)(a2))
      case (T3(a1, a2, a3), tt@TupleType(t1, t2, t3)) =>
        val (baseSize, _) = tt.alignment
        (decodeTupleElt(baseSize, t1, buffer)(a1), decodeTupleElt(baseSize, t2, buffer)(a2), decodeTupleElt(baseSize, t3, buffer)(a3))
      case (T4(a1, a2, a3, a4), tt@TupleType(t1, t2, t3, t4)) =>
        val (baseSize, _) = tt.alignment
        (decodeTupleElt(baseSize, t1, buffer)(a1), decodeTupleElt(baseSize, t2, buffer)(a2), decodeTupleElt(baseSize, t3, buffer)(a3), decodeTupleElt(baseSize, t4, buffer)(a4))

      case _ => throw new IllegalArgumentException(s"$hint and $ty are not compatible")
    }
  }

  /**
   * Helper function to decode an component of a tuple.
   *
   * @param baseSize the size of the base elements of the struct.
   *                 Used to respect the alignment.
   */
  private def decodeTupleElt[T](baseSize: ArithExpr, ty: Type, buffer: ByteBuffer)(hint: DecodeType[T]): T = {
    val before = buffer.position()
    val value = decodeAny(ty, buffer)(hint)
    ty match {
      case tt: TupleType =>
        val (_, count) = tt.alignment
        buffer.position(before + baseSize.eval * count.eval)
      case ScalarType(_, _) =>
        buffer.position(before + baseSize.eval)
      case VectorType(_, len) =>
        buffer.position(before + baseSize.eval)
      case _ =>
    }
    value
  }

  private def decodeArray[E](ty: ArrayType, buffer: ByteBuffer)(hint: DecodeType[E]): Vector[E] = {
    val beforeHeader = buffer.position()
    val afterHeader = beforeHeader + ty.headerLength * alignment
    def align(value: Int): Int = {
      if (baseSize < alignment && ty.headerLength != 0)
        ((value + alignment - 1) / alignment) * alignment // pad at the end
      else value
    }

    // Fetch header
    val (capacity, size) = decodeHeader(ty, buffer)

    // Fetch content
    buffer.position(afterHeader)
    (hint, ty.elemT) match {
      case (VECTOR(_), at: ArrayType) =>
        if (at.hasFixedAllocatedSize) {
          val vec = Vector.fill(size)(decodeAny(at, buffer)(hint))
          val contentSize = align(Type.getAllocatedSize(at).eval * capacity)
          buffer.position(afterHeader + contentSize)
          vec
        } else {
          val offsets = Vector.fill(size)(decodeHeaderValue(buffer.position(), buffer))
          val vec = offsets.map((ofs: Int) => {
            buffer.position(beforeHeader + ofs)
            decodeAny(at, buffer)(hint)
          })
          val after = buffer.position()
          buffer.position(afterHeader + align(after - afterHeader))
          vec
        }
      case (_, elemT) =>
        val vec = Vector.fill(size)(decodeAny(ty.elemT, buffer)(hint))
        buffer.position(afterHeader + capacity * Type.getAllocatedSize(elemT).eval)
        vec
    }
  }

  /**
   * Decode or read from the type the "header" information, namely the size and
   * the capacity of the array.
   */
  private def decodeHeader(ty: ArrayType, buffer: ByteBuffer): (Int, Int) = {
    val before = buffer.position()
    ty match {
      case sc: Size with Capacity =>
        (sc.capacity.eval, sc.size.eval)
      case s: Size => (
        decodeHeaderValue(before + ty.capacityIndex * alignment, buffer),
        s.size.eval
      )
      case c: Capacity => (
        c.capacity.eval,
        decodeHeaderValue(before + ty.sizeIndex * alignment, buffer)
      )
      case _ => (
        decodeHeaderValue(before + ty.capacityIndex * alignment, buffer),
        decodeHeaderValue(before + ty.sizeIndex * alignment, buffer)
      )
    }
  }

  /** Read a header value at position `idx`. */
  private def decodeHeaderValue(idx: Int, buffer: ByteBuffer): Int = {
    val v = size_t match {
      case Int => buffer.getInt(idx)
      case _ => throw new NotImplementedError(size_t.toString)
    }
    buffer.position(idx + alignment)
    v
  }
}

object Decoder {
  import DecodeTypes._

  /**
   * Download a global argument through the JNI and return a Scala value of
   * type `T`.
   *
   * - If `T` is `Array[some scalar type]`, the output is a flattened version
   * of the kernel's output type.
   * - Otherwise the output is build out of a byte array. It is much slower but
   * can handle any Lift type.
   */
  def apply[T](ty: Type, data: GlobalArg)(implicit hint: DecodeType[T]): T = {
    hint match {
      case ARRAY(elt) => downloadFlat(Type.getBaseType(ty), data)(elt)
      case _ =>
        val buffer = ByteBuffer.wrap(data.asByteArray())
        val endianness = if (Executor.isLittleEndian) ByteOrder.LITTLE_ENDIAN else ByteOrder.BIG_ENDIAN
        buffer.order(endianness)
        decode(ty, buffer)(hint)
    }
  }

  /**
   * Public interface for building a typed value out of a byte buffer.
   * Only specify the expected type as `T` and Scala will infer a `DecodeType[T]`
   * instance to help the `Decoder` to keep track of the types of the produced
   * values all along the way. See `DecodeTypes` below for the list of supported
   * types.
   *
   * @param ty the Lift type of the value to decode
   * @param buffer a byte buffer, typically return by the Executor
   * @return a value of the expected type
   */
  def decode[T](ty: Type, buffer: ByteBuffer)(implicit hint: DecodeType[T]): T = {
    buffer.position(0)
    val decoder = new Decoder(ty)
    decoder.decodeAny(ty, buffer)(hint)
  }

  /**
   * Find a scalar type `st` such as casting the memory of the output as a
   * `st*` pointer makes sense.
   */
  private def getBaseTypeWithChecks(ty: Type): ScalarType = ty match {
    case st: ScalarType => st
    case vt: VectorType => vt.scalarT
    case tt: TupleType =>
      tt.elemsT.distinct match {
        case Seq(elemT) => getBaseTypeWithChecks(elemT)
        case _ => throw new IllegalArgumentException(s"Heterogeneous tuple type: $tt")
      }
    case at: ArrayType =>
      at match {
        case sc: Size with Capacity if sc.size == sc.capacity =>
        case _ => println(s"Warning: decoding a value of $ty as a flat array, use at your own risk…")
      }
      getBaseTypeWithChecks(at.elemT)
    case NoType | UndefType => throw new IllegalArgumentException(s"Not supported type")
  }

  private def downloadFlat[T](ty: Type, data: GlobalArg)(implicit hint: DecodeType[T]): Array[T] = {
    val baseType = getBaseTypeWithChecks(ty)
    (baseType, hint) match {
      case (Bool, BOOL()) => data.asBooleanArray()
      case (Int, INT()) => data.asIntArray()
      case (Float, FLOAT()) => data.asFloatArray()
      case (Double, DOUBLE()) => data.asDoubleArray()
      case _ => throw new IllegalArgumentException(s"$hint and $baseType are not compatible")
    }
  }

  object DecodeTypes {
    sealed trait DecodeType[T]
    sealed trait Scalar // Special tag for scalar types

    // Scalars
    final case class BOOL() extends DecodeType[Boolean] with Scalar
    final case class INT() extends DecodeType[Int] with Scalar
    final case class FLOAT() extends DecodeType[Float] with Scalar
    final case class DOUBLE() extends DecodeType[Double] with Scalar
    implicit val _B = BOOL()
    implicit val _I = INT()
    implicit val _F = FLOAT()
    implicit val _D = DOUBLE()

    // Array (decoded as a vector)
    final case class VECTOR[E](elt: DecodeType[E]) extends DecodeType[Vector[E]]
    implicit def wrapInVector[E](implicit elt: DecodeType[E]): DecodeType[Vector[E]] = VECTOR(elt)

    // Array (decoded as a flat 1D array)
    // The element type must me a scalar type
    case class ARRAY[E](elt: DecodeType[E] with Scalar) extends DecodeType[Array[E]]
    implicit def wrapInArray[E](implicit elt: DecodeType[E] with Scalar): DecodeType[Array[E]] = ARRAY(elt)

    // Tuples, up to 4 components (extend at will)
    final case class T1[A1](a1: DecodeType[A1]) extends DecodeType[Tuple1[A1]]
    final case class T2[A1, A2](a1: DecodeType[A1], a2: DecodeType[A2]) extends DecodeType[(A1, A2)]
    final case class T3[A1, A2, A3](a1: DecodeType[A1], a2: DecodeType[A2], a3: DecodeType[A3]) extends DecodeType[(A1, A2, A3)]
    final case class T4[A1, A2, A3, A4](a1: DecodeType[A1], a2: DecodeType[A2], a3: DecodeType[A3], a4: DecodeType[A4]) extends DecodeType[(A1, A2, A3, A4)]
    implicit def wrapInTuple1[A1](implicit a1: DecodeType[A1]): DecodeType[Tuple1[A1]] = T1(a1)
    implicit def wrapInTuple2[A1, A2](implicit a1: DecodeType[A1], a2: DecodeType[A2]): DecodeType[(A1, A2)] = T2(a1, a2)
    implicit def wrapInTuple3[A1, A2, A3](implicit a1: DecodeType[A1], a2: DecodeType[A2], a3: DecodeType[A3]): DecodeType[(A1, A2, A3)] = T3(a1, a2, a3)
    implicit def wrapInTuple4[A1, A2, A3, A4](implicit a1: DecodeType[A1], a2: DecodeType[A2], a3: DecodeType[A3], a4: DecodeType[A4]): DecodeType[(A1, A2, A3, A4)] = T4(a1, a2, a3, a4)
  }
}
