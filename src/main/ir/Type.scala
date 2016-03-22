package ir

import apart.arithmetic.{ArithExpr, Cst, Var}
import arithmetic.TypeVar

import scala.collection.{immutable, mutable}


/**
 * Class representing types.
 * Expressions (instances of the class Expr) have types.
 *
 * Possible types (implemented as subclasses) are:
 * - scala types
 * - vector types
 * - array types
 * - tuple types
 *
 * There is no function type.
 *
 */
sealed abstract class Type {
  lazy val varList : Seq[Var] = this match {
    case at: ArrayType => at.elemT.varList ++ at.len.varList
    case vt: VectorType => vt.len.varList.to[Seq]
    case tt: TupleType => tt.elemsT.foldLeft(Seq[Var]())((set,inT) => set ++ inT.varList)
    case _ => Seq[Var]().distinct
  }

  /**
   * Vectorize the current type:
   * - For a scalar type a vectorized type is returned.
   * - For a tuple type, all its component types are vectorized.
   * - For an array type, its element type is vectorized
   * - For a vectorized type the same type is returned
   *
   * @param n The vector width
   * @return A vectorized type derived from `this`
   */
  def vectorize(n: ArithExpr): Type = this match {
    case sT: ScalarType => new VectorType(sT, n)
    case tT: TupleType => new TupleType( tT.elemsT.map( _.vectorize(n) ):_* )
    case aT: ArrayType => asVector(aT, n)
    case v: VectorType => v
    case _ => throw new TypeException(this, "anything else")
  }

  private def asVector(at0: ArrayType, len: ArithExpr): Type = {
    at0.elemT match {
      case pt:ScalarType => new ArrayType(new VectorType(pt,len), at0.len/^len)
      case at1:ArrayType => new ArrayType(asVector(at1,len), at0.len)
      case _ => throw new TypeException(at0.elemT, "ArrayType or PrimitiveType")
    }
  }
}

/**
 * Instances of this class represent scalar types (e.g., int, float, ...)
 *
 * @param name The name of the type (e.g., "int")
 * @param size The size of an object of this type in bytes
 */
case class ScalarType(name: String, size: ArithExpr) extends Type {
  override def toString = name
}

/**
 * Instances of this class represent vector types (e.g., float2, float4, ...)
 *
 * @param scalarT The underlying scalar type
 * @param len The vector length, i.e., the number of components in the vector
 *            type
 */
case class VectorType(scalarT: ScalarType, len: ArithExpr) extends Type {
  override def toString = scalarT.toString + len.toString
}

/**
 * Instances of this class represent tuple types
 * (e.g, (float, int), (int, (int, float)) )
 *
 * @param elemsT The element types in order from left to right
 */
case class TupleType(elemsT: Type*) extends Type {
  override def toString
    = "Tuple(" + elemsT.map(_.toString).reduce(_ + ", " + _) + ")"
}

/**
 * Instances of this class represent array types with length information
 * (e.g., [int],,1024,, , [ [float],,4,, ],,N,,)
 *
 * @param elemT The element type of the array
 * @param len The length of the array
 */
case class ArrayType(elemT: Type, len: ArithExpr) extends Type {

  if (len.isEvaluable) {
    val length = len.evalDbl

    if (!length.isValidInt || length < 1)
      throw new TypeException(length + " is not a valid length for an array!")
  }

  override def toString = "Arr(" +elemT+","+len+ ")"
}

/**
 * This instance indicates that a type has not been determined yet, e.g., prior
 * to type checking
 */
object UndefType extends Type {override def toString = "UndefType"}

/**
 * This instance indicates that there should be nothing, i.e., this corresponds
 * to `void` or `bottom` in other type systems.
 */
object NoType extends Type {override def toString = "NoType"}

/**
 * Collection of operations on types
 */
object Type {
  /**
   * A string representation of a type
   *
   * @param t A type
   * @return A string representation of `t`
   */
  def name(t: Type): String = {
    t match {
      case st: ScalarType => st.name
      case vt: VectorType => vt.scalarT.name + vt.len.toString
      case tt: TupleType  => "Tuple_" + tt.elemsT.map(Type.name).reduce(_+"_"+_)
      case at: ArrayType  => "Array_" + Type.name(at.elemT)
      case _ => throw new IllegalArgumentException
    }
  }

  /**
   * Visit the given type `t` by recursively traversing it.
   *
   * Invokes the given function `pre` on a given type before recursively
   * traversing it.
   * Invokes the given function `post` on a given type after recursively
   * traversing it.
   *
   * This function returns nothing. Therefore, `pre` or `post` usually have a
   * side effect (e.g. printing a given type)
   *
   * @param t The type to be visited
   * @param pre The function to be invoked before traversing `t`
   * @param post The function to be invoked after travering `t`
   */
  def visit(t: Type, pre: Type => Unit, post: Type => Unit) : Unit = {
    pre(t)
    t match {
      case vt: VectorType => visit(vt.scalarT, pre, post)
      case tt: TupleType  => tt.elemsT.foreach(et => visit(et,pre,post))
      case at: ArrayType  => visit(at.elemT, pre, post)
      case _ => // nothing to do
    }
    post(t)
  }

  /**
   * This function return a new type which has been constructed from the given
   * type `t` by recursively visiting it and applying `pre` and `post` which
   * return new types for a given type.
   *
   * The visiting works as follows:
   * 1. the function `pre` is invoked on `t`
   * 1. the return value of `pre(t)` is recursively visited
   * 3. on the return value from the recursively visit the function `post` is
   *    invoked and its return value is returned from this function
   *
   * @param t The 'source' type to be visited
   * @param pre The function to be invoked on `t` before it is recursively
   *            visited. The return value of this function is then recursively
   *            visited.
   * @param post The function to be invoked on `t` after it has been recursively
   *             visited. The return value of this function is return from the
   *             entire function.
   * @return The rebuild type after recursively applying `pre` and `post` to `t`
   */
  def visitAndRebuild(t: Type,
                      pre: Type => Type,
                      post: Type => Type) : Type = {
    var newT = pre(t)
    newT = newT match {
      case vt: VectorType =>
        new VectorType(visitAndRebuild(vt.scalarT,
                                    pre, post).asInstanceOf[ScalarType],vt.len)

      case tt: TupleType =>
        new TupleType(tt.elemsT.map(et => visitAndRebuild(et,pre,post)):_*)

      case at: ArrayType =>
        new ArrayType(visitAndRebuild(at.elemT, pre, post), at.len)

      case _ => newT // nothing to do
    }
    post(newT)
  }


  /**
   * Return the base type of a type.
   * The base type is defined as follows:
   * - for a scala type the base type is the type itself
   * - for a vector type the base type is it's scalar type
   * - for a tuple type the base type is the type itself
   * - for an array type the base type is it's elements base type
   *
   * @param t A type
   * @return The base type of `t`
   */
  def getBaseType(t: Type): Type = {
    t match {
      case vt: VectorType => vt.scalarT
      case at: ArrayType  => getBaseType(at.elemT)
      case _ => t
    }
  }

  /**
   * Return the value type of a type.
   * The value type is defined as follows:
   * - for a scalar, vector, or tuple type the value type is the type itself
   * - for an array type the value type is it's elements value type
   *   (i.e., the first element type which is not an array type)
   *
   * @param t A type
   * @return The value type of `t`
   */
  def getValueType(t: Type): Type = {
    t match {
      case at: ArrayType  => getValueType(at.elemT)
      case _ => t
    }
  }

  /**
   * Return the element type of a vector or array type.
   *
   * @param t A type. Must be a vector or array type.
   * @return The element type of `t`
   */
  def getElemT(t: Type): Type = {
    t match {
      case vt: VectorType => vt.scalarT
      case at: ArrayType  => at.elemT
      case _ => throw new TypeException(t, "ArrayType or VectorType")
    }
  }

  /**
   * Return the type at a given index for a tuple type, or a tuple type nested
   * in an array type.
   * Returns the given type otherwise.
   *
   * @param t A type
   * @param index A index. Must be in range for the given type.
   * @return The type at the given index.
   */
  def getTypeAtIndex(t: Type, index: Int): Type = {
    t match {
      case tt: TupleType =>
        assert(index < tt.elemsT.length)
        tt.elemsT(index)
      case at: ArrayType =>
        ArrayType(getTypeAtIndex(at.elemT, index), at.len)
      case _ => t
    }
  }

  /**
   * For a given array type turn every nested vector type into a corresponding
   * array type.
   *
   * @param at An array type
   * @return An array type where all nested vector types have been replaced by
   *         corresponding array types.
   */
  def asScalarType(at: ArrayType): ArrayType = {
    at.elemT match {
      case vt: VectorType => new ArrayType(vt.scalarT, at.len*vt.len)
      case at: ArrayType =>  new ArrayType(asScalarType(at),at.len)
      case _ => throw new TypeException(at.elemT , "ArrayType or VectorType")
    }
  }

  /**
   * Return the size (in bytes) of a given type.
   *
   * @param t A type
   * @return The size in bytes.
   */
  def getSize(t: Type) : ArithExpr = {
    t match {
      case st: ScalarType => st.size
      case vt: VectorType => vt.scalarT.size * vt.len
      case tt: TupleType  => tt.elemsT.map(getSize).reduce(_+_)
      case at: ArrayType  => at.len * getSize(at.elemT)
      case _ => throw new IllegalArgumentException
    }
  }

  /**
   * Returns the length (i.e., the number of values represented by this type)
   *
   * @param t A type
   * @return The length of `t`
   */
  def getLength(t: Type) : ArithExpr = {
    t match {
      case st: ScalarType => Cst(1)
      case vt: VectorType => vt.len
      case tt: TupleType  => Cst(1)
      case at: ArrayType  => at.len
      case _ => throw new IllegalArgumentException
    }
  }

  /**
   * Returns a list of lengths for the given type.
   * If the given type is not an array type the list will contain one element
   * which is equal to calling `getLength(t)`.
   * If the given type is an array type the list will contain one element for
   * every nested type equal to calling `getLength` on every nested type and
   * concatinating them
   *
   * @param t A type
   * @return A sequence of lengths from `t`
   */
  def getLengths(t: Type): Seq[ArithExpr] = {
    t match {
      case at: ArrayType => Seq(getLength(at)) ++ getLengths(at.elemT)
      case _ => Seq(getLength(t))
    }
  }

  /**
   * TODO: document (christophe?)
   * @param t1
   * @param t2
   * @return
   */
  def reify(t1: Type, t2: Type): immutable.Map[TypeVar, ArithExpr] = {
    val result = mutable.Map[TypeVar, ArithExpr]()
    (t1, t2) match {
      case (at1: ArrayType, at2: ArrayType) =>
        result ++= reifyExpr(at1.len, at2.len) ++= reify(at1.elemT, at2.elemT)

      case (tt1: TupleType, tt2: TupleType) =>
        result ++= tt1.elemsT.zip(tt2.elemsT)
                      .foldLeft(mutable.Map[TypeVar, ArithExpr]())(
                            (m, types) => m ++= reify(types._1, types._2))

      case (vt1: VectorType, vt2: VectorType) =>
        result ++= reifyExpr(vt1.len, vt2.len)

      case _ => if (t1 != t2) throw new TypeException(t1, t2)
    }
    result.toMap
  }

  /**
   * TODO: document (christophe?)
   * @param e1
   * @param e2
   * @return
   */
  private def reifyExpr(e1: ArithExpr,
                        e2: ArithExpr) : immutable.Map[TypeVar, ArithExpr] = {
    val result = mutable.Map[TypeVar, ArithExpr]()
    (e1, e2) match {
      case (tv: TypeVar, _) => result += (tv->e2)
      case (_, tv: TypeVar) => result += (tv->e1)
      case _ => // todo check that the two expressions are equivalent
    }
    result.toMap
  }

  /**
   * Visit and rebuild the given type by replacing arithmetic expressions in the
   * length information of array and vector types following the given
   * substitution map.
   *
   * @param t A type
   * @param substitutions A substituon map with entries of the form
   *                      [pattern => replacement]
   * @return A new type with the substitution from `substitutions` applied to
   *         `t`
   */
  def substitute(t: Type,
                 substitutions: immutable.Map[ArithExpr, ArithExpr]) : Type = {
    Type.visitAndRebuild(t, t1 => t1, {
      case ArrayType(et,len) =>
        new ArrayType(et, ArithExpr.substitute(len, substitutions.toMap))

      case VectorType(st,len) =>
        new VectorType(st, ArithExpr.substitute(len, substitutions.toMap))

      case t: Type => t
    })
  }


  /**
   * Function to determine if two types are equal
   *
   * @param l A type object
   * @param r Another type object
   * @return True if `l` and `r` have the same type and are equal
   */
  def isEqual(l: Type, r: Type): Boolean = {
    (l, r) match {
      case (lst: ScalarType, rst: ScalarType) => isEqual(lst, rst)
      case (ltt: TupleType, rtt: TupleType)   => isEqual(ltt, rtt)
      case (lat: ArrayType, rat: ArrayType)   => isEqual(lat, rat)
      case (lvt: VectorType, rvt: VectorType) => isEqual(lvt, rvt)
      case _ => false
    }
  }

  def haveSameValueTypes(l: Type, r: Type): Boolean = {
    Type.isEqual(Type.getValueType(l), Type.getValueType(r))
  }

  def haveSameBaseTypes(l: Type, r: Type): Boolean = {
    Type.isEqual(Type.getBaseType(l), Type.getBaseType(r))
  }

  private def isEqual(l: ScalarType, r: ScalarType): Boolean = {
    l.size == r.size && l.name == r.name
  }

  private def isEqual(lt: TupleType, rt: TupleType): Boolean = {
    if (lt.elemsT.length != rt.elemsT.length) return false

    (lt.elemsT zip rt.elemsT).forall({ case (l,r) => isEqual(l, r) })
  }

  private def isEqual(l: ArrayType, r: ArrayType): Boolean = {
    l.len == r.len && isEqual(l.elemT, r.elemT)
  }

  private def isEqual(l: VectorType, r: VectorType): Boolean = {
    l.len == r.len && isEqual(l.scalarT, r.scalarT)
  }

  @deprecated("replaced by Type.vectorize(n)")
  def vectorize(t: Type, n: ArithExpr): Type = t.vectorize(n)

  /**
   * Devecorize a given type.
   * I.e. removes all vector types in it by replacing them with corresponding
   * scalar types.
   *
   * @param t A type
   * @return A type similar to `t` but without any vector types
   */
  def devectorize(t: Type): Type = {
    t match {
      case vt: VectorType => vt.scalarT
      case tt: TupleType  => TupleType( tt.elemsT:_* )
      case at: ArrayType  => ArrayType(devectorize(at.elemT), at.len)
      case _ => t
    }
  }
}

/**
 * Exception thrown by the type checker on an typing error
 * @param msg A string message presented to the user
 */
case class TypeException(msg: String) extends Exception(msg) {

  def this(found: Type, expected: String) =
    this(found + " found but " + expected + " expected")

  def this(found: Type, expected: Type) =
    this(found + " found but " + expected + " expected")

}

class ZipTypeException(val tt: TupleType)
  extends TypeException(s"Can not statically prove that sizes ( ${tt.elemsT.mkString(", ")} ) match!")

object ZipTypeException {
  def apply(tt: TupleType) = new ZipTypeException(tt)
}

/**
 * Exception thrown by the type checker on an arity mismatch
 * @param msg A string message presented to the user
 */
case class NumberOfArgumentsException(msg: String) extends Exception(msg) {
  def this()  = this("Number of arguments is wrong!")
}
