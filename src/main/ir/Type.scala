package ir

import lift.arithmetic._
import arithmetic.TypeVar

import scala.collection.immutable.HashMap
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
    case at: ArrayType => at.elemT.varList ++
      (at match {case s:Size => s.size.varList; case _ => Seq() }) ++
      (at match {case c:Capacity => c.capacity.varList; case _ => Seq()})
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
    case sT: ScalarType => VectorType(sT, n)
    case tT: TupleType => TupleType( tT.elemsT.map( _.vectorize(n) ):_* )
    case aT: ArrayType with Size with Capacity => asVector(aT, n)
    case v: VectorType => v
    case _ => throw new TypeException(this, "anything else")
  }

  private def asVector(at0: ArrayType with Size with Capacity, len: ArithExpr): Type = {
    at0.elemT match {
      case pt:ScalarType => ArrayTypeWSWC(VectorType(pt,len), at0.size/^len, at0.capacity/^len)
      case at1:ArrayType with Size with Capacity => ArrayTypeWSWC(asVector(at1,len), at0.size, at0.capacity)
      case _ => throw new TypeException(at0.elemT, "ArrayType or PrimitiveType")
    }
  }

  /**
    * Return true if the type has a fixed allocated size in byte.
    * For instance, this will always be true all the primitive types.
    * It will be false for array's whose capacity is not in the type.
   */
  def hasFixedAllocatedSize: Boolean

}

/**
 * Instances of this class represent scalar types (e.g., int, float, ...)
 *
 * @param name The name of the type (e.g., "int")
 * @param size The size of an object of this type in bytes
 */
case class ScalarType(name: String, size: ArithExpr) extends Type {
  override def toString : String = name
  override def hasFixedAllocatedSize: Boolean = true
}

/**
 * Instances of this class represent vector types (e.g., float2, float4, ...)
 *
 * @param scalarT The underlying scalar type
 * @param len The vector length, i.e., the number of components in the vector
 *            type
 */
case class VectorType(scalarT: ScalarType, len: ArithExpr) extends Type {
  override def toString : String = scalarT.toString + len.toString
  override def hasFixedAllocatedSize: Boolean = true
}

/**
 * Instances of this class represent tuple types
 * (e.g, (float, int), (int, (int, float)) )
 *
 * @param elemsT The element types in order from left to right
 */
case class TupleType(elemsT: Type*) extends Type {
  override def toString : String
    = "Tuple(" + elemsT.map(_.toString).reduce(_ + ", " + _) + ")"

  override def hasFixedAllocatedSize: Boolean = elemsT.forall(_.hasFixedAllocatedSize)

}


/**
  * This trait is used to store size information (number of elements in an array) in an ArrayType.
  */
sealed trait Size {
  val size: ArithExpr
}

/**
  * This trait is used to store capacity information (maximum number of elements that can be held in an array) in an ArrayType.
  */
sealed trait Capacity {
  val capacity: ArithExpr
}

/**
 * Instances of this class represent array types with length information
 * (e.g., [int],,1024,, , [ [float],,4,, ],,N,,)
 *
 * @param elemT The element type of the array
 */
case class ArrayType(elemT: Type) extends Type {

  def getSize : Option[ArithExpr] = {
    this match {
      case s:Size => Some(s.size)
      case _ => None
    }
  }

  def getCapacity : Option[ArithExpr] = {
    this match {
      case c:Capacity => Some(c.capacity)
      case _ => None
    }
  }

  override def toString : String = {
    "Arr(" +elemT+
    (this match { case s:Size => ",s="+s.size.toString; case _ => ""}) +
    (this match { case c:Capacity => ",c="+c.capacity.toString; case _ => ""}) +
    ")"
  }

  override def hasFixedAllocatedSize: Boolean =
    this match {
      case _:Capacity => elemT.hasFixedAllocatedSize
      case _ => false
    }

  // we need to override equals to use Type.isEqual since this will take care of the Size and Capacity traits
  override def equals(o: Any) : Boolean = {
    o match {
      case at: ArrayType => Type.isEqual(this, at)
      case _ => false
    }
  }

  // we need to override hashCode to make sure we check the size and capacity
  override def hashCode(): Int = {
    runtime.ScalaRunTime._hashCode(this) +
      (this match {case s:Size => s.size.hashCode case _ => 0 }) +
      (this match {case c:Capacity => c.capacity.hashCode case _ => 0 })
  }

}


object ArrayType {


  def checkSizeOrCapacity(s: String, ae: ArithExpr) : Unit = {
    // TODO: remove the need to check for unknown (but this is used currently in a few places)
    if (ae != ? & ae.sign != Sign.Positive)
    // TODO: turn this back into an error (eventually)
    //throw new TypeException("Length must be provably positive! (len="+len+")")
      println(s"Warning: $s must be provably positive! (len=$ae)")

    if (ae.isEvaluable) {
      val length = ae.evalDouble

      if (!length.isValidInt || length < 1)
        throw TypeException(length + " is not a valid "+s+" for an array!")
    }
  }

}


object ArrayTypeWSWC {

  def apply(elemT: Type, sizeAndCapacity: ArithExpr) : ArrayType with Size with Capacity = {
    apply(elemT, sizeAndCapacity, sizeAndCapacity)
  }

  def apply(elemT: Type, _size: ArithExpr, _capacity: ArithExpr) : ArrayType with Size with Capacity = {

    // TODO: remove this assertation once the framework is ready to handle array with a different size and capacity
    assert (_size == _capacity)

    ArrayType.checkSizeOrCapacity("size", _size)
    ArrayType.checkSizeOrCapacity("capacity", _capacity)
    new ArrayType(elemT) with Size with Capacity {
      val size: ArithExpr = _size
      val capacity: ArithExpr = _capacity
    }
  }

  def unapply(at : ArrayType with Size with Capacity): Option[(Type,ArithExpr,ArithExpr)]  = {
    Some(at.elemT,at.size,at.capacity)
  }
}

object ArrayTypeWS {

  def apply(elemT: Type, _size: ArithExpr) : ArrayType with Size = {
    ArrayType.checkSizeOrCapacity("size", _size)
    new ArrayType(elemT) with Size {
      val size: ArithExpr = _size
    }
  }

  def unapply(at : ArrayType with Size): Option[(Type,ArithExpr)]  = {
    Some(at.elemT,at.size)
  }
}

object ArrayTypeWC {

  def apply(elemT: Type, _capacity: ArithExpr) : ArrayType with Capacity  = {
    ArrayType.checkSizeOrCapacity("capacity", _capacity)
    new ArrayType(elemT) with Capacity {
      val capacity: ArithExpr = _capacity
    }
  }

  def unapply(at : ArrayType with Capacity): Option[(Type,ArithExpr)]  = {
    Some(at.elemT,at.capacity)
  }
}




// todo make sure we can distinguish between different unkownlengtharraytype (override hashCode and equals)
class RuntimeSizedArrayType(override val elemT: Type) extends ArrayType(elemT) {
  override def hasFixedAllocatedSize: Boolean = false
}

object RuntimeSizedArrayType {
  def apply(elemT: Type): RuntimeSizedArrayType = {
    new RuntimeSizedArrayType(elemT)
  }
  def unapply(array: RuntimeSizedArrayType): Type = array.elemT
}

/**
 * This instance indicates that a type has not been determined yet, e.g., prior
 * to type checking
 */
object UndefType extends Type {
  override def toString = "UndefType"
  override def hasFixedAllocatedSize = false
}

/**
 * This instance indicates that there should be nothing, i.e., this corresponds
 * to `void` or `bottom` in other type systems.
 */
object NoType extends Type {
  override def toString = "NoType"
  override def hasFixedAllocatedSize = true
}

/**
 * Collection of operations on types
 */
object Type {

  def fromAny(a: Any): Type = {
    a match {
      case _: Float => ScalarType("float", 4)
      case _: Int => ScalarType("int", 4)
      case a: Seq[_] if a.nonEmpty => ArrayTypeWSWC(fromAny(a.head), a.length)
      case t: (_,_) => TupleType(Seq(fromAny(t._1), fromAny(t._2)):_*)
      case t: (_,_,_) => TupleType(Seq(fromAny(t._1), fromAny(t._2), fromAny(t._3)):_*)
      case _ => throw new NotImplementedError()
    }
  }

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
      case tt: TupleType  => s"Tuple${tt.elemsT.length}_" + tt.elemsT.map(Type.name).reduce(_+"_"+_)
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
  def visit(t: Type, pre: Type => Unit, post: Type => Unit = _ => {}) : Unit = {
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
        VectorType(visitAndRebuild(vt.scalarT,
                                    pre, post).asInstanceOf[ScalarType],vt.len)

      case tt: TupleType =>
        TupleType(tt.elemsT.map(et => visitAndRebuild(et,pre,post)):_*)

      case ArrayTypeWSWC(et, s, c) => ArrayTypeWSWC(visitAndRebuild(et, pre, post), s, c)
      case ArrayTypeWC(et,c) => ArrayTypeWC(visitAndRebuild(et, pre, post), c)
      case ArrayTypeWS(et,s) => ArrayTypeWS(visitAndRebuild(et, pre, post), s)
      case ArrayType(et) => ArrayType(visitAndRebuild(et, pre, post))

      case _ => newT // nothing to do
    }
    post(newT)
  }

  /**
    * This function returns a new type which has been constructed from the given
    * type `t` by recursively visiting it and applying `f` to any arithmetic expression.
    * @param t The type to be visited
    * @param f The function to be invoked on any arithmetic expression
    * @return The rebuilt type
    */
  def visitAndRebuild(t: Type,
                      f: ArithExpr => ArithExpr) : Type = {
    Type.visitAndRebuild(t, {
      case ArrayTypeWSWC(et,s,c) => ArrayTypeWSWC(et, f(s), f(c))
      case ArrayTypeWS(et,s) => ArrayTypeWS(et, f(s))
      case ArrayTypeWC(et,c) => ArrayTypeWC(et, f(c))
      case at : ArrayType => at
      case vt: VectorType => VectorType(vt.scalarT, f(vt.len))
      case tt: TupleType => tt
      case st: ScalarType => st
      case NoType => NoType
      case UndefType => UndefType
    }, t => t)
  }

  def visit(t: Type, f: ArithExpr => Unit) : Unit = {
    Type.visit(t, {
      case at: ArrayType =>
        at match {case s: Size => f(s.size) case _ => }
        at match {case c: Capacity => f(c.capacity) case _ => }
      case vt: VectorType => f(vt.len)
      case _: TupleType  | _:ScalarType =>
      case NoType | UndefType =>
    }, _=>{})
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
  @scala.annotation.tailrec
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
  @scala.annotation.tailrec
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

      case ArrayTypeWSWC(et,s,c) => ArrayTypeWSWC(getTypeAtIndex(et, index), s,c)
      case ArrayTypeWC(et,c) => ArrayTypeWC(getTypeAtIndex(et, index), c)
      case ArrayTypeWS(et,s) => ArrayTypeWS(getTypeAtIndex(et, index), s)
      case ArrayType(et) => ArrayType(getTypeAtIndex(et, index))

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
  def asScalarType(at: ArrayType with Size with Capacity): ArrayType = {

    // TODO: if the array has no size or capacity in the type, we need to somehow store the information that the new length is the same the old one but divided by the vector length

    at.elemT match {
      case vt: VectorType => ArrayTypeWSWC(vt.scalarT, at.size*vt.len, at.capacity*vt.len)
      case at: ArrayType with Size with Capacity =>  ArrayTypeWSWC(asScalarType(at),at.size, at.capacity)
      case _ => throw new TypeException(at.elemT , "ArrayType or VectorType")
    }
  }

  /**
   * Return the size (in bytes) of a given type.
   *
   * @param t A type
   * @return The size in bytes.
   */
  def getAllocatedSize(t: Type) : ArithExpr = {
    t match {
      case st: ScalarType => st.size
      case vt: VectorType => vt.scalarT.size * vt.len
      case tt: TupleType  => tt.elemsT.map(getAllocatedSize).reduce(_+_)
      case at: ArrayType with Capacity => at.capacity * getAllocatedSize(at.elemT)
      case _ => throw new IllegalArgumentException
    }
  }

  def getMaxAllocatedSize(t: Type) : ArithExpr = {
    // quick hack (set all the type var to theur max value)
    // TODO: need to be fixed
    val size = getAllocatedSize(t)
    val map = TypeVar.getTypeVars(size).map(tv => (tv, tv.range.max)).toMap
    ArithExpr.substitute(size, map.toMap)
  }

  def getMaxLength(t: Type) : ArithExpr = {
    // quick hack (set all the type var to theur max value)
    // TODO: need to be fixed
    val size = getLength(t)
    val map = TypeVar.getTypeVars(size).map(tv => (tv, tv.range.max)).toMap
    ArithExpr.substitute(size, map.toMap)
  }

  /**
   * Returns the length (i.e., the number of values represented by this type)
   *
   * @param t A type
   * @return The length of `t`
   */
  def getLength(t: Type) : ArithExpr = {
    t match {
      case _: ScalarType => Cst(1)
      case vt: VectorType => vt.len
      case _: TupleType  => Cst(1) // TODO: is this correct??
      case ArrayTypeWS(_,s) => s
      case _:ArrayType => ? // TODO: when used from the view or codegen, we may need to return a bit of AST node. Currently, if the Size is not in the type, the length will be unknown
      case _ => throw new IllegalArgumentException(t.toString)
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
   */
  def reify(t1: Type, t2: Type): immutable.Map[TypeVar, ArithExpr] = {
    val result = mutable.Map[TypeVar, ArithExpr]()
    (t1, t2) match {
      case (at1: ArrayType, at2: ArrayType) =>
        (at1, at2) match { case (s1:Size, s2:Size) => result ++= reifyExpr(s1.size, s2.size) case _ => }
        (at1, at2) match { case (c1:Capacity, c2:Capacity) => result ++= reifyExpr(c1.capacity, c2.capacity) case _ => }
        result ++= reify(at1.elemT, at2.elemT)

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

  def substitute(t: Type, oldVal: ArithExpr, newVal: ArithExpr) : Type ={
    Type.substitute(t,new HashMap[ArithExpr,ArithExpr]() + ((oldVal, newVal)))
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
                 substitutions: scala.collection.Map[ArithExpr, ArithExpr]) : Type = {

    visitAndRebuild(t, (ae: ArithExpr) => ArithExpr.substitute(ae, substitutions.toMap))
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
    isEqual(l.elemT, r.elemT) && l.getSize == r.getSize && l.getCapacity == r.getCapacity
  }


  private def isEqual(l: VectorType, r: VectorType): Boolean = {
    l.len == r.len && isEqual(l.scalarT, r.scalarT)
  }

  /**
   * Devectorize a given type.
   * I.e. removes all vector types in it by replacing them with corresponding
   * scalar types.
   *
   * @param t A type
   * @return A type similar to `t` but without any vector types
   */
  def devectorize(t: Type): Type = {
        t match {
        case ArrayType(VectorType(st,len)) => t match {
          case ArrayTypeWSWC(_,s,c) => ArrayTypeWSWC(st, len * s, len * c)
          case ArrayTypeWS(_,s) => ArrayTypeWS(st, len * s)
          case ArrayTypeWC(_,c) => ArrayTypeWC(st, len * c)
          case _ => ArrayType(st)
            // TODO: somehow communicate the information that when the size or capacity is not in the type, it is still multiplied by len
        }
        case ArrayTypeWSWC(et,s,c) => ArrayTypeWSWC(devectorize(et), s,c)
        case ArrayTypeWS(et,s) => ArrayTypeWS(devectorize(et), s)
        case ArrayTypeWC(et,c) => ArrayTypeWC(devectorize(et), c)
        case ArrayType(et) => ArrayType(devectorize(et))

        case vt: VectorType => vt.scalarT

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

case class SuspiciousTypeVariableDeclaredException(msg: String) extends Exception(msg)
