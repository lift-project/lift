package opencl.ir.pattern

import ir._
import ir.ast.{FPattern, Lambda, Lambda1, Pattern, fun, isGenerable}
import ir.interpreter.Interpreter.ValueMap
import opencl.ir.id
import lift.arithmetic.{PosVar, Var}

/**
  * An implementation of the sequential filter.
  *
  * @param f is the predicate used to filter the data: the elements x of the
  *          input array such as `f(x)` is true will be stored in the output
  *          array
  * @param loopRead is the index used to fetch data from the input array
  * @param loopWrite is the index user to store data into the output array
  */
case class FilterSeq(f: Lambda1, var loopRead: Var, var loopWrite: Var)
  extends Pattern(arity=1) with FPattern with isGenerable {
  /**
    * This Lambda is used to copy the elements that satisfy the predicate from
    * the input array to the output array. It is generated automatically during
    * the type-checking.
    */
  private var _copyFun: Lambda1 = _
  def copyFun: Lambda1 = this._copyFun
  
  private def generateCopyFun(ty: Type): Lambda1 = ty match {
      case _: ScalarType | _: TupleType => id(ty, name=s"_filterseq_${Type.name(ty)}_id")
      case ArrayType(elemTy) => MapSeq(generateCopyFun(elemTy))
      case _ => throw new NotImplementedError()
    }
  
  override def checkType(argType: Type, setType: Boolean): Type = {
    // Check that the argument is an array and fetch it's type information
    val (elemT, size) = argType match {
      case ArrayTypeWSWC(ty, size, _) => (ty, size)
      case _ => throw new TypeException(argType, "Array")
    }
    
    // Check that the predicate has type `elemT -> Boolean`
    // TODO: add support for booleans
    f.params.head.t = elemT
    val predicate_ty = TypeChecker.check(f.body, setType)
    if (predicate_ty != opencl.ir.Int)
      throw new TypeException(predicate_ty, "Int")
    
    // At this point, we are able to generate the copy function
    this._copyFun = this.generateCopyFun(elemT)
    this.copyFun.params.head.t = elemT
    TypeChecker.check(this.copyFun.body, setType)
    
    // TODO: return UnknownLengthArrayType
    ArrayTypeWSWC(elemT, size)
  }
  
  override def copy(f: Lambda): Pattern = FilterSeq(f, loopRead, loopWrite)
  
  override def eval(valueMap: ValueMap, args: Any*): Any = {
    args.head match {
      case a: Vector[_] => a.filter(f.eval(valueMap, _) != 1)
    }
  }
}

object FilterSeq {
  def apply(f: Lambda1): Lambda1 =
    fun(l => FilterSeq(f, PosVar("i"), PosVar("j"))(l))
}
