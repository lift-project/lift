package opencl.ir.pattern

import ir._
import ir.ast.{FPattern, Lambda, Lambda1, Pattern, fun, isGenerable}
import ir.interpreter.Interpreter.ValueMap
import opencl.ir.{id, Bool}
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
    // Check that the argument is an array and compute the output type
    val retTy = argType match {
      case ArrayTypeWS(ty, size) => ArrayTypeWC(ty, size)
      case ArrayTypeWC(ty, capacity) => ArrayTypeWC(ty, capacity)
      case ArrayType(ty) => ArrayType(ty)
      case _ => throw new TypeException(argType, "Array", this)
    }
    
    // Check that the predicate has type `elemT -> Bool`
    f.params.head.t = retTy.elemT
    TypeChecker.assertTypeIs(f.body, Bool, setType)
    
    // At this point, we are able to generate the copy function
    _copyFun = generateCopyFun(retTy.elemT)
    _copyFun.params.head.t = retTy.elemT
    TypeChecker.assertTypeIs(_copyFun.body, retTy.elemT, setType)
    
    retTy
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
