package opencl.ir.pattern

import ir.{Type, ScalarType, TupleType, ArrayType, TypeChecker, TypeException}
import opencl.ir.id
import ir.ast.{FPattern, Lambda, Lambda1, Lambda2, Pattern, fun, isGenerable}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{PosVar, Var}

/**
  * An implementation of the insertion sort.
  *
  * @param f a comparision function of type `a -> a -> bool` returning true
  *          iff it's first argument is less than the second one.
  * @param loopRead the index used to read the data from the input array
  * @param loopWrite the index used to write data to the output array.
  * @param loopShift the index used during the inserting to shift data to the
  *                  right of the array.
  */
case class InsertionSortSeq(f: Lambda2, var loopRead: Var,
                            var loopWrite: Var, var loopShift: Var)
           extends Pattern(arity=1) with FPattern with isGenerable {
 
  // These two functions are used to
  //   1. Copy data from the input array to the output array
  //   2. Move data in the output array to free some space for the insertion
  // Does this have to be here?
  private var _copyFun: Lambda1 = _
  def copyFun: Lambda1 = this._copyFun
  
  private var _shiftFun: Lambda1 = _
  def shiftFun: Lambda1 = this._shiftFun
  
  // Generate the identity function for the type `ty`
  private def generateCopyFun(ty: Type): Lambda1 = ty match {
    case ScalarType(_, _) | TupleType(_) => id(ty, name="_insertion_sort_id")
    case ArrayType(elemTy, _) => MapSeq(generateCopyFun(elemTy))
    case _ =>
      throw new NotImplementedError(s"InsertionSortSeq.generateCopyFun: $ty")
  }
  
  override def checkType(argType: Type, setType: Boolean): Type = {
    argType match {
      case ArrayType(ty, len) =>
        // Type-check the comparison function
        f.params.foreach(p => p.t = ty)
        TypeChecker.assertType(f.body, setType, opencl.ir.Int)
        
        // Generate and type-check the copy/shift functions
        this._copyFun = generateCopyFun(ty)
        this._shiftFun = generateCopyFun(ty)
        copyFun.params.head.t = ty
        shiftFun.params.head.t = ty
        TypeChecker.assertType(copyFun.body, setType, ty)
        TypeChecker.assertType(shiftFun.body, setType, ty)
        
        // Return the output type
        ArrayType(ty, len)
      case _ => throw new TypeException(argType, "ArrayType")
    }
  }
  
  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case a: Vector[_] =>
        a.sortWith(f.eval(valueMap, _, _).asInstanceOf[Boolean])
    }
  }
  
  def copy(f: Lambda): InsertionSortSeq =
    InsertionSortSeq(f, PosVar("s"), PosVar("t"), PosVar("k"))
}

object InsertionSortSeq {
  def apply(compare: Lambda2): Lambda1 = {
    fun(l =>
      InsertionSortSeq(compare, PosVar("s"), PosVar("t"), PosVar("k")) $ l
    )
  }
}