package opencl.ir.pattern

import ir.{Type, ScalarType, TupleType, ArrayType, TypeChecker, TypeException}
import opencl.ir.id
import ir.ast.{FPattern, Lambda, Lambda1, Lambda2, Pattern, fun, isGenerable}
import ir.interpreter.Interpreter.ValueMap
import ir.view.View
import lift.arithmetic.{PosVar, Var}

/**
  * An implementation of the insertion sort.
  *
  * @param f a comparision function of type `a -> a -> bool` returning true
  *          iff it's first argument is less than the second one.
  * @param loopRead the index used to read the data from the input array
  * @param loopWrite the index used to write data to the output array.
  */
case class InsertionSortSeq(f: Lambda2, var loopRead: Var, var loopWrite: Var)
           extends Pattern(arity=1) with FPattern with isGenerable {
  
  // This attribute cannot be modified outside of the class
  private var _copyFun: Lambda1 = _
  def copyFun: Lambda1 = this._copyFun
  
  var writeView: View = _
  
  private def generateCopyFun(ty: Type): Lambda1 = ty match {
    case ScalarType(_, _) | TupleType(_) => id(ty, name="_insertion_sort_id")
    case ArrayType(elemTy, _) => MapSeq(generateCopyFun(elemTy))
    case _ =>
      throw new NotImplementedError(s"InsertionSortSeq.generateCopyFun: $ty")
  }
  
  override def checkType(argType: Type, setType: Boolean): Type = {
    argType match {
      case ArrayType(ty, len) =>
        f.params.foreach(p => p.t = ty)
        this._copyFun = generateCopyFun(ty)
        copyFun.params.head.t = ty
        TypeChecker.check(copyFun.body, setType)
        ArrayType(TypeChecker.check(f.body, setType), len)
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
    InsertionSortSeq(f, PosVar("i"), PosVar("j"))
}

object InsertionSortSeq {
  def apply(compare: Lambda2): Lambda1 = {
    fun(l => InsertionSortSeq(compare, PosVar("i"), PosVar("j")) $ l)
  }
}