package opencl.ir.pattern

import ir.{Type, ArrayType, UnknownLengthArrayType, TypeChecker, TypeException}
import ir.ast.{FPattern, Lambda, Lambda1, Pattern, fun, isGenerable}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{ArithExpr, Var, PosVar}


case class FilterSeq(f: Lambda1, var loopRead: Var, var loopWrite: Var)
  extends Pattern(arity=1) with FPattern with isGenerable {
  
  assert(f.params.length == 1)
  
  def iterationCount: ArithExpr = loopRead.range.numVals
  
  override def checkType(argType: Type, setType: Boolean): Type = {
    // Check that the argument is an array and fetch it's type information
    val (elemT, len) = argType match {
      case UnknownLengthArrayType(_, _) =>
        throw new NotImplementedError()
      case ArrayType(ty, l) => (ty, l)
    }
    
    // Check that the predicate has type `elemT -> Boolean`
    // TODO: add support for booleans
    f.params.head.t = elemT
    val predicate_ty = TypeChecker.check(f.body, setType)
    if (predicate_ty != opencl.ir.Int)
      throw new TypeException(predicate_ty, "Int")
    
    // TODO: return UnknownLengthArrayType
    ArrayType(elemT, len)
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
