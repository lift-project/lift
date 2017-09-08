package opencl.ir.pattern

import ir._
import ir.ast.{FPattern, Lambda, Lambda1, Pattern, fun}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{PosVar, Var}
import opencl.ir.Bool

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
  extends Pattern(arity=1) with FPattern  {
  override def checkType(argType: Type, setType: Boolean): Type = {
    val retTy = argType match {
      // Filter expects an array
      case at @ ArrayType(ty) => at match {
        // The size of the output of a filter can never be known statically.
        // But if we know the size (or at least the capacity) of the input, we
        // have an upper bound on the number of elements of the output, in
        // other words, its capacity.
        case s: Size => ArrayTypeWC(ty, s.size)
        case c: Capacity => ArrayTypeWC(ty, c.capacity)
        // Without further information we just return an array type
        case _ => ArrayType(ty)
      }
      case _ => throw new TypeException(argType, "Array", this)
    }
    
    // Check that the predicate has type `elemT -> Bool`
    f.params.head.t = retTy.elemT
    TypeChecker.assertTypeIs(f.body, Bool, setType)
    
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
