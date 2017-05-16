package ir.ast

import lift.arithmetic.{PosVar, Var}
import ir._
import ir.interpreter.Interpreter.ValueMap

/**
 * Abstract class for search patterns.
 *
 * An object of the search pattern has to be instantiated with a given lambda `f`,
 * the comparator function used to search the array, therefore it is not possible to 
 * have a term like `Search()`
 *
 * f returns a value denoting the result of a comparison between the element of the array
 * passed to it, and a ``baked in`` element, which the search is looking for.
 *
 * The value returned must adhere to the following schema:
 *    f(e) <  0  iff e  > ix
 *    f(e) == 0  iff e == ix 
 *    f(e) >  0  iff e  < ix
 *
 *  This is slightly counterintuitive, however it allows for efficient comparisons (for example)
 *  between numeric types using a simple subtraction, for example:
 *
 *  int compare(int e, int ix) {
 *    return (ix-e)
 *  }
 *
 * TODO: Should this be renamed something like "ArraySearch" to avoid confusion with the rewriting search?
 *
 * @param f A lambda comparing a passed element to a baked in search element, similar to C's `bsearch` function
 */

abstract class AbstractSearch(val f: Lambda, 
                              val name: String) extends Pattern(arity = 2) 
                                                        with FPattern {
  assert(f.params.length == 1)
  var indexVar: Var = PosVar("ix")

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case TupleType(defaultT, ArrayType(elemT)) =>
        // check the default and element type match
        // TODO: Can this be done in the pattern match statement?
        if(defaultT != elemT) throw new TypeException(defaultT, elemT, this)
        // check the direction function takes a single argument
        if(f.params.length != 1) throw new NumberOfArgumentsException
        // set the argument type to the array element type
        f.params(0).t = elemT
        // recursively check the body
        TypeChecker.check(f.body, setType)
        // ensure that the body function returns an integer
        if(f.body.t != opencl.ir.Int) throw new TypeException(f.body.t, "Int", f.body)
        // finally, return a single element array type
        ArrayTypeWSWC(elemT, 1)

      case _ => throw new TypeException(argType, "TupleType(a, ArrayType(a, _)", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Any =
    throw new NotImplementedError("AbstractSearch.eval is not implemented")
}

case class Search(override val f: Lambda1) extends AbstractSearch(f, "Search") with isGenerable{
  override def copy(f: Lambda): Pattern = Search(f)
}
