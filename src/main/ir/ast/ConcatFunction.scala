package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

/**
 * ConcatFunction pattern.
 * Code for this pattern can be generated.
 *
 * The concat pattern has the following high-level semantics:
 *   <code>ConcatFunction(2)( [x,,1,,, ..., x,,n,,], [y,,1,,, ..., y,,n,,] )
 *      = [ (x,,1,,, y,,1,,), ..., (x,,n,,, y,,n,,) ]</code>
 * The definitions for `n > 2` are accordingly.
 *
 * The concat pattern has the following type:
 *   `ConcatFunction(2) : [a],,i,, -> [b],,i,, -> [a x b],,i,,`
 * The definitions for `n > 2` are accordingly.
 *
 * @param n The number of arrays which are combined. Must be >= 2.
 */
case class ConcatFunction(n : Int) extends Pattern(arity = n) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case tt: TupleType =>
        if (tt.elemsT.length != n) throw new NumberOfArgumentsException

        val arrayTypes = tt.elemsT.map( t => t match {
          case at @ ArrayTypeWSWC(_,_,_) => at
          case _ => throw TypeException("All input types must be arrays!")
        })
        val elemType = arrayTypes.head.elemT
        if (! arrayTypes.forall( at => at.elemT == elemType  ))
          throw TypeException("Elements are not of the same type!")

        val sizeAndCapacity = arrayTypes.tail.foldLeft( (arrayTypes.head.size,arrayTypes.head.capacity)) ((acc,at) =>  (acc._1+at.size,acc._2+at.capacity))

        ArrayTypeWSWC(elemType,sizeAndCapacity._1,sizeAndCapacity._2)

      case _ => throw new TypeException(argType, "TupleType", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
      ???
  }
}

object ConcatFunction {
  /**
   * Create an instance of the concat pattern.
   * This function infers the number of arrays which are combined with the concat
   * pattern.
   *
   * @param args The arrays to be combined with the concat pattern.
   * @return An instance of the concat pattern combining the arrays given by `args`
   */
  def apply(args : Expr*) : Expr = {
    assert(args.length >= 2)
    ConcatFunction(args.length)(args:_*)
  }

}

