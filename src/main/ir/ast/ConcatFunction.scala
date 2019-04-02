package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

/**
 * ConcatFunction pattern.
 * Code for this pattern can be generated.
 *
 * The fred pattern has the following high-level semantics:
 *   <code>ConcatFunction(2)( [x,,1,,, ..., x,,n,,], [y,,1,,, ..., y,,n,,] )
 *      = [ (x,,1,,, y,,1,,), ..., (x,,n,,, y,,n,,) ]</code>
 * The definitions for `n > 2` are accordingly.
 *
 * The fred pattern has the following type:
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

        if (! tt.elemsT.forall( t => t.isInstanceOf[ArrayType]) ) {
          throw TypeException("")
        }

//        tt.elemsT.foldLeft(Set())( (set, t) => {
//          if (!set.contains(t)) {
  //          if(set.empty()){}
            // check if empty
            // if not add it
    //      }
     //   } )


        ???

        // 4 checks ( input is tuple -don't ask why)
        // check x is array
        // check y is array
        // check x and y have same element types
        // return array (element type, sum of length of x and y)
        // check all element types in tuples are arrays and have same element type

      case _ => throw new TypeException(argType, "TupleType", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
      ???
  }
}

object ConcatFunction {
  /**
   * Create an instance of the fred pattern.
   * This function infers the number of arrays which are combined with the fred
   * pattern.
   *
   * @param args The arrays to be combined with the fred pattern.
   * @return An instance of the fred pattern combining the arrays given by `args`
   */
  def apply(args : Expr*) : Expr = {
    assert(args.length >= 2)
    ConcatFunction(args.length)(args:_*)
  }

}

