package ir.ast

import ir.interpreter.Interpreter._
import ir._
import opencl.ir.Int
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * Filter pattern.
 * Code for this pattern can be generated.
 *
 * The filter pattern has the following high-level semantics:
 * `Filter()( [x,,1,,, ..., x,,n,,] )( [y,,1,,, ..., y,,m,,] ) = [x<sub>y<sub>1</sub></sub> ..., x<sub>y<sub>m</sub></sub>]`
 *
 * The filter pattern has the following type:
 * `Filter() : [a],,I,, -> [Int],,J,, -> [a],,J,,`
 *
 */
case class Filter() extends Pattern(arity = 2) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case TupleType(ArrayTypeWS(t, n), ArrayTypeWS(Int, m)) =>
        ArrayTypeWSWC(t, m)

      case _ =>
        throw new TypeException(
          argType, "TupleType(ArrayType(_, _), ArrayType(Int, _))", this
        )
    }
  }


  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException
  }
}

object Filter {
  /**
   * Create an instance of the filter pattern.
   *
   * @param input The input array from which to extract elements.
   * @param ids An array of indices that specify which elements to extract.
   * @return Extracted elements specified by `ids`
   */
  def apply(input: Param, ids: Expr): Expr = {
    Filter()(input, ids)
  }
}
