package ir.ast

import ir._

/**
 * Zip pattern.
 * Code for this pattern can be generated.
 *
 * The zip pattern has the following high-level semantics:
 *   <code>Zip(2)( [x,,1,,, ..., x,,n,,], [y,,1,,, ..., y,,n,,] )
 *      = [ (x,,1,,, y,,1,,), ..., (x,,n,,, y,,n,,) ]</code>
 * The definitions for `n > 2` are accordingly.
 *
 * The zip pattern has the following type:
 *   `Zip(2) : [a],,i,, -> [b],,i,, -> [a x b],,i,,`
 * The definitions for `n > 2` are accordingly.
 *
 * @param n The number of arrays which are combined. Must be >= 2.
 */
case class Zip(n : Int) extends FunDecl(arity = n) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case tt: TupleType =>
        if (tt.elemsT.length != n) throw new NumberOfArgumentsException

        // make sure all arguments are array types
        tt.elemsT.map({
          case at: ArrayType => at
          case t => throw new TypeException(t, "ArrayType")
        })
        val arrayTypes = tt.elemsT.map(_.asInstanceOf[ArrayType])

        // make sure all arguments have the same size
        if (arrayTypes.map(_.len).distinct.length != 1)
          throw new ZipTypeException(tt)

        ArrayType(TupleType(arrayTypes.map(_.elemT):_*), arrayTypes.head.len)

      case _ => throw new TypeException(argType, "TupleType")
    }
  }
}

object Zip {
  /**
   * Create an instance of the zip pattern.
   * This function infers the number of arrays which are combined with the zip
   * pattern.
   *
   * @param args The arrays to be combined with the zip pattern.
   * @return An instance of the zip pattern combining the arrays given by `args`
   */
  def apply(args : Expr*) : Expr = {
    assert(args.length >= 2)
    Zip(args.length)(args:_*)
  }
}
