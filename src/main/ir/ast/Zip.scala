package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap

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
case class Zip(n : Int) extends Pattern(arity = n) with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case tt: TupleType =>
        if (tt.elemsT.length != n) throw new NumberOfArgumentsException

        // make sure all arguments are array types of equal size and capacity
        tt.elemsT.foreach({
          case _: ArrayType with Size with Capacity =>
          case t => throw new TypeException(t, "ArrayType", this)
        })
        val arrayTypes = tt.elemsT.map(_.asInstanceOf[ArrayType with Size with Capacity])

        // make sure all arguments have the same size
        if (arrayTypes.map(_.size).distinct.length != 1)
          throw new ZipTypeException(tt)

        ArrayTypeWSWC(TupleType(arrayTypes.map(_.elemT):_*), arrayTypes.head.size)

      case _ => throw new TypeException(argType, "TupleType", this)
    }
  }


  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    (n, args) match {
      case (2, Seq(a: Vector[_], b: Vector[_])) => a zip b
//      case (3, a, b, c) =>
      case _ => throw new NotImplementedError()
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

object Zip3D {

   def apply(arg1: Expr, arg2: Expr) : Expr = {
      Map(Map(\(tuple2 => Zip(tuple2._0, tuple2._1)))) o Map( \(tuple => Zip(tuple._0, tuple._1))) $ Zip(arg1,arg2)
    }

  def apply(arg1: Expr, arg2: Expr, arg3: Expr) : Expr = {
      Map(Map(\(tuple2 => Zip(tuple2._0, tuple2._1, tuple2._2)))) o Map( \(tuple => Zip(tuple._0, tuple._1, tuple._2))) $ Zip(arg1,arg2,arg3)
   }
}

object Zip2D{

  def apply(arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr) : Expr = {
    Map(\(tuple => Zip(tuple._0, tuple._1, tuple._2, tuple._3, tuple._4, tuple._5))) $ Zip(arg1, arg2, arg3, arg4, arg5, arg6)
  }

  def apply(arg1: Expr, arg2: Expr) : Expr = {
    Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(arg1, arg2)
  }

}
