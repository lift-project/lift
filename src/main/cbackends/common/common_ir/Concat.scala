package cbackends.common.common_ir


import ir._
import ir.ast.{Expr, Pattern}
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{ArithExpr, Cst}

import scala.collection.immutable

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
case class Concat(n : Int) extends Pattern(arity = n) {

  // first component: memory to be replaced
  // second component: memory to replace the first component with
  var replacementMap: immutable.Map[Memory, Memory] = null

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case tt: TupleType =>
        if (tt.elemsT.length != n) throw new NumberOfArgumentsException

       Concat.computeOutType(tt)

      case _ => throw new TypeException(argType, "TupleType", this)
    }
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    // TODO: should this be something else?
    ???
//    assert(args.length == arity)
  }



}

object Concat {
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
    Concat(args.length)(args:_*)
  }

  def computeOutType(tt: TupleType): ArrayType = {

    val arrayTypes = tt.elemsT.map {
      case at@ArrayTypeWSWC(_, _, _) => at
      case _ => throw TypeException("All input types must be arrays!")
    }
    val elemType = arrayTypes.head.elemT
    if (! arrayTypes.forall( at => at.elemT == elemType  ))
      throw TypeException(s"Elements are not of the same type ($arrayTypes)!")

    val sizeAndCapacity = arrayTypes.tail.foldLeft( (arrayTypes.head.size,arrayTypes.head.capacity)) ((acc,at) =>  (acc._1+at.size,acc._2+at.capacity))

    ArrayTypeWSWC(elemType,sizeAndCapacity._1,sizeAndCapacity._2)

  }

}