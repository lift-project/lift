package ir.ast

import apart.arithmetic.ArithExpr
import ir.{TupleType, VectorType, ScalarType, Type}

/**
 * Representation of a "user function" declaration which usually operates on scala values.
 * @param name The name of the function. This has to follow standard C naming conventions.
 * @param paramNames The array of parameter names.
 * @param body The body of the function as a string. The body currently must be valid OpenCL C code.
 * @param inTs The types of the parameters. The size and order has to match with `paramNames`.
 * @param outT The return type of the user function.
 */
case class UserFun(name: String, paramNames: Array[String], body: String,
                   inTs: Seq[Type], outT: Type) extends FunDecl(inTs.length)
                                                        with isGenerable {

  // enforce at runtime that types and names match
  if (paramNames.length != inTs.length || !namesAndTypesMatch())
    throw new IllegalArgumentException(s"Structure of parameter names ( $paramNamesString ) " +
                                       s"and the input type ( $inT ) doesn't match!")


  /**
   * Represent the types of the parameters as a single type.
   * @return If there are multiple parameters a tuple type is returned.
   *         Otherwise, the type of the single parameter is returned.
   */
  def inT = if (inTs.size == 1) inTs.head else TupleType(inTs:_*)

  /**
   * Returns the single name, or multiple names of the parameters.
   * @return If there is only a single parameter return the name of it.
   *         Otherwise return the array of parameter names.
   */
  def paramName = if (paramNames.length == 1) paramNames.head else paramNames

  /**
   * Vectorize the current function
   * @param n The vector width
   * @return
   */
  def vectorize(n: ArithExpr): UserFun = new UserFun(s"$name$n", paramNames, body,
                                                     inTs.map(_.vectorize(n)), outT.vectorize(n))

  /**
   * Get all unique tuple types from the types of this user function.
   * @return A sequence of tuple types used in the definition of this user function.
   */
  def tupleTypes: Seq[TupleType] = {
    (inTAsTupleType ++ outTAsTupleType).distinct
  }

  /**
   * Return a tuple type if the output type is one, otherwise return an empty sequence.
   * @return If the output type is a tuple return its type, otherwise return an empty sequence.
   */
  private def outTAsTupleType: Seq[TupleType] = {
    outT match {
      case tt: TupleType => Seq(tt)
      case _ => Seq()
    }
  }

  /**
   * Combine the type of the parameters and their name to figure out what names are mapping to
   * tuples and what are mapping to scalar values.
   *
   * @return A sequence of tuple types referred to in the parameter types.
   */
  private def inTAsTupleType: Seq[TupleType] = {
    def emit(param: (Type, Any)): Seq[TupleType] = {
      param match {
        case (tt: TupleType, _:String) => Seq(tt)
        case (tt: TupleType, names: Array[Any]) =>
          (tt.elemsT zip names).flatMap { case (t, n) => emit(t, n) }
        case _ => Seq()
      }
    }
    emit((inT, paramName))
  }


  // function for checking that names and types match
  private def namesAndTypesMatch(): Boolean = {

    def checkParam(param: (Type, Any)): Boolean = {
      param match {
        case (_:ScalarType, _: String) => true
        case (_:VectorType, _: String) => true
        case (_:TupleType, _: String)  => true
        case (tt:TupleType, names: Array[String]) =>
          if (tt.elemsT.length != names.length) false
          else (tt.elemsT zip names).forall( {case (t,n) => checkParam( (t,n) )} )
        case _ => false
      }
    }

    checkParam( (inT, paramName) )
  }

  // format parameter names
  private lazy val paramNamesString: String = {
    def printAny(arg: Any): String = arg match {
      case a: Array[Any] => "Array(" + a.map(printAny).reduce(_+", "+_) + ")"
      case _ => arg.toString
    }

    printAny(paramName)
  }

  // for debug purposes
  override def toString = name
}

object UserFun {
  /**
   * Constructor for creating instances of UserFun.
   * This provides convenience for creating instances with a single parameter.
   *
   * @param name The name of the function. This has to follow standard C naming conventions.
   * @param paramName The parameter name.
   * @param body The body of the function as a string.
   *             The body currently must be valid OpenCL C code.
   * @param inT The type of the parameter.
   * @param outT The return type of the user function.
   * @return
   */
  def apply(name: String, paramName: String, body: String, inT: Type, outT: Type): UserFun = {
    UserFun(name, Array(paramName), body, Seq(inT), outT)
  }

  @deprecated("replaced by UserFun.vectorize(n)")
  def vectorize(uf: UserFun, n: ArithExpr): UserFun = uf.vectorize(n)

}
