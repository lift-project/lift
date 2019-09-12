package ir

import ir.ast.{ArrayFromExpr, Expr, FunCall, IRNode, Lambda, Param}
import lift.arithmetic.Var
import opencl.generator.TreatWarningsAsErrors

/**
 * Type-checks a lambda or an expression and returns its type/output type.
 */
object TypeChecker {
  def apply(lambda: Lambda): Type = {
    checkForSuspiciousTypeVariableDeclarations(lambda)
    check(lambda.body)
  }
  
  def apply(expr: Expr): Type = check(expr)
  
  /**
   * Shorthand to infer the type of an expression and check that it is equal to
   * a given type.
   *
   * @param expr the expression to type-check
   * @param expected the expected type for this expression
   * @param setType flag passed to the `check` method.
   */
  def assertTypeIs(expr: Expr, expected: Type, setType: Boolean = true): Unit = {
    val ty = check(expr, setType)
    if (ty != expected)
      throw new TypeException(ty, expected, expr)
  }
  
  /**
   * Infers and checks the type of an expression.
   *
   * @param expr the expression to type-check
   * @param setType if true, attach the type to the expression
   * @return the expression's inferred type.
   */
  def check(expr: Expr, setType: Boolean = true): Type = {
    val inferredOuT = expr match {
      case call: FunCall =>
        // 1. get type of arguments
        val argType = if (call.args.isEmpty)
          throw new IllegalArgumentException(s"FunCall without arguments: $call")
        else if (call.args.length == 1)
          check(call.args.head, setType)
        else
          // if multiple arguments wrap their types in a tuple
          TupleType(call.args.map(check(_, setType)): _*)

        // 2. check return type of function with the given arguments type
        call.f.checkType(argType, setType)

      case ArrayFromExpr(e) =>
        val et = check(e, setType)
        expr.t = ArrayTypeWSWC(et, 1, 1)
        expr.t
      case _ => expr.t
    }

    if (setType)
      expr.t = inferredOuT

    inferredOuT
  }
  
  def checkAndSetTypeForParams(params: Array[Param], argType: Type,
                               where: IRNode // For error reporting
                              ): Unit = {
    (params.length, argType) match {
      case (1, _) => params.head.t = argType
      case (n, tt: TupleType) if n == tt.elemsT.length =>
        (params zip tt.elemsT).foreach({case (p, t) => p.t = t})
      // error cases:
      case (n, tt: TupleType) =>
        throw NumberOfArgumentsException(
          s"Expected ${tt.elemsT.length} arguments but got $n"
        )
      case (n, _) =>
        throw new TypeException(argType, s"TupleType of arity $n", where)
    }
  }
  
  private def checkForSuspiciousTypeVariableDeclarations(lambda: Lambda): Unit = {
    // collect all the variables
    // TODO: getLengths is suspicious
    val vars = lambda.params.flatMap(p => Type.getLengths(p.t)).collect { case v: Var => v }

    // check that no 2 variables with the same name, but different id's exist
    vars.groupBy(_.name).foreach { case (name, vs) =>
      val ids = vs.map(_.id)
      if (ids.distinct.length > 1) {
        val msg = s"Type variable '$name' declared multiple times with ids: " +
          s"[${ids.map(_.toString).reduce(_ + ", " + _)}]"
        if (TreatWarningsAsErrors())
          throw SuspiciousTypeVariableDeclaredException(msg)
        else println("Warning: " + msg)
      }
    }
  }
}
