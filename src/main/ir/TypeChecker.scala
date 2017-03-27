package ir

import ir.ast._
import lift.arithmetic.Var
import opencl.generator.TreatWarningsAsErrors

object TypeChecker {

  def apply(lambda: Lambda): Type = {
    checkForSuspiciousTypeVariableDeclarations(lambda)
    check(lambda.body)
  }

  def apply(expr: Expr): Type = check(expr)

  def check(expr: Expr, setType: Boolean = true): Type = {

    val inferredOuT = expr match {
      case param: Param =>
        // assert(param.t != UndefType)
        param.t

      case call: FunCall =>
        // 1. get type of arguments
        val argType = if (call.args.isEmpty) {
          NoType // TODO: throw exception?
        } else if (call.args.length == 1) {
          check(call.args.head, setType)
        } else {
          // if multiple arguments wrap their types in a tuple
          TupleType( call.args.map(check(_, setType)):_* )
        }

        // 2. check type of function with the given arguments type

        call.f.checkType(argType, setType)
    }

    if (setType)
      expr.t = inferredOuT

    inferredOuT
  }

  def checkAndSetTypeForParams(params: Array[Param], argType: Type): Unit = {
    (params.length, argType) match {
      case (0, _) =>
      case (1, _) => params.head.t = argType
      case (n, tt: TupleType) if n == tt.elemsT.length =>
        (params zip tt.elemsT).foreach({case (p, t) => p.t = t})
      // error cases:
      case (n, tt: TupleType) =>
        throw NumberOfArgumentsException(s"Expected $n arguments but " +
                                             s"got ${tt.elemsT.length}")
      case _ => throw new TypeException(argType, "some other type")
    }
  }

  def checkForSuspiciousTypeVariableDeclarations(lambda: Lambda): Unit = {
    val vars = lambda.params.flatMap(p => Type.getLengths(p.t)).collect { case v: Var => v }

    // check that no 2 variables with the same name, but different id's exist
    vars.groupBy(_.name).foreach { case (name, vs) =>
      val ids = vs.map(_.id)
      if (ids.distinct.length > 1) {
        val msg = s"Type variable '$name' declared multiple times with ids: " +
          s"[${ids.map(_.toString).reduce(_ + ", " + _)}]"
        if (TreatWarningsAsErrors()) {
          throw SuspiciousTypeVariableDeclaredException(msg)
        } else {
          println("Warning: " + msg)
        }
      }
    }
  }

}
