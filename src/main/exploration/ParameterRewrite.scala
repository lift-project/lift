package exploration

import ir.{Type, TypeChecker}
import ir.ast.debug.AssertType
import ir.ast.{Expr, FPattern, FunCall, FunDecl, Gather, Lambda, Param, ReorderWithStride, Scatter, Slide, Split, Value}
import lift.arithmetic.{ArithExpr, Cst, Var}

object ParameterRewrite {

  def substituteVars(ae: ArithExpr, substitutionTable: Map[Var, Cst]): ArithExpr =
    ae.visitAndRebuild {
      case v: Var if substitutionTable.contains(v) => substitutionTable(v)
      case arithexpr => arithexpr
    }

  def apply(lambda: Lambda, substitutionTable: Map[Var, Cst]): Lambda = {


    def substitute(e: Expr): Expr =
      e match {
        case call: FunCall => {
          val newArgs = call.args.map((arg) => Expr.replace(arg, substitute))
          FunCall(call.f match {
            case Split(chunkSize) =>
              Split(substituteVars(chunkSize, substitutionTable))

            case Slide(windowSize, step) =>
              val newWindowSize = substituteVars(windowSize, substitutionTable)
              val newStep = substituteVars(step, substitutionTable)
              Slide(newWindowSize, newStep)

            case AssertType(expectedType, name) =>
              AssertType(Type.substitute(expectedType, substitutionTable.asInstanceOf[Map[ArithExpr, ArithExpr]]), name)

            case Gather(ReorderWithStride(step)) =>
              Gather(ReorderWithStride(substituteVars(step, substitutionTable)))

            case Scatter(ReorderWithStride(step)) =>
              Scatter(ReorderWithStride(substituteVars(step, substitutionTable)))

            case fp: FPattern =>
              val replaced = FunDecl.replace(fp.f, substitute)
              if (fp.f.eq(replaced)) fp
              else fp.copy(replaced)

            case l: Lambda => FunDecl.replace(l, substitute)

            // TODO: add Reorder handling
            case f => f
          }, newArgs: _*)
        }

        case Value(value, typ) =>
          Value(value, Type.substitute(typ, substitutionTable.toMap))

        case other => other
    }

    val newParams = lambda.params.map(param =>
      Param(Type.substitute(param.t, substitutionTable.asInstanceOf[Map[ArithExpr, ArithExpr]])))

    val lambdaBodyWithNewParams = lambda.params.zip(newParams).foldLeft(lambda.body) {
      case (expr, (oldParam, newParam)) => Expr.replace(expr, oldParam, newParam)
    }
    val newLambda = Lambda(params = newParams, body = Expr.replace(lambdaBodyWithNewParams, substitute))

    TypeChecker(newLambda)

    newLambda
  }
}
