package exploration

import ir.{Type, TypeChecker}
import ir.ast.debug.AssertType
import ir.ast.{Expr, FPattern, FunCall, FunDecl, Gather, Lambda, Let, Param, ReorderWithStride, Scatter, Slide, Split, Unslide, UserFun, Value, VectorizeUserFun, asVector}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.generator.NDRange
import opencl.ir.ast.OpenCLBuiltInFun

import scala.language.implicitConversions

object ParameterRewrite {

  def substitute(ae: ArithExpr, rule: PartialFunction[Var, ArithExpr]): ArithExpr =
    ArithExpr.substitute(ae, rule)

  def apply(ndRange: NDRange, rule: PartialFunction[Var, ArithExpr]): NDRange = NDRange(
    ArithExpr.substitute(ndRange(0), rule),
    ArithExpr.substitute(ndRange(1), rule),
    ArithExpr.substitute(ndRange(2), rule))

  def apply(ndRanges: (NDRange, NDRange), rule: PartialFunction[Var, ArithExpr]): (NDRange, NDRange) =
    (apply(ndRanges._1, rule), apply(ndRanges._2, rule))



  def substitute(e: Expr, rule: PartialFunction[Var, ArithExpr]): Expr =
    e match {
      case call: FunCall =>
        FunCall(substitute(call.f, rule),
          call.args.map((arg) => Expr.replace(arg, substitute(_, rule))): _*)

      case Value(value, typ) => Value(value, Type.substitute(typ, rule))
      case p: Param =>  // should already be substituted
        val visited = p.t == Type.substitute(p.t, rule)
        assume(visited)
        p

      case other => throw new NotImplementedError() //other
    }

  def substitute(f: FunDecl, rule: PartialFunction[Var, ArithExpr]): FunDecl = f match {
    case Split(chunkSize) =>
      Split(substitute(chunkSize, rule))

    case Slide(windowSize, step) =>
      val newWindowSize = substitute(windowSize, rule)
      val newStep = substitute(step, rule)
      Slide(newWindowSize, newStep)

    case Unslide(windowSize, step) =>
      val newWindowSize = substitute(windowSize, rule)
      val newStep = substitute(step, rule)
      Unslide(newWindowSize, newStep)

    case AssertType(expectedType, expectedLengths, name) =>
      new AssertType(
        expectedType match {
          case Some(_) => Some(expectedType.get.map(
            Type.substitute(_, rule)))
          case None => None },
        expectedLengths match {
          case Some(_) => Some(expectedLengths.get.map {
            case (axis, length) => (axis, substitute(length, rule))
          })
          case None => None }, name)

    case Gather(ReorderWithStride(step)) =>
      Gather(ReorderWithStride(substitute(step, rule)))

    case Scatter(ReorderWithStride(step)) =>
      Scatter(ReorderWithStride(substitute(step, rule)))

    case asVector(vectorLen) =>
      asVector(substitute(vectorLen, rule))

    case l: Lambda =>
      val newParams = l.params.map(param => Param(Type.substitute(param.t, rule)))

      val lambdaBodyWithNewParams = l.params.zip(newParams).foldLeft(l.body) {
        case (expr, (oldParam, newParam)) => Expr.replace(expr, oldParam, newParam)
      }
      val lambdaBodySubstituted = Expr.replace(lambdaBodyWithNewParams, substitute(_, rule))

      l match {
        case _: Let => Let(params = newParams, body = lambdaBodySubstituted)
        case _ => Lambda(params = newParams, body = lambdaBodySubstituted)
      }


    case builtin: OpenCLBuiltInFun =>
      new OpenCLBuiltInFun(builtin.name, builtin.inTs,
        Type.substitute(builtin.outT, rule))


    case UserFun(name, paramNames, body, paramTypes, outType) =>
      UserFun(name, paramNames, body,
        paramTypes.map(Type.substitute(_, rule)),
        Type.substitute(outType, rule))

    case fp: FPattern =>
      val replaced = substitute(fp.f, rule)
      if (fp.f.eq(replaced)) fp
      else fp.copy(replaced)

    // TODO: add Reorder handling

    case other => other
  }

  def apply(lambda: Lambda, rule: PartialFunction[Var, ArithExpr]): Lambda = {

    val newLambda = substitute(lambda, rule)

    TypeChecker(newLambda)

    newLambda
  }

  implicit def toPartialAeToAe(rule: PartialFunction[Var, ArithExpr]): PartialFunction[ArithExpr, ArithExpr] =
    (_: ArithExpr) match {
      case v: Var if rule.isDefinedAt(v) => rule(v)
    }
}
