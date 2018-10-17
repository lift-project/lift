package ir.ast

import ir._
import ir.interpreter.Interpreter.ValueMap
import rewriting.Rewrite
import rewriting.rules.{FissionRules, FusionRules, Rule}

import scala.collection.mutable
import scala.language.implicitConversions

/**
 * Instances of this class represent declarations (and definition) of anonymous functions (a.k.a., lambda expressions).
 * This class allows for an arbitrary number of parameters.
 * Use one of the provided subclasses to get a (runtime) checking of the number of arguments when constructing an
 * instance of such a subclass.
 *
 * @param params The parameters of the lambda expression.
 * @param body The body of the lambda expression.
 */
abstract case class Lambda private[ast] (params: Array[Param],
                                         body: Expr) extends FunDecl(params.length) {

  /**
   * Debug string representation
   */
  override def toString: String = "(\\" + params.map(_.toString).reduce(_ + ", " + _) +
      " -> \n" + body.toString.split("\n").map("  " + _ + "\n").mkString + ")"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    TypeChecker.checkAndSetTypeForParams(params, argType, this)
    TypeChecker.check(body, setType)
  }

  def rewrite(id: Int, rule: Rule): Lambda = {
    Rewrite.applyRuleAtId(this, id, rule)
  }

  def rewrite(rule: Rule, expr: Expr): Lambda = {
    Rewrite.applyRuleAt(this, expr, rule)
  }

  def rewrite(tuple: (Rule, Expr)): Lambda = {
    rewrite(tuple._1, tuple._2)
  }

  def rewriteUntilCannot(rule: Rule): Lambda = {
    Rewrite.applyRuleUntilCannot(this, rule)
  }

  def fuseMaps = rewriteUntilCannot(FusionRules.mapFusion)
  def fissionMaps = rewriteUntilCannot(FissionRules.mapFission)

  override def apply(args : Expr*) : Expr = {
    assert (args.length == arity)

    val paramUsedMultipleTimes =
      params.map(p => Expr.visitWithState(0)(body, (e, c) => {
        e match {
          case p2: Param => if (p.eq(p2)) c + 1 else c
          case _ => c
        }})).exists(_ > 1)

    // don't inline if one param is used multiple times
    val inline = !paramUsedMultipleTimes

    val allParams = args.forall(_.isInstanceOf[Param])

    if (!inline && !allParams) {
      super.apply(args:_*)
    } else {

      // 1. pair up parameters and arguments
      // 2. replace each param with the corresponding argument
      (params, args).zipped.foldLeft(body) {
        case (e, (p, a)) => Expr.replace(e, p, a)
      }
    }
  }

  /**
    * This function is used for two use-cases:
    * 1. determine arguments for kernels
    * 2. print lambdas to file during rewriting (rewriting.utils.Utils.dumpLambdaToString)
    *
    * In the first use-case we order each kernel argument by name to have consistent
    * and deterministic ordering. When printing lambdas to file we need to respect
    * dependencies among parameters:
    *
    * val M = SizeVar("M")
    * val N = Var("N", GoesToRange(M))
    * val f = \(ArrayType(Float, N), ArrayType(Float, M), ...)
    *
    * In this case M needs to be declared before N because of their dependency even though
    * M is used as the second parameter of the lambda.
    *
    * @param ordering specifies the sorting of variables either by name or by declaration order
    * @return sequence of variables used in parameters of the Lambda
    */
  def getVarsInParams(ordering: Ordering = ByName): Seq[lift.arithmetic.Var] = {
    ordering match {
      case ByName => params.flatMap(_.t.varList).sortBy(_.name).distinct
      case ByDeclarationOrder => params.flatMap(_.t.varList).distinct
    }
  }

  def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    val updatedMap =
      (params zip args).
        foldLeft(valueMap)((m, kv) => m updated (kv._1, kv._2))

    body.eval(updatedMap)
  }
}

object Lambda {

  def apply(params: Array[Param],
            body: Expr): Lambda = {
    body match {
      case FunCall(funDecl, FunCall(Lambda(lambdaParams,lambdaBody), args@_*))
        if lambdaParams.length == params.length && (lambdaParams, args).zipped.forall((a, b) => a eq b)
      =>

        val a = (lambdaParams, params).zipped.foldLeft(lambdaBody)(
            (e: Expr, pair: (Param, Param)) => Expr.replace(e, pair._1, pair._2))

        new Lambda(params, FunCall(funDecl, a)) {}
      case _ => new Lambda(params, body) {}
    }
  }

  /**
   * Implicitly wrap a given function declaration `f` into a lambda.
   *
   * @param f A given function declaration
   * @return A lambda with the same arity as `f` which immediately calls `f` in its body.
   */
  implicit def FunDefToLambda(f: FunDecl): Lambda = {
    val params = Array.fill(f.arity)(Param(UndefType))
    f match {
      case lambda@Lambda(ps, _)
        if ps.length == params.length => lambda
      case _ => Lambda(params, f(params: _*))
    }
  }

  /**
    * Make a copy of `lambda` where all type/mem/etc fields are blank.
    * Should only be called for lambdas where all `Param`s are bound.
    */
  def copyStructure(lambda: Lambda): Lambda = {

    val lambdaParams = lambda.params

    val usedParams = getUsedParams(lambda)
    val bodyParams = usedParams.filter(!lambdaParams.contains(_))
    val declaredParams = getDeclaredParams(lambda)

    val undeclaredParams = usedParams.filterNot(_.isInstanceOf[Value]).diff(declaredParams)
    if (undeclaredParams.nonEmpty)
      throw new IllegalArgumentException(s"Some Param (${undeclaredParams.mkString(", ")}) have not been declared!")

    val bodyParamsMap = bodyParams.map((_, Param())).toMap

    val lambdaParamsMap = lambdaParams.map(oldParam => (oldParam: Expr, Param(oldParam.t))).toMap

    val allParamsMap: collection.Map[Expr, Expr] = lambdaParamsMap ++ bodyParamsMap

    val replaceFun: Expr => Expr = {
      case Value(value, typ) => Value(value, typ)
      case expr => allParamsMap.getOrElse(expr, expr)
    }

    val newLambda = FunDecl.replace(lambda, replaceFun)

    assert(!newLambda.eq(lambda))

    newLambda
  }

  private def getUsedParams(lambda: Lambda) = {
    Expr.visitWithState(Set[Param]())(lambda.body, {
      case (param: Param, set) => set + param
      case (_, set) => set
    })
  }

  private def getDeclaredParams(lambda: Lambda) = {
    Expr.visitWithState(Set[Param]())(lambda.body, {
      case (FunCall(Lambda(params, _), _*), set) => set ++ params
      case (FunCall(fp: FPattern, _*), set) => set ++ fp.f.params
      case (_, set) => set
    }) ++ lambda.params
  }
}

/**
 * A "lambda" expression of arity 0. Basically an expression wrapped into a
 * `Lambda` so it can be compiled and passed to the executor.
 * @param body The body of the lambda expression.
 */
class Lambda0(override val body: Expr) extends Lambda(Array(), body)

object Lambda0 {
  def apply(expr: Expr) = new Lambda0(expr)
  def unapply(l: Lambda0): Option[Expr] = Some(l.body)
}

/**
 * A lambda expression of arity 1.
 * @param params The parameters of the lambda expression.
 * @param body The body of the lambda expression.
 */
class Lambda1(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 1)
}

object Lambda1 {
  /**
   * Implicitly wrap a given function declaration `f` into a lambda.
   *
   * If `f` has arity  1 the returned lambda immediately calls `f` in its body.
   *
   * If `f` has arity >1 the returned lambda will unpack (or, 'uncurry') its single parameter
   * (under the assumption that this is a tuple) and pass all components of the tuple as separate parameters to the
   * function `f`.
   *
   * @param f A given function declaration of arity >=1.
   * @return A lambda with arity 1 which calls `f` in its body.
   */
  implicit def FunDefToLambda(f: FunDecl): Lambda1 = {
    assert(f.arity >= 1)
    if (f.arity == 1) {
      f match {
        case Lambda(params, body) =>
          // Don't wrap unnecessarily
          new Lambda1(params, body)
        case _ => fun(f(_))
      }
    } else {
      fun( x => f( (0 until f.arity).map( Get(x, _) ):_* ) )
    }
  }

  def unapply(arg: Lambda1): Option[(Array[Param], Expr)] =
    Some(arg.params, arg.body)
}

/**
 * A lambda expression of arity 2.
 * @param params The parameters of the lambda expression.
 * @param body The body of the lambda expression.
 */
class Lambda2(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 2)

  /**
   * Returns a curried lambda expression, i.e.,  lambda expression of arity 1, where the first argument of `this` is
   * bound with `arg`.
   * @param arg The argument to be bound to the first parameter
   * @return A lambda expression of arity 1 immediately calling `this` where the first parameter is bound to `arg`
   */
  def apply(arg: Expr): Lambda1 = {
    fun( tmp => super.apply(arg, tmp) )
  }
}

object Lambda2 {
  implicit def FunDefToLambda(f: FunDecl): Lambda2 = {
    assert(f.arity == 2)
    f match {
      // Don't wrap unnecessarily
      case Lambda(params, body) => new Lambda2(params, body)
      case _ => fun(f(_, _))
    }
  }
}

class Lambda3(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 3)

  def apply(arg0: Expr, arg1: Expr): Lambda1 = {
    fun( tmp => super.apply(arg0, arg1, tmp) )
  }

  def apply(arg: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg, tmp0, tmp1) )
  }
}

class Lambda4(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 4)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda1 = {
    fun( tmp => super.apply(arg0, arg1, arg2, tmp) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, tmp0, tmp1) )
  }

  def apply(arg: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg, tmp0, tmp1, tmp2) )
  }
}

class Lambda5(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 5)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda1 = {
    fun( tmp => super.apply(arg0, arg1, arg2, arg3, tmp) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, tmp0, tmp1, tmp2) )
  }

  def apply(arg: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg, tmp0, tmp1, tmp2, tmp3) )
  }
}

class Lambda6(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 6)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr): Lambda1 = {
    fun( tmp => super.apply(arg0, arg1, arg2, arg3, arg4, tmp) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, arg3, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, arg2, tmp0, tmp1, tmp2) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg0, arg1, tmp0, tmp1, tmp2, tmp3) )
  }

  def apply(arg: Expr): Lambda5 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4) => super.apply(arg, tmp0, tmp1, tmp2, tmp3, tmp4) )
  }
}

class Lambda7(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 7)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr): Lambda1 = {
    fun( (tmp0) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5,  tmp0) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, arg3, arg4, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, arg2, arg3, tmp0, tmp1, tmp2) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg0, arg1, arg2, tmp0, tmp1, tmp2, tmp3) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda5 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4) => super.apply(arg0, arg1, tmp0, tmp1, tmp2, tmp3, tmp4) )
  }

  def apply(arg: Expr): Lambda6 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) => super.apply(arg, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) )
  }
}

class Lambda8(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 8)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr): Lambda1 = {
    fun( (tmp0) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, tmp0) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5,  tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, arg2, arg3, arg4, tmp0, tmp1, tmp2) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg0, arg1, arg2, arg3, tmp0, tmp1, tmp2, tmp3) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda5 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4) => super.apply(arg0, arg1, arg2, tmp0, tmp1, tmp2, tmp3, tmp4) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda6 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) => super.apply(arg0, arg1, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) )
  }

  def apply(arg: Expr): Lambda7 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) => super.apply(arg, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) )
  }
}

class Lambda9(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 9)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr): Lambda1 = {
    fun( (tmp0) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, tmp0) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5,  tmp0, tmp1, tmp2) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg0, arg1, arg2, arg3, arg4, tmp0, tmp1, tmp2, tmp3) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda5 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4) => super.apply(arg0, arg1, arg2, arg3, tmp0, tmp1, tmp2, tmp3, tmp4) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda6 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) => super.apply(arg0, arg1, arg2, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda7 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) => super.apply(arg0, arg1, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) )
  }

  def apply(arg: Expr): Lambda8 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) => super.apply(arg, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) )
  }
}

class Lambda10(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 10)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr): Lambda1 = {
    fun( (tmp0) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, tmp0) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6,  tmp0, tmp1, tmp2) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, tmp0, tmp1, tmp2, tmp3) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr): Lambda5 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4) => super.apply(arg0, arg1, arg2, arg3, arg4, tmp0, tmp1, tmp2, tmp3, tmp4) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda6 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) => super.apply(arg0, arg1, arg2, arg3, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda7 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) => super.apply(arg0, arg1, arg2, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) )
  }

  def apply(arg: Expr, arg1: Expr): Lambda8 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) => super.apply(arg, arg1, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) )
  }

  def apply(arg: Expr): Lambda9 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) => super.apply(arg, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) )
  }
}

class Lambda11(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 11)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr): Lambda1 = {
    fun( (tmp0) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, tmp0) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, tmp0, tmp1, tmp2) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, tmp0, tmp1, tmp2, tmp3) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr): Lambda5 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, tmp0, tmp1, tmp2, tmp3, tmp4) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr): Lambda6 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) => super.apply(arg0, arg1, arg2, arg3, arg4, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda7 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) => super.apply(arg0, arg1, arg2, arg3, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda8 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) => super.apply(arg0, arg1, arg2, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda9 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) => super.apply(arg0, arg1, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) )
  }

  def apply(arg: Expr): Lambda10 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) => super.apply(arg, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) )
  }
}

class Lambda12(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 12)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr, arg10: Expr): Lambda1 = {
    fun( (tmp0) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, tmp0) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, tmp0, tmp1, tmp2) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, tmp0, tmp1, tmp2, tmp3) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr): Lambda5 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, tmp0, tmp1, tmp2, tmp3, tmp4) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr): Lambda6 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr): Lambda7 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) => super.apply(arg0, arg1, arg2, arg3, arg4, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda8 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) => super.apply(arg0, arg1, arg2, arg3, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda9 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) => super.apply(arg0, arg1, arg2, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda10 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) => super.apply(arg0, arg1, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) )
  }

  def apply(arg0: Expr): Lambda11 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10) => super.apply(arg0, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10) )
  }
}

class Lambda13(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 13)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr, arg10: Expr, arg11: Expr): Lambda1 = {
    fun( (tmp0) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, tmp0) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr, arg10: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, tmp0, tmp1, tmp2) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, tmp0, tmp1, tmp2, tmp3) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr): Lambda5 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, tmp0, tmp1, tmp2, tmp3, tmp4) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr): Lambda6 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr): Lambda7 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr): Lambda8 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) => super.apply(arg0, arg1, arg2, arg3, arg4, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda9 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) => super.apply(arg0, arg1, arg2, arg3, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda10 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) => super.apply(arg0, arg1, arg2, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda11 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10) => super.apply(arg0, arg1, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10) )
  }

  def apply(arg0: Expr): Lambda12 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11) => super.apply(arg0, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11) )
  }
}

class Lambda14(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 14)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr, arg10: Expr, arg11: Expr, arg12: Expr): Lambda1 = {
    fun( (tmp0) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, tmp0) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr, arg10: Expr, arg11: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr, arg10: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, tmp0, tmp1, tmp2) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, tmp0, tmp1, tmp2, tmp3) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr): Lambda5 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, tmp0, tmp1, tmp2, tmp3, tmp4) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr): Lambda6 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr): Lambda7 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr): Lambda8 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr): Lambda9 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) => super.apply(arg0, arg1, arg2, arg3, arg4, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda10 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) => super.apply(arg0, arg1, arg2, arg3, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda11 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10) => super.apply(arg0, arg1, arg2, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda12 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11) => super.apply(arg0, arg1, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11) )
  }

  def apply(arg0: Expr): Lambda13 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12) => super.apply(arg0, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12) )
  }
}

class Lambda15(override val params: Array[Param], override val body: Expr) extends Lambda(params, body) {
  assert(params.length == 15)

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr, arg10: Expr, arg11: Expr, arg12: Expr, arg13: Expr): Lambda1 = {
    fun( (tmp0) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, tmp0) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr, arg10: Expr, arg11: Expr, arg12: Expr): Lambda2 = {
    fun( (tmp0, tmp1) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, tmp0, tmp1) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr, arg10: Expr, arg11: Expr): Lambda3 = {
    fun( (tmp0, tmp1, tmp2) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, tmp0, tmp1, tmp2) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr, arg10: Expr): Lambda4 = {
    fun( (tmp0, tmp1, tmp2, tmp3) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, tmp0, tmp1, tmp2, tmp3) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr, arg9: Expr): Lambda5 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, tmp0, tmp1, tmp2, tmp3, tmp4) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr, arg8: Expr): Lambda6 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr, arg7: Expr): Lambda7 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr, arg6: Expr): Lambda8 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, arg6, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr, arg5: Expr): Lambda9 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) => super.apply(arg0, arg1, arg2, arg3, arg4, arg5, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr, arg4: Expr): Lambda10 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) => super.apply(arg0, arg1, arg2, arg3, arg4, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Lambda11 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10) => super.apply(arg0, arg1, arg2, arg3, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10) )
  }

  def apply(arg0: Expr, arg1: Expr, arg2: Expr): Lambda12 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11) => super.apply(arg0, arg1, arg2, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11) )
  }

  def apply(arg0: Expr, arg1: Expr): Lambda13 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12) => super.apply(arg0, arg1, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12) )
  }

  def apply(arg0: Expr): Lambda14 = {
    fun( (tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13) => super.apply(arg0, tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmp9, tmp10, tmp11, tmp12, tmp13) )
  }
}

trait funDef {
  def apply(expr: Expr): Lambda0 = Lambda0(expr)

  def apply(f: (Param) => Expr): Lambda1 = {
    val params = Array(Param(UndefType))
    new Lambda1(params, f(params(0)))
  }

  def apply(f: (Param, Param) => Expr): Lambda2 = {
    val params = Array(Param(UndefType), Param(UndefType))
    new Lambda2(params, f(params(0), params(1)))
  }

  def apply(f: (Param, Param, Param) => Expr): Lambda3 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda3(params, f(params(0), params(1), params(2)))
  }

  def apply(f: (Param, Param, Param, Param) => Expr): Lambda4 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda4(params, f(params(0), params(1), params(2), params(3)))
  }

  def apply(f: (Param, Param, Param, Param, Param) => Expr): Lambda5 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda5(params, f(params(0), params(1), params(2), params(3), params(4)))
  }

  def apply(f: (Param, Param, Param, Param, Param, Param) => Expr): Lambda6 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda6(params, f(params(0), params(1), params(2), params(3), params(4), params(5)))
  }

  def apply(f: (Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda7 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda7(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6)))
  }

  def apply(f: (Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda8 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda8(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7)))
  }

  def apply(f: (Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda9 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda9(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8)))
  }

  def apply(f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda10 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda10(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9)))
  }

  def apply(f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda11 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda11(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10)))
  }

  def apply(f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda12 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda12(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10), params(11)))
  }

  def apply(f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda13 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda13(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10), params(11), params(12)))
  }

  def apply(f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda14 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda14(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10), params(11), params(12), params(13)))
  }

  def apply(f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda15 = {
    val params = Array(Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType), Param(UndefType))
    new Lambda15(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10), params(11), params(12), params(13), params(14)))
  }

  def apply(t: Type, f: (Param) => Expr): Lambda1 = {
    val params = Array(Param(t))
    new Lambda1(params, f(params(0)))
  }

  def apply(t1: Type, t2: Type, f: (Param, Param) => Expr): Lambda2 = {
    val params = Array(Param(t1), Param(t2))
    new Lambda2(params, f(params(0), params(1)))
  }

  def apply(t1: Type, t2: Type, t3: Type, f: (Param, Param, Param) => Expr): Lambda3 = {
    val params = Array(Param(t1), Param(t2), Param(t3))
    new Lambda3(params, f(params(0), params(1), params(2)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, f: (Param, Param, Param, Param) => Expr): Lambda4 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4))
    new Lambda4(params, f(params(0), params(1), params(2), params(3)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, f: (Param, Param, Param, Param, Param) => Expr): Lambda5 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5))
    new Lambda5(params, f(params(0), params(1), params(2), params(3), params(4)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, f: (Param, Param, Param, Param, Param, Param) => Expr): Lambda6 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6))
    new Lambda6(params, f(params(0), params(1), params(2), params(3), params(4), params(5)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, f: (Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda7 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7))
    new Lambda7(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, f: (Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda8 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7), Param(t8))
    new Lambda8(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9:Type, f: (Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda9 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7), Param(t8), Param(t9))
    new Lambda9(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9:Type, t10:Type, f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda10 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7), Param(t8), Param(t9), Param(t10))
    new Lambda10(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9:Type, t10:Type, t11:Type, f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda11 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7), Param(t8), Param(t9), Param(t10), Param(t11))
    new Lambda11(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9:Type, t10:Type, t11:Type, t12:Type, f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda12 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7), Param(t8), Param(t9), Param(t10), Param(t11), Param(t12))
    new Lambda12(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10), params(11)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9:Type, t10:Type, t11:Type, t12:Type, t13: Type, f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda13 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7), Param(t8), Param(t9), Param(t10), Param(t11), Param(t12), Param(t13))
    new Lambda13(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10), params(11), params(12)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9:Type, t10:Type, t11:Type, t12:Type, t13: Type, t14: Type, f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda14 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7), Param(t8), Param(t9), Param(t10), Param(t11), Param(t12), Param(t13), Param(t14))
    new Lambda14(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10), params(11), params(12), params(13)))
  }

  def apply(t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9:Type, t10:Type, t11:Type, t12:Type, t13: Type, t14: Type, t15: Type, f: (Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param, Param) => Expr): Lambda15 = {
    val params = Array(Param(t1), Param(t2), Param(t3), Param(t4), Param(t5), Param(t6), Param(t7), Param(t8), Param(t9), Param(t10), Param(t11), Param(t12), Param(t13), Param(t14), Param(t15))
    new Lambda15(params, f(params(0), params(1), params(2), params(3), params(4), params(5), params(6), params(7), params(8), params(9), params(10), params(11), params(12), params(13), params(14)))
  }
}

// three names for the same thing:
object fun extends funDef
object \ extends funDef
object λ extends funDef
