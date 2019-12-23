package rewriting.utils

import ir.ast._
import ir.{ArrayType, ArrayTypeWS, Type, TypeException}
import lift.arithmetic._

object Utils {

  private[rewriting] val mapPattern: PartialFunction[Expr, Unit] =
  { case FunCall(map: AbstractMap, _) if map.f.body.isConcrete => }

  private[rewriting] val reducePattern: PartialFunction[Expr, Unit] =
  { case FunCall(_: AbstractPartRed, _, _) => }

  private[rewriting] val splitPattern: PartialFunction[Expr, Unit] =
  { case FunCall(Split(_), _) => }

  private[rewriting] val concretePattern: PartialFunction[Expr, Unit] =
  { case call: FunCall if call.isConcrete(false) => }

  def getHash(lambda: Lambda): String =
    DumpToFile.Sha256Hash(DumpToFile.dumpLambdaToMethod(lambda))

  @scala.annotation.tailrec
  def getMapAtDepth(expr:Expr, depth: Int): Expr = {
    val outermostMap = Utils.getExprForPatternInCallChain(expr, mapPattern).get

    if (depth == 0)
      outermostMap
    else
      getMapAtDepth(getMapBody(outermostMap), depth - 1)
  }

  def getMapBody(expr: Expr): Expr = {
    expr match {
      case FunCall(m: AbstractMap, _) => m.f.body
    }
  }

  /**
   * Quick and dirty substitution of arithmetic expressions.
   * This relies on the reference on the nodes gathered in the original expression.
   * As long as we substitute from right to left, we do only shallow copies of the expression tree,
   * so it seems to work.
   * @param st Substitutions to perform.
   * @param tunableNodes Nodes where to perform the substitutions.
   * @param expr The original expression.
   * @return An expression where all the substitutions have been performed
   */
  def quickAndDirtySubstitution(st: collection.immutable.Map[ArithExpr, ArithExpr],
                                tunableNodes: List[Expr],
                                expr: Expr): Expr = {
    var tuned_expr = expr

    tunableNodes.foreach(node => {
      tuned_expr = Expr.replace(tuned_expr, node, node match {
        case FunCall(s: Split, x) =>
          FunCall(Split(ArithExpr.substitute(s.chunkSize, st)), x)
        case FunCall(Scatter(idx: ReorderWithStride), x) =>
          FunCall(Scatter(ReorderWithStride(ArithExpr.substitute(idx.s, st))), x)
        case FunCall(Gather(idx: ReorderWithStride), x) =>
          FunCall(Gather(ReorderWithStride(ArithExpr.substitute(idx.s, st))), x)
        case FunCall(sl: Slide, x) =>
          FunCall(Slide(ArithExpr.substitute(sl.size, st), ArithExpr.substitute(sl.step, st)), x)
        case v: Value =>
          Value(v.value, Type.substitute(v.t, st))
        case _ =>
          // If you end up here, it is most likely because of one of the following:
          // - a Scatter/Gather with an index function other than a ReorderWithStride
          throw new scala.RuntimeException("Cannot substitute node " + node)
      })
    })

    tuned_expr
  }

  /**
   * Quick and dirty substitution of arithmetic expressions.
   * This relies on the reference on the nodes gathered in the original lambda.
   * As long as we substitute from right to left, we do only shallow copies of the expression tree,
   * so it seems to work.
   * @param st Substitutions to perform.
   * @param tunableNodes Nodes where to perform the substitutions.
   * @param lambda The original expression.
   * @return A lambda where all the substitutions have been performed
   */
  def quickAndDirtySubstitution(st: collection.immutable.Map[ArithExpr, ArithExpr],
                                tunableNodes: List[Expr],
                                lambda: Lambda): Lambda = {
    val tuned_expr = quickAndDirtySubstitution(st, tunableNodes, lambda.body)
    Lambda(lambda.params, tuned_expr)
  }

  /** List all the tunable parameters in the expression */
  def findTunableNodes(expr: Expr): List[Expr] = {
    val tunableNodes = Expr.visitLeftToRight(List[Expr]())(expr, (x, set) => {
      if (Utils.isParametric(x)) x :: set
      else set
    })

    tunableNodes//.reverse
    //          ^--- this is necessary to traverse from right to left
  }

  /** List all the tunable parameters in the lambda */
  def findTunableNodes(lambda: Lambda): List[Expr] =
    findTunableNodes(lambda.body)

  /** Extract an arithmetic expression from an expression. */
  def extractArithExpr(expr: Expr): Option[ArithExpr] = expr match {
    case FunCall(s: Split, _) => Some(s.chunkSize)
    case FunCall(sl: Slide, _) => Some(sl.step)
    case FunCall(Scatter(ReorderWithStride(arithExpr)), _) => Some(arithExpr)
    case FunCall(Gather(ReorderWithStride(arithExpr)), _) => Some(arithExpr)
    case FunCall(Scatter(Shift(arithExpr)), _) => Some(arithExpr)

    // Sanity checks: [[ReorderWithStride]] is currently the only index function defined.
    // The two tests below are just sanity checks to introduce a failure in case we add other functions.
    case FunCall(_:Scatter,_) => throw new RuntimeException("rewrite can only handle reorder function")
    case FunCall(_:Gather,_) => throw new RuntimeException("rewrite can only handle reorder function")
    case _ => None
  }

  /** Filter function to find the nodes affected by parameters */
  def isParametric(expr: Expr): Boolean = expr match {
    case _ if extractArithExpr(expr).isDefined => true
    case _: Value => true // this is necessary to propagate the parameters in the types
    case _ => false
  }

  def innerLengthOfTypeMatches(t: Type, n: ArithExpr): Boolean =
    expressionsMatch(n, getLengthOfSecondDim(t))

  def expressionsMatch(a: ArithExpr, b: ArithExpr): Boolean =
    (a, b) match {
      case (Var(_, r1), Var(_, r2)) => r1 == r2
      case (IntDiv(n1, d1), IntDiv(n2, d2)) => expressionsMatch(n1, n2) && expressionsMatch(d1, d2)
      case (Var(_, r), n: Cst) => r.max == r.min && r.max == n
      case (n: Cst, Var(_, r)) => r.max == r.min && r.max == n
      case (i, j) => i == j
    }

  def findGets(expr: Expr, tupleParam: Expr): List[FunCall] = {
    Expr.visitWithState(List[FunCall]())(expr, (e, s) => {
      e match {
        case get@FunCall(Get(_), getParam) if getParam eq tupleParam => get :: s
        case _ => s
      }
    })
  }

  def findExpressionForPattern(lambda: Lambda, pattern: PartialFunction[Expr, Unit]): Option[Expr] =
    findExpressionForPattern(lambda.body, pattern)

  def findExpressionForPattern(expr: Expr, pattern: PartialFunction[Expr, Unit]): Option[Expr] = {
    Expr.visitWithStateDepthFirst(None: Option[Expr])(expr, (e, a) =>
      a match {
        case None if pattern.isDefinedAt(e) => Some(e)
        case _ => a
      })
  }

  def findExpressionForPatternBreadthFirst(expr: Expr, pattern: PartialFunction[Expr, Unit]): Option[Expr] = {
    Expr.visitWithState(None: Option[Expr])(expr, (e, a) =>
      a match {
        case None if pattern.isDefinedAt(e) =>
          Some(e)
        case _ => a
      })
  }

  def collect(expr: Expr, pattern: PartialFunction[Expr, Unit]): List[Expr] = {
    Expr.visitWithState(List[Expr]())(expr, (e, s) => {
      e match {
        case currentExpr if pattern.isDefinedAt(currentExpr) => currentExpr :: s
        case _ => s
      }
    })
  }

  def getLengthOfSecondDim(t: Type): ArithExpr = t match {
    case ArrayType(ArrayTypeWS(_, m)) => m
    case _ => throw new TypeException(t, "ArrayType(ArrayType(), _)", null)
  }

  def validSplitVariable(t: Type): ArithExpr = {
    t match {
      case ArrayTypeWS(_, len) => Var(RangeMul(Cst(1), len + 1, Cst(2)))
      case _ => throw new TypeException(t, "ArrayType", null)
    }
  }

  def splitVariable(given: ArithExpr, t: Type): ArithExpr =
    if (given == ?) Utils.validSplitVariable(t) else given

  def validSlideStep(t: Type, overlap: ArithExpr): ArithExpr = {
    t match {
      case ArrayTypeWS(_, s) =>
        Var(RangeMul(Cst(1), s - overlap, Cst(2)))
      case _ => throw new TypeException(t, "ArrayType", null)
    }
  }

  @scala.annotation.tailrec
  def getExprForPatternInCallChain(expr: Expr, pattern: PartialFunction[Expr, Unit]): Option[Expr] = {
    if (pattern.isDefinedAt(expr))
      Some(expr)
    else
      expr match {
        case FunCall(_, arg) => getExprForPatternInCallChain(arg, pattern)
        case FunCall(_ : AbstractPartRed, _, arg) => getExprForPatternInCallChain(arg, pattern)
        case _ => None
      }
  }

  @scala.annotation.tailrec
  def getIndexForPatternInCallChain(expr: Expr, pattern: PartialFunction[Expr, Unit], currentId: Int = 0): Int = {
    if (pattern.isDefinedAt(expr))
      currentId
    else
      expr match {
        case FunCall(_, arg) => getIndexForPatternInCallChain(arg, pattern, currentId + 1)
        case FunCall(_ : AbstractPartRed, _, arg) => getIndexForPatternInCallChain(arg, pattern, currentId + 1)
        case _ => -1
      }
  }

  @scala.annotation.tailrec
  def visitFunCallChain(expr: Expr, visitFun: Expr => Unit): Unit = {
    visitFun(expr)
    expr match {
      case FunCall(_, arg) => visitFunCallChain(arg, visitFun)
      case FunCall(_ : AbstractPartRed, _, arg) => visitFunCallChain(arg, visitFun)
      case _ =>
    }
  }

  def visitFunCallChainWithState[T](init: T)(expr: Expr, visitFun: (Expr, T) => T): T = {
    val result = visitFun(expr, init)
    expr match {
      case FunCall(_, arg) => visitFunCallChainWithState(result)(arg, visitFun)
      case FunCall(_ : AbstractPartRed, _, arg) => visitFunCallChainWithState(result)(arg, visitFun)
      case FunCall(Zip(_), args@_*) => args.foldRight(result)((arg, x) => {
        visitFunCallChainWithState(x)(arg, visitFun)
      })
      case _ => result
    }
  }

  @scala.annotation.tailrec
  def getFinalArg(expr: Expr): Expr = {
    expr match {
      case FunCall(_, arg) => getFinalArg(arg)
      case FunCall(_ : AbstractPartRed, _, arg) => getFinalArg(arg)
      case _ => expr
    }
  }

  def countMapsAtCurrentLevel(expr: Expr): Int =
    Utils.visitFunCallChainWithState(0)(expr, {
      case (FunCall(m: AbstractMap, _), count) if m.f.body.isConcrete => count + 1
      case (_, count) => count
    })

  def isTranspose(funDecl: FunDecl): Boolean =
    funDecl.isInstanceOf[Transpose] || funDecl.isInstanceOf[TransposeW]
}
