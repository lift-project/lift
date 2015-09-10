package exploration

import apart.arithmetic._
import ir.{TypeException, ArrayType, Type}
import ir.ast._

object Utils {

	def listPossiblities[T](oriList : Seq[T], optionsList : Seq[Seq[T]]) : Seq[Seq[T]] = {
	    oriList.zip(optionsList).foldLeft((Seq[T](),Seq[Seq[T]]()))((state,p) => {
	      val ori = p._1
	      val options = p._2
	      var prefix = state._1
	      var results = state._2
	      results = results.map(x => x :+ ori) // add the current original element
	      results = results ++ options.map(x => prefix :+ x) // prepand the prefix to all the current options
	      prefix = prefix :+ ori	// prepare the prefix for next time      
	      (prefix,results)	      
	    } )._2	  
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
        case f@FunCall(s: Split, x) =>
          FunCall(Split(ArithExpr.substitute(s.chunkSize, st)), x)
        case f@FunCall(s@Scatter(idx: ReorderWithStride), x) =>
          FunCall(Scatter(ReorderWithStride(ArithExpr.substitute(idx.s, st))), x)
        case f@FunCall(s@Gather(idx: ReorderWithStride), x) =>
          FunCall(Gather(ReorderWithStride(ArithExpr.substitute(idx.s, st))), x)
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
    val tunableNodes = Expr.visitWithState(List[Expr]())(expr, (x, set) => {
      if (Utils.isParametric(x)) x :: set
      else set
    })

    tunableNodes.reverse
    //          ^--- this is necessary to traverse from right to left
  }

  /** List all the tunable parameters in the lambda */
  def findTunableNodes(lambda: Lambda): List[Expr] =
    findTunableNodes(lambda.body)

  /** Extract an arithmetic expression from an expression. */
  def extractArithExpr(expr: Expr): Option[ArithExpr] = expr match {
    case f@FunCall(s: Split, _) => Some(s.chunkSize)
    case f@FunCall(Scatter(ReorderWithStride(arithExpr)), _) => Some(arithExpr)
    case f@FunCall(Gather(ReorderWithStride(arithExpr)), _) => Some(arithExpr)

    // Sanity checks: [[ReorderWithStride]] is currently the only index function defined.
    // The two tests below are just sanity checks to introduce a failure in case we add other functions.
    case FunCall(_:Scatter,_) => throw new RuntimeException("rewrite can only handle reorder function")
    case FunCall(_:Gather,_) => throw new RuntimeException("rewrite can only handle reorder function")
    case _ => None
  }

  /** Filter function to find the nodes affected by parameters */
  def isParametric(expr: Expr): Boolean = expr match {
    case _ if extractArithExpr(expr).isDefined => true
    case v: Value => true // this is necessary to propagate the parameters in the types
    case _ => false
  }

  def innerLengthOfTypeMatches(t: Type, n: ArithExpr): Boolean =
    expressionsMatch(n, getLengthOfSecondDim(t))

  def expressionsMatch(a: ArithExpr, b: ArithExpr): Boolean =
    (a, b) match {
      case (Var(_, r1), Var(_, r2)) => r1 == r2
      case (IntDiv(n1, d1), IntDiv(n2, d2)) => expressionsMatch(n1, n2) && expressionsMatch(d2, d2)
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

  def getLengthOfSecondDim(t: Type) = t match {
    case ArrayType(ArrayType(_, m), _) => m
    case _ => throw new TypeException(t, "ArrayType(ArrayType(_, _), _)")
  }

  def validSplitVariable(t: Type): ArithExpr = {
    t match {
      case ArrayType(_, len) => Var(RangeMul(Cst(1), len, Cst(2)))
      case _ => throw new TypeException(t, "ArrayType")
    }
  }

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
      case _ => result
    }
  }

  def getFinalArg(expr: Expr): Expr = {
    expr match {
      case FunCall(_, arg) => getFinalArg(arg)
      case FunCall(_ : AbstractPartRed, _, arg) => getFinalArg(arg)
      case _ => expr
    }
  }
}
