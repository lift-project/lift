package exploration

import apart.arithmetic.{Cst, RangeMul, Var, ArithExpr}
import ir.{TypeException, ArrayType, Type}
import ir.ast.{AbstractPartRed, Get, FunCall, Expr}

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


  def findGets(expr: Expr, tupleParam: Expr): List[FunCall] = {
    Expr.visitWithState(List[FunCall]())(expr, (e, s) => {
      e match {
        case get@FunCall(Get(_), getParam) if getParam eq tupleParam => get :: s
        case _ => s
      }
    })
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
