package ir.ast

import apart.arithmetic.ArithExpr
import ir._
import ir.view.{NoView, View}

import scala.language.implicitConversions

/** Abstract class representing all kinds of expressions, i.e.,
  *
  * - function calls: map(f, x), zip(x,y), ...
  *
  * - parameter: x, y, ...
  *
  * - values: 4, 128, ...
  */
abstract class Expr {
  /**
   * The type of the expression
   */
  var t: Type = UndefType

  /**
   * The memory object representing the storage of the value computed by this
   * expression
   */
  var mem: Memory = UnallocatedMemory

  /**
   * The view of this expression explaining how to access the memory object
   */
  var view: View = NoView

  /**
   * The context keeps track where this expression is inside a bigger
   * expression for checking (possible) constrains on nesting expression.
   */
  var context: Context = null

  /**
   * A list storing variable, length pairs that describe the full type and loop variables
   * of the expression (i.e. the outer part not included in `this`).
   *
   * Used for constructing input views.
   */
  var inputDepth: List[(ArithExpr, ArithExpr)] = List()

  /**
   * A list storing variable, length pairs that describe the full type and loop variables
   * of the expression (i.e. the outer part not included in `this`).
   *
   * Used for constructing output views.
   */
  var outputDepth: List[(ArithExpr, ArithExpr)] = List()

  /**
   * Checks if the expression eventually writes to memory, i.e., it contains a
   * user function.
   * @return Returns `true` iff the expression eventually writes to memory.
   */
  def isConcrete: Boolean = {
    Expr.visitWithState(false)(this, (e: Expr, b: Boolean) => {
      e match {
        case call: FunCall =>
          call.f match {
            case _: UserFun => true
            case _ => b
          }
        case _ => b
      }
    })
  }

  /**
   * Checks if the expression never writes to memory, i.e., it contains no user
   * function. For expressions where this method returns `true` the `view`
   * influences how following `concrete` functions will access memory.
   * @return Returns `true` iff the expression never writes to memory
   */
  def isAbstract: Boolean = !isConcrete

  /**
   * Perform a deep copy of the expression.
   * @return A copy of `this`
   */
  def copy: Expr
}

object Expr {

  /**
   * Visit the given expression `expr` by recursively traversing it.
   *
   * Invoking the given function `pre` on a given expression before recursively
   * traversing it.
   * Invoking the given function `post` on a given expression after recursively
   * traversing it.
   *
   * This function returns nothing. Therefore, `pre` or `post` usually have a
   * side effect (e.g. printing a given expression).
   *
   * @param expr The expression to be visited.
   * @param pre The function to be invoked before traversing a given expression
   * @param post The function to be invoked after traversing a given expression
   */
  def visit(expr: Expr, pre: Expr => Unit, post: Expr => Unit): Unit = {
    pre(expr)
    expr match {
      case call: FunCall =>
        call.args.foreach((arg) => visit(arg, pre, post))

        call.f match {
          case fp: FPattern => visit(fp.f.body, pre, post)
          case l: Lambda => visit(l.body, pre, post)
          case cf: CompFun =>
            cf.funs.reverseMap(inF => visit(inF.body, pre, post))
          case _ =>
        }
      case _ =>
    }
    post(expr)
  }

  /**
   * Returns an aggregated state computed by visiting the given expression
   * `expr` by recursively traversing it and calling the given `visitFun` on the
   * visited sub expressions.
   *
   * @param z The initial state of type `T`
   * @param expr The expression to be visited.
   * @param visitFun The function to be invoked with the current expression to
   *                 visit and the current state computing an updated state.
   *                 This function is invoked before the current expression is
   *                 recursively visited.
   * @tparam T The type of the state
   * @return The computed state after visiting the expression `expr` with the
   *         initial state `z`.
   */
  def visitWithState[T](z: T)(expr: Expr, visitFun: (Expr, T) => T): T = {
    val result = visitFun(expr, z)
    expr match {
      case call: FunCall =>
        // visit args first
        val newResult =
          call.args.foldRight(result)((arg, x) => {
            visitWithState(x)(arg, visitFun)
          })

        // do the rest ...
        call.f match {
          case fp: FPattern => visitWithState(newResult)(fp.f.body, visitFun)
          case cf: CompFun =>
            cf.funs.foldRight(newResult)((inF, x) => {
              visitWithState(x)(inF.body, visitFun)
            })
          case l: Lambda => visitWithState(newResult)(l.body, visitFun)
          case _ => newResult
        }
      case _ => result
    }
  }

  /**
   * This function returns a new expression which has been constructed from the
   * given expression `expr` by recursively visiting it and applying `pre` and
   * `post` which return new expressions for a given expression.
   *
   * The visiting works as follows:
   * 1. for the given expression `expr` the function `pre` is invoked
   * 2. the return value of `pre(expr)` is recursively visited
   * 3. on the return value from the recursive visit the function `post` is
   *    invoked and its return value is returned from this function
   *
   * @param expr The 'source' expression to be visited
   * @param pre The function to be invoked on a given expression before it is
   *            recursively visited. The return value of this function is then
   *            recursively visited.
   * @param post The function to be invoked on a given expression after it has
   *             been recursively visited.
   * @return The rebuild expression after recursively applying `pre` and `post`
   *         to `expr`.
   */
  def visitAndRebuild(expr: Expr,
                      pre:  Expr => Expr,
                      post: Expr => Expr): Expr = {
    var newExpr = pre(expr)
    newExpr = newExpr match {
      case call: FunCall =>
        val newArgs = call.args.map((arg) => visitAndRebuild(arg, pre, post))
        call.f match {
          case cf: CompFun =>
            CompFun(
              cf.funs.map(
                inF => new Lambda(inF.params,
                  visitAndRebuild(inF.body, pre, post))): _*)
              .apply(newArgs: _*)

          case ar: AbstractPartRed =>
            ar.getClass.getConstructor(classOf[Lambda], classOf[Value])
              .newInstance(visitAndRebuild(ar.f.body, pre, post), ar.init)
              .apply(newArgs: _*)

          case fp: FPattern =>
            fp.getClass.getConstructor(classOf[Expr])
              .newInstance(visitAndRebuild(fp.f.body, pre, post))
              .apply(newArgs: _*)

          case _ => newExpr.copy
        }
      case _ => newExpr.copy
    }
    post(newExpr)
  }

  /**
   * Convenient function for replacing a single expression (`oldE`) with a given
   * new expression (`newE`) in an expression to be recursively visited (`e`).
   *
   * @param e The 'source' expression to be visited
   * @param oldE The expression to be replaced in `e`
   * @param newE The expression to replace `oldE`
   * @return The rebuild expression from `e` where `oldE` has be replaced with
   *         `newE`
   */
  def replace(e: Expr, oldE: Expr, newE: Expr): Expr =
    visitAndRebuild(e, (e: Expr) => if (e.eq(oldE)) newE else oldE,
                       (e: Expr) => e)
}
