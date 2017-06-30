package ir.ast

import lift.arithmetic.ArithExpr
import ir._
import ir.interpreter.Interpreter.ValueMap
import ir.view.{AccessInfo, NoView, View}
import opencl.ir.pattern.{FilterSeq, ReduceWhileSeq}
import opencl.ir.{OpenCLAddressSpace, UndefAddressSpace}

import scala.language.implicitConversions

/** Abstract class representing all kinds of expressions, i.e.,
  *
  * - function calls: map(f, x), zip(x,y), ...
  *
  * - parameter: x, y, ...
  *
  * - values: 4, 128, ...
  */
abstract class Expr extends IRNode {
  /**
   * The type of the expression
   */
  var t: Type = UndefType

  /**
   * The memory object representing the storage of the value computed by this
   * expression
   */
  var mem: Memory = UnallocatedMemory

  var addressSpace: OpenCLAddressSpace = UndefAddressSpace

  /**
   * The view of this expression explaining how to access the memory object
   */
  var view: View = NoView

  var outputView: View = NoView

  /**
   * The context keeps track where this expression is inside a bigger
   * expression for checking (possible) constrains on nesting expression.
   */
  var context: Context = null

  /**
   * A list storing (ArrayType constructor, variable) tuples that describe the
   * full type and loop variable of the expression (i.e. the outer part not
   * included in `this`).
   *
   * Used for constructing input views.
   */
  var inputDepth: List[ir.view.SingleAccess] = List()

  var accessInf = AccessInfo()

  /**
   * A list storing (ArrayType constructor, variable) tuples that describe the
   * full type and loop variable of the expression (i.e. the outer part not
   * included in `this`).
   *
   * Used for constructing output views.
   */
  var outputDepth: List[ir.view.SingleAccess] = List()

  /**
   * Checks if the expression eventually writes to memory, i.e., it contains a
   * user function.
   * @return Returns `true` iff the expression eventually writes to memory.
   */
  def isConcrete: Boolean = isConcrete(true)

  /**
   * Checks if the expression eventually writes to memory, i.e., it contains a
   * user function.
   * @param visitArgs Should arguments be checked as well
   * @return Returns `true` iff the expression eventually writes to memory.
   */
  def isConcrete(visitArgs: Boolean): Boolean = {
    Expr.visitWithState(false)(this, (e: Expr, b: Boolean) => {
      e match {
        case call: FunCall =>
          call.f match {
            case _: UserFun | _: VectorizeUserFun => true
            case _ => b
          }
        case _ => b
      }
    }, visitArgs)
  }

  /**
   * Checks if the expression never writes to memory, i.e., it contains no user
   * function. For expressions where this method returns `true` the `view`
   * influences how following `concrete` functions will access memory.
   * @return Returns `true` iff the expression never writes to memory
   */
  def isAbstract: Boolean = !isConcrete

  /**
   * Checks if the expression never writes to memory, i.e., it contains no user
   * function. For expressions where this method returns `true` the `view`
   * influences how following `concrete` functions will access memory.
   * @param visitArgs Should arguments be checked as well
   * @return Returns `true` iff the expression never writes to memory
   */
  def isAbstract(visitArgs: Boolean): Boolean = !isConcrete(visitArgs)

  /**
   * Perform a deep copy of the expression.
   * @return A copy of `this`
   */
  def copy: Expr

  def contains(pattern: PartialFunction[Expr, Unit]) =
    Expr.visitWithState(false)(this, (e, s) => pattern.isDefinedAt(e) || s)

  /**
   * Reverse function application.
   * I.e. e :>> f == f(e).
   * @param f The function to apply to `this`.
   * @return `f.apply(this)`
   */
  def :>>(f: FunDecl) = f.apply(this)

  /**
   * (double reverse) Function application.
   * I.e. f <<: e == e.<<:(f) == f(e)
   *
   * @param f The function to apply to `this`.
   * @return `f.apply(this)`
   */
  def <<:(f: FunDecl) = f.apply(this)

  def at(i: ArithExpr) = ArrayAccess(i) $ this

  def eval(valueMap: ValueMap): Any
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
          case l: Lambda =>    visit(l.body, pre, post)
          case _ =>
        }
      case _ =>
    }
    post(expr)
  }

  /**
   * Returns an aggregated state computed by visiting the given expression
   * `expr` by recursively traversing it in a breadth-first manner
   * and calling the given `visitFun` on the visited sub expressions.
   *
   * @param z The initial state of type `T`
   * @param expr The expression to be visited.
   * @param visitFun The function to be invoked with the current expression to
   *                 visit and the current state computing an updated state.
   *                 This function is invoked before the current expression is
   *                 recursively visited.
   * @param visitArgs Should the arguments be visited
   * @tparam T The type of the state
   * @return The computed state after visiting the expression `expr` with the
   *         initial state `z`.
   */
  def visitWithState[T](z: T)(expr: Expr,
                              visitFun: (Expr, T) => T,
                              visitArgs: Boolean = true): T = {
    val result = visitFun(expr, z)
    expr match {
      case call: FunCall =>
        // visit args first
        val newResult = if (visitArgs)
          call.args.foldRight(result)((arg, x) => {
            visitWithState(x)(arg, visitFun)
          }) else result

        // do the rest ...
        call.f match {
          case rs: ReduceWhileSeq =>
            val newResult2 = visitWithState(newResult)(rs.f.body, visitFun)
            visitWithState(newResult2)(rs.p.body, visitFun)
          case fs: FilterSeq =>
            // Both the predicate and the copy function have to be visited
            val newResult2 = visitWithState(newResult)(fs.f.body, visitFun)
            if (fs.copyFun != null)
              visitWithState(newResult2)(fs.copyFun.body, visitFun)
            else
              newResult2
          case fp: FPattern =>  visitWithState(newResult)(fp.f.body, visitFun)
          case l: Lambda =>     visitWithState(newResult)(l.body, visitFun)
          case _ => newResult
        }
      case _ => result
    }
  }

  def visitLeftToRight[T](z: T)(expr: Expr,
                                visitFun: (Expr, T) => T,
                                visitArgs: Boolean = true): T = {

    expr match {
      case call: FunCall =>

        // do the rest ...
        val result = call.f match {
          case fs: FilterSeq =>
            val newResult = visitLeftToRight(z)(fs.f.body, visitFun)
            if (fs.copyFun != null)
              visitLeftToRight(newResult)(fs.copyFun.body, visitFun)
            else
              newResult
          case fp: FPattern =>  visitLeftToRight(z)(fp.f.body, visitFun)
          case l: Lambda =>     visitLeftToRight(z)(l.body, visitFun)
          case _ => z
        }

        val newResult = visitFun(expr, result)

        // visit args first
        if (visitArgs)
          call.args.foldRight(newResult)((arg, x) => {
            visitLeftToRight(x)(arg, visitFun)
          }) else newResult


      case _ => visitFun(expr, z)
    }
  }

  def visitRightToLeft[T](z: T)(expr: Expr,
                                visitFun: (Expr, T) => T): T = {

    expr match {
      case call: FunCall =>
        // visit args first
        val result =
          call.args.foldRight(z)((arg, x) => {
            visitRightToLeft(x)(arg, visitFun)
          })

        // do the rest ...
        val newResult = call.f match {
          case fs: FilterSeq =>
            val newResult2 = visitRightToLeft(result)(fs.f.body, visitFun)
            if (fs.copyFun != null)
              visitRightToLeft(newResult2)(fs.copyFun.body, visitFun)
            else
              newResult2
          case fp: FPattern =>  visitRightToLeft(result)(fp.f.body, visitFun)
          case l: Lambda =>     visitRightToLeft(result)(l.body, visitFun)
          case _ => result
        }

        visitFun(expr, newResult)

      case _ => visitFun(expr, z)
    }
  }

  /**
   * Returns an aggregated state computed by visiting the given expression
   * `expr` by recursively traversing it in a depth-first manner
   * and calling the given `visitFun` on the visited sub expressions.
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
  def visitWithStateDepthFirst[T](z: T)(expr: Expr,
                              visitFun: (Expr, T) => T): T = {
    val result = visitFun(expr, z)
    expr match {
      case call: FunCall =>
        // do the rest ...
        val newResult = call.f match {
          case fs: FilterSeq =>
            val newResult2 = visitWithStateDepthFirst(result)(fs.f.body, visitFun)
            if (fs.copyFun != null)
              visitWithStateDepthFirst(newResult2)(fs.copyFun.body, visitFun)
            else
              newResult2
          case fp: FPattern =>  visitWithStateDepthFirst(result)(fp.f.body, visitFun)
          case l: Lambda =>     visitWithStateDepthFirst(result)(l.body, visitFun)
          case _ => result
        }

        // then visit the args
        call.args.foldRight(newResult)((arg, x) => {
          visitWithStateDepthFirst(x)(arg, visitFun)
        })
      case _ => result
    }
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
  def replace(e: Expr, oldE: Expr, newE: Expr): Expr = {
    if (e.eq(oldE)) {
      newE
    } else {
      e match {
        case call: FunCall =>
          val newArgs = call.args.map((arg) => replace(arg, oldE, newE))

          val newCall = call.f match {

            case fp: FPattern =>
              // Try to do the replacement in the body
              val replaced = replace(fp.f.body, oldE, newE)

              // If replacement didn't occur return fp
              // else instantiate a new pattern with the updated lambda
              if (fp.f.body.eq(replaced))
                fp
              else
                fp.copy(Lambda(fp.f.params, replaced))

            case l: Lambda =>
              // Try to do the replacement in the body
              val replaced = replace(l.body, oldE, newE)

              // If replacement didn't occur return l
              // else instantiate the updated lambda
              if (l.body.eq(replaced))
                l
              else
                Lambda(l.params, replaced)

            case other => other
          }

          if (!newCall.eq(call.f) || (newArgs, call.args).zipped.exists( (e1, e2) => !e1.eq(e2)) ) {
            // Instantiate a new FunCall if anything has changed
            FunCall(newCall, newArgs: _*)
          } else
            e // Otherwise return the same FunCall object

        case _ => e
      }
    }
  }

  /**
   * Replace function which applies a given rewrite rule to every sub-expression
   * where the rule fires.
   *
   * @param e The expression to be recursively visited and rewritten
   * @param rule The rewrite rule to be applied
   * @return A rewritten expression where the rewrite rule has been applied to
   *         all matching subexpressions
   */
  def replace(e: Expr, oldE: Expr, rule: PartialFunction[Expr, Expr]): Expr = {
    if (e.eq(oldE)) {
      rule(e)
    } else {
      e match {
        case call: FunCall =>
          val newArgs = call.args.map((arg) => replace(arg, oldE, rule))

          val newCall = call.f match {
            case fp: FPattern =>
              // Try to do the replacement in the body
              val replaced = replace(fp.f.body, oldE, rule)

              // If replacement didn't occur return fp
              // else instantiate a new pattern with the updated lambda
              if (fp.f.body.eq(replaced))
                fp
              else
                fp.copy(Lambda(fp.f.params, replaced))

            case l: Lambda =>
              // Try to do the replacement in the body
              val replaced = replace(l.body, oldE, rule)

              // If replacement didn't occur return l
              // else instantiate the updated lambda
              if (l.body.eq(replaced))
                l
              else
                Lambda(l.params, replaced)

            case other => other
          }

          if (!newCall.eq(call.f) || newArgs != call.args) {
            // Instantiate a new FunCall if anything has changed
            FunCall(newCall, newArgs: _*)
          } else
            e // Otherwise return the same FunCall object

        case _ => e
      }
    }
  }
}
