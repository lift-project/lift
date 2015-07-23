package ir.ast

import scala.language.implicitConversions

/**
 * Abstract class representing a declaration.
 * Currently only a function declaration is a legal kind of declaration.
 * There are no other forms of declarations possible.
 */
abstract class Decl

/**
 * An instance of this class represents a function declaration.
 *
 * The type of the declared function is implemented in the type checker and not
 * here.
 *
 * @param arity The arity of the declared function
 */
abstract class FunDecl(val arity: Int) extends Decl {

  /**
   * Indicating if it is possible to generate code for this function declaration.
   * Might be overwritten by a subclass or by mixing in the `isGenerable` trait.
   */
  val isGenerable = false


  /**
   * Method to sequentially compose this instance with a given lambda expression
   * (which is also a form of function declaration).
   * @param that The lambda expression to sequentially compose with.
   * @return An object representing the sequential function composition of
   *         `this` and `that`.
   */
  def comp(that: Lambda) : CompFun = {
    // unify the representation and turn `this` in to a (sequence of) lambdas.
    // Prevent CompFunDef objects to be nested, therefore, unpack here and
    // create a new one later
    val thisFs = this match {
      case cf : CompFun => cf.funs
      case l : Lambda => Seq(l)
      case _ => Seq(Lambda.FunDefToLambda(this))
    }
    // create a function composition object with the `that` lambda appended to
    // all the other lambdas
    CompFun( thisFs :+ that:_* )
  }

  /**
   * Sequentially compose with a FunDecl (convert it to a Lambda).
   * @param f A FunDecl object.
   * @return An object representing the sequential composition of `this` and `f`
   *         (wrapped in a lambda)
   */
  def comp(f: FunDecl): CompFun = comp(Lambda.FunDefToLambda(f))

  /**
   * Sequential composition operator syntax. Calls `this.comp(f)`.
   * @param f The lambda expression to sequentially compose with.
   * @return An object representing the sequential function composition of
   *         `this` and `f`.
   */
  def o(f: Lambda): CompFun = comp(f)


  /**
   * Function call. This method returns an object representing the function call
   * of `this` with `args`.
   * This method will fail at runtime if the number of given `args` does not
   * match the length of `params`!
   * @param args The arguments to call the function (`this`) with.
   * @return An object (of type FunCall) representing the function call of
   *         `this` with `args`.
   */
  def apply(args : Expr*) : FunCall = {
    assert (args.length == arity)
    new FunCall(this, args:_*)
  }

  /**
   * Alternative function call operator syntax. Calls `this.apply(arg)`.
   * @param arg The argument to call the function with.
   * @return An object (of type FunCall) representing the function call of
   *         `this` with `arg`.
   */
  def $(arg: Expr) : FunCall = apply(arg)

  /**
   * Alternative function call method. Used by the Java bindings. Calls
   * `this.apply(arg)`
   * @param arg The argument to call the function with.
   * @return An object (of type FunCall) representing the function call of
   *         `this` with `arg`.
   */
  def call(arg: Expr) = apply(arg)
  /**
   * Alternative function call method. Used by the Java bindings. Calls
   * `this.apply(arg0, arg1)`
   * @param arg0 The first argument to call the function with.
   * @param arg1 The second argument to call the function with.
   * @return An object (of type FunCall) representing the function call of
   *         `this` with `arg0` and `arg1`.
   */
  def call(arg0: Expr, arg1: Expr) = apply(arg0, arg1)
  /**
   * Alternative function call method. Used by the Java bindings.
   * Calls `this.apply(arg0, arg1, arg2)`
   * @param arg0 The first argument to call the function with.
   * @param arg1 The second argument to call the function with.
   * @param arg2 The third argument to call the function with.
   * @return An object (of type FunCall) representing the function call of
   *         `this` with `arg0`, `arg1` and `arg2`.
   */
  def call(arg0: Expr, arg1: Expr, arg2: Expr) = apply(arg0, arg1, arg2)
}

object FunDecl {

  /**
   * Recursively visit the given lambda expression `l`, searching for `oldE`,
   * and replacing every occurrences with `newE`.
   * @param l The lambda expression to visit
   * @param oldE The expression to look for and replace
   * @param newE The expression to replace the oldE with
   * @return The lambda expression `l` where all occurrences of `oldE` are
   *         replaced with `newE`
   */
  def replace(l: Lambda, oldE: Expr, newE: Expr) : Lambda = {
    visitAndRebuild(l, (l: Lambda) => {
      if (l.body.eq(oldE)) new Lambda(l.params, newE) else l
    }, (l: Lambda) => l)
  }

  /**
   * This function returns a new lambda which bas been constructed from the
   * given lambda  `f` by recursively visiting it and applying `pre` and `post`
   * which return new lambda for a given lambda.
   *
   * The visiting works as follows:
   * 1. for a given lambda `f` the function `pre` is invoked
   * 2. the body of the return value of `pre(f)` is recursively visited calling
   *    `visitAndRebuild` on callees of every function call
   * 3. from the return value of the recursive visit a lambda is constructed and
   *    the function `post` is invoked on it.
   * 4. the return value of this invokation is returned from this function
   *
   * @param f The lambda to be visited
   * @param pre The function to apply before visiting a given (sub-) lambda
   * @param post The function to apply after visiting a given (sub-) lambda
   * @return A Lambda expression rewritten based on the functions `pre` and
   *         `post`
   */
  def visitAndRebuild(f: Lambda,
                      pre: Lambda => Lambda,
                      post: Lambda => Lambda) : Lambda = {
    val newF = pre(f)

    val newBodyFunDef : Expr = newF.body match {
      case call: FunCall => call.f match {
        case l : Lambda =>
          new Lambda(l.params,
                     visitAndRebuild(l, pre, post)(call.args:_*) ).body
        case cfd : CompFun =>
          (new CompFun(cfd.funs.map(f => visitAndRebuild(f, pre, post)):_*)
            )(call.args:_*)
        case ar: AbstractPartRed =>
          ar.getClass.getConstructor(classOf[Lambda],classOf[Value])
            .newInstance(visitAndRebuild(ar.f, pre, post),call.args.head)(call.args.tail:_*)
        case fp: FPattern =>
          fp.getClass.getConstructor(classOf[Lambda])
            .newInstance(visitAndRebuild(fp.f, pre, post))(call.args:_*)
        case _ => newF.body.copy
      }
      case _ => newF.body.copy
    }

    post(new Lambda(f.params, newBodyFunDef))
  }
}

/**
 * A trait indicating that code can be generated for this function declaration.
 */
trait isGenerable extends FunDecl {
  override val isGenerable = true
}

/**
 * An object representing a function defining a sequential composition of
 * functions.
 * @param funs The sequentially composed functions.
 */
case class CompFun(funs: Lambda*) extends FunDecl(funs.last.params.length)
                                          with isGenerable {

  /**
   * String representation of function composition.
   * @return A string representation of the function composition
   */
  override def toString: String =
    funs.map((f) =>f.toString()).reduce((s1, s2) => s1 + " o " + s2)

  /**
   * Check if the given object `that` is equal to `this`
   * @param that The given object to compare with
   * @return True iff `this` and `that` are both of type CompFunDef and their
   *         `funs` are equal
   */
  override def equals(that: Any) =
    that match {
      case cf : CompFun => funs.seq.equals(cf.funs)
      case _ => false
    }
}
