package ir.ast

import ir.{Type, UndefType}

import scala.language.implicitConversions

/**
 * Abstract class representing a declaration.
 * Currently only a function declaration is a legal kind of declaration.
 * There are no other forms of declarations possible.
 */
abstract class Decl extends IRNode

/**
 * An instance of this class represents a function declaration.
 *
 * The type of the declared function is implemented in the type checker and not
 * here.
 *
 * @param arity The arity of the declared function
 */
abstract class FunDecl(val arity: Int) extends Decl {

  def checkType(argType: Type, setType: Boolean): Type


  /**
   * Sequentially compose with a FunDecl (convert it to a Lambda).
   * @param f A FunDecl object.
   * @return An object representing the sequential composition of `this` and `f`
   *         (wrapped in a lambda)
   */
  def comp(f: FunDecl): Lambda = this o f

  /**
   * Sequential composition operator syntax.
   * @param f The lambda expression to sequentially compose with.
   * @return An object representing the sequential function composition of
   *         `this` and `f`.
   */
  def o(f: FunDecl): Lambda = {
    val params = Array.fill(f.arity)(Param(UndefType))
    Lambda(params, this(f(params:_*)))
  }

  /**
   * Reverse function composition. I.e. g >>> f == f o g
   * @param f The function to compose with (from the right).
   * @return `f o this`
   */
  def >>>(f: FunDecl): Lambda = f o this

  /**
   * Alternative syntax for function composition.
   * I.e. g <<< f = g o f
   * @param f The function to compose with (from the left).
   * @return `this o f`
   */
  def <<<(f: FunDecl): Lambda = this o f


  /**
   * Function call. This method returns an object representing the function call
   * of `this` with `args`.
   * This method will fail at runtime if the number of given `args` does not
   * match the length of `params`!
   * @param args The arguments to call the function (`this`) with.
   * @return An object (of type FunCall) representing the function call of
   *         `this` with `args`.
   */
  def apply(args : Expr*) : Expr = {
    assert (args.length == arity)
    FunCall(this, args:_*)
  }

  /**
   * Alternative function call operator syntax. Calls `this.apply(arg)`.
   * @param arg The argument to call the function with.
   * @return An object (of type FunCall) representing the function call of
   *         `this` with `arg`.
   */
  def $(arg: Expr) : Expr = apply(arg)

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
  def replace(l: Lambda, oldE: Expr, newE: Expr) : Lambda =
    replace(l, expr => if (expr eq oldE) newE else expr)

  def replace(l: Lambda, f: Expr => Expr, exprReplacer: (Expr, Expr => Expr) => Expr = Expr.replace): Lambda = {
    val newBody = exprReplacer(l.body, f)

    val replaceInParams = l.params.exists(p => !(f(p) eq p))

    val newParams =
      if (replaceInParams)
        l.params.map(exprReplacer(_, f).asInstanceOf[Param])
      else
        l.params

    if (newBody.eq(l.body) && ! replaceInParams)
      l
    else
      Lambda(newParams, newBody)
  }

}
