package ir.ast

import ir.{Type, UndefType}

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


  def checkType(argType: Type, setType: Boolean): Type


  /**
   * Sequentially compose with a FunDecl (convert it to a Lambda).
   * @param f A FunDecl object.
   * @return An object representing the sequential composition of `this` and `f`
   *         (wrapped in a lambda)
   */
  def comp(f: FunDecl): Lambda = this o f

  /**
   * Sequential composition operator syntax. Calls `this.comp(f)`.
   * @param f The lambda expression to sequentially compose with.
   * @return An object representing the sequential function composition of
   *         `this` and `f`.
   */
  def o(f: FunDecl): Lambda = {
    val params = Array.fill(f.arity)(Param(UndefType))
    Lambda(params, this(f(params:_*)))
  }


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
    new FunCall(this, args:_*)
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
  def replace(l: Lambda, oldE: Expr, newE: Expr) : Lambda = {
    val newBody = Expr.replace(l.body, oldE, newE)

    if (newBody.eq(l.body))
      l
    else
      Lambda(l.params, newBody)
  }

  /**
   * Recursively visit the given lambda expression `l`, searching for `oldL`,
   * and replacing every occurrences with `newL`.
   * @param toVisit The lambda expression to visit
   * @param oldL The sequence of expressions to look for and replace
   * @param newL The sequence of expressions to replace the `oldL` with
   * @return The lambda expression `l` where the occurrence of `oldL` is
   *         replaced with `newL`
   */
  def replace(toVisit: Lambda, oldL: Seq[Lambda], newL: Seq[Lambda]) : Lambda = {

    if (oldL.length == 1 && toVisit.eq(oldL.head)) {
      if (newL.length == 1)
        return newL.head
      else if (newL.isEmpty)
        return Lambda(toVisit.params,
                      Epsilon()(toVisit.body.asInstanceOf[FunCall].args:_*))
      else {// If replaced by several, compose lambdas
        val lambdaCompositon = newL.foldLeft(fun(x => x))((lhs, l) => lhs o l)
        return Lambda(toVisit.params,
                      lambdaCompositon(toVisit.body.asInstanceOf[FunCall].args:_*))
      }
    }

    toVisit.body match {
//      case FunCall(CompFun(functions @ _*), args @ _*) =>
//
//        val indexOfSlice = functions.indexOfSlice(oldL)
//
//        if (indexOfSlice == -1) {
//
//          // Not found on this level, recurse
//          val newFunctions = functions.map(cfLambda => {
//            // Attempt to replace
//            val replaced = replace(cfLambda, oldL, newL)
//
//            // If replacement didn't occur return toVisit
//            // else instantiate the updated lambda
//            if (replaced.eq(cfLambda))
//              cfLambda
//            else
//              replaced
//          })
//
//          // If replacement didn't occur return toVisit
//          // else instantiate the updated lambda
//          if (newFunctions == functions)
//            toVisit
//          else
//            Lambda(toVisit.params, FunCall(CompFun(newFunctions:_*) , args:_*))
//
//        } else {
//
//          // Attempt to replace
//          val newFunctions = functions.patch(indexOfSlice, newL, oldL.length)
//
//          if (newFunctions == functions) // If replacement didn't occur return toVisit
//            toVisit
//          else if (newFunctions.length > 1) // Instantiate new CompFun if left with several functions
//            Lambda(toVisit.params, FunCall(CompFun(newFunctions: _*), args: _*))
//          else if (newFunctions.length == 1) // Eliminate CompFun, if left with one function
//            newFunctions.head
//          else // Insert epsilon, if left with no functions
//            Lambda(toVisit.params, FunCall(Epsilon(), args: _*))
//        }


//      case FunCall(fp: FPattern, args @ _*) =>
//
//        if (Seq(fp.f) == oldL) {
//
//          // Attempt to replace
//          val reduce: Lambda = if (newL.isEmpty) Epsilon() else if (newL.length == 1) newL.head
//            else CompFun(newL:_*)
//
//          // If replacement didn't occur return toVisit
//          // else instantiate the updated lambda
//          if (reduce == fp.f)
//            toVisit
//          else
//            Lambda(toVisit.params, fp.copy(reduce)(args: _*))
//
//        } else {
//          // Recurse
//          val replaced = FunDecl.replace(fp.f, oldL, newL)
//
//          // If replacement didn't occur return toVisit
//          // else instantiate the updated lambda
//          if (replaced.eq(fp.f))
//            toVisit
//          else
//            Lambda(toVisit.params, FunCall(fp.copy(replaced), args:_*))
//
//        }

      case _ => toVisit
    }
  }
}

/**
 * A trait indicating that code can be generated for this function declaration.
 */
trait isGenerable extends FunDecl {
  override val isGenerable = true
}
