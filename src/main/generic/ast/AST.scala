package generic.ast

import ir.{TupleType, Type}
import lift.arithmetic._


object GenericAST {

  trait AstNode {
    def visit[T](z: T)(visitFun: (T, AstNode) => T): T
  }

  // define an implicit class called pipe that lets us write visitors
  //
  implicit class Pipe[A](a: A) {
    def |>[B](f: A => B): B = f(a)
    def pipe[B](f: A => B): B = f(a)
  }



  trait BlockMember

  trait Attribute extends AstNode

  trait Declaration extends AstNode with BlockMember

  trait Statement extends AstNode with BlockMember

  trait Expression extends AstNode

  /**
    * A function declaration
    */

  trait GenericFunction extends Declaration {
    def name : String
    def ret: Type
    def params: List[ParamDecl]
    def body: Block
    def attribute: Option[Attribute]
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        // visit the function object
        (visitFun(_, this)) |>
        // visit the parameters
        (params.foldLeft(_) {
          case (acc, node) => {
            node.visit(acc)(visitFun)
          }
        }) |>
        // visit the body
        (body.visit(_)(visitFun)) //|>
      // TODO: visit the attribute
    }
  }

  case class Function(name: String, ret: Type, params: List[ParamDecl],
                 body: Block, attribute: Option[Attribute] = None) extends
    GenericFunction


  trait GenericVarDecl extends Declaration

  case class VarDecl(v: Var,
                     t: Type,
                     init: AstNode = null,
                     length: Long = 0) extends Declaration {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        // visit the VarDecl object
        (visitFun(_, this)) |>
        // visit the initial value
        (visitFun(_, init))
    }

  }

  /** Parameter declaration. These have to be separated from variable
    * declaration since the vectorization has to be handled differently
    */
  abstract class ParamDecl(name: String, t: Type,
                           const: Boolean = false) extends Declaration {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)
  }


  /**
    * List of nodes enclosed in a bock. This behaves like (and emits) a C block.
    */
  case class Block(var content: Vector[AstNode with BlockMember] = Vector.empty,
                   global: Boolean = false) extends Statement {
    /** Append a sub-node. Could be any node, including a sub-block.
      *
      * @param node The node to add to this block.
      */
    def +=(node: AstNode with BlockMember): Unit = {
      content = content :+ node
    }

    def add(node: AstNode with BlockMember): Unit = {
      this.content :+ node
    }

    def ::(node: AstNode with BlockMember): Unit = {
      content = node +: content
    }

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (content.foldLeft(_) {
          case (acc, node) => {
            node.visit(acc)(visitFun)
          }
        })
    }
  }

  /**
    *
    * @param init      The expression/value initializing the iteration variabel. should either be an ExpressionStatement or VarDecl
    * @param cond      The condition used in the for loop
    * @param increment The expression used to increment the iteration variable
    * @param body      The loop body
    */
  case class ForLoop(init: Declaration,
                     cond: ExpressionStatement,
                     increment: Expression,
                     body: Block) extends Statement {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this))

    }
  }


  /** An alternative looping construct, using a predicate - a 'while' loop
    *
    * @param loopPredicate the predicate the loop tests each iteration
    * @param body          the body of the loop
    */
  case class WhileLoop(loopPredicate: Predicate,
                       body: Block) extends Statement {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, body))
    }
  }

  /** An if-then-else set of statements, with two branches.
    *
    * @param cond      the condition
    * @param trueBody  the body evaluated if switchPredicate is true
    * @param falseBody the body evaluated if switchPredicate is false
    */
  case class IfThenElse(cond: Expression,
                        trueBody: Block,
                        falseBody: Block = Block()) extends Statement {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, cond)) |>
        (visitFun(_, trueBody)) |>
        (visitFun(_, falseBody))
    }
  }

  /** A goto statement, targeting the label with corresponding name
    * TODO: Think of a better way of describing goto labels
    *
    * @param nameVar the name of the label to go to
    */
  case class GOTO(nameVar: Var) extends Statement {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)
  }

  case class Break() extends Statement {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)
  }

  case class TypeDef(t: Type) extends Statement {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)
  }

  case class TupleAlias(t: Type, name: String) extends Statement {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)
  }

  case class ExpressionStatement(e: Expression) extends Statement {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, e))
    }
  }


  case class FunctionCall(name: String,
                          args: List[GenericAST.AstNode]) extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (args.foldLeft(_) {
          case (acc, node) => {
            node.visit(acc)(visitFun)
          }
        })
    }
  }

  /** A reference to a declared variable
    *
    * @param v          The variable referenced.
    * @param suffix     An optional suffix appended to the name.
    *                   Used e.g. for unrolled variables in private memory.
    * @param arrayIndex Offset used to index from pointers, if any.
    * @note This uses a String instead of a Var because some nodes (like user
    *       functions), inject variables from string.
    */
  case class VarRef(v: Var,
                    suffix: String = null,
                    arrayIndex: ArithExpression = null) extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z, this)
  }

  case class Load(v: VarRef,
                  offset: ArithExpression) extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, v))
    }
  }

  case class Store(v: VarRef,
                   value: AstNode,
                   offset: ArithExpression) extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, v)) |>
        (visitFun(_, value))
    }
  }

  /** Represent an assignment.
    *
    * @param to    Left-hand side.
    * @param value Right-hand side.
    * @note Vectors are using Store instead of assignment.
    */
  case class AssignmentExpression(to: AstNode, value: AstNode) extends
    Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, to)) |>
        (visitFun(_, value))
    }
  }

  /** Wrapper for arithmetic expression
    *
    * @param content The arithmetic expression.
    */
  case class ArithExpression(var content: ArithExpr) extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)
  }

  case class BinaryExpression(lhs: Expression, rhs: Expression, op: BinaryExpression.Operator.Operator)
    extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, lhs)) |>
        (visitFun(_, rhs))
    }
  }

  object BinaryExpression {

    object Operator extends Enumeration {
      type Operator = Value
      val + = Value("+")
      val - = Value("-")
      val * = Value("*")
      val / = Value("/")
      val % = Value("%")
    }

  }

  case class CondExpression(lhs: Expression, rhs: Expression, cond: CondExpression.Operator.Operator) extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, lhs)) |>
        (visitFun(_, rhs))
    }
  }

  object CondExpression {

    /**
      * List of comparison operators
      */
    object Operator extends Enumeration {
      type Operator = Value
      val < = Value("<")
      val > = Value(">")
      val <= = Value("<=")
      val >= = Value(">=")
      val != = Value("!=")
      val == = Value("==")
      val || = Value("||")
      val && = Value("&&")
    }

  }

  case class TernaryExpression(cond: CondExpression, trueExpr: Expression, falseExpr: Expression) extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, cond)) |>
        (visitFun(_, trueExpr)) |>
        (visitFun(_, falseExpr))
    }
  }

  /** Force a cast of a variable to the given type. This is used to
    *
    * @param v A referenced variable.
    * @param t The type to cast the variable into.
    */
  case class Cast(v: VarRef, t: Type) extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, v))
    }
  }

  case class PointerCast(v: VarRef, t: Type) extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, v))
    }
  }

  case class StructConstructor(t: TupleType, args: Vector[AstNode]) extends
    Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (args.foldLeft(_) {
          case (acc, node) => {
            node.visit(acc)(visitFun)
          }
        })
    }
  }

  case class ArbitraryExpression(code: String) extends Expression {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)
  }

  /** Inline comment block.
    *
    * @param content Comment string*
    */
  case class Comment(content: String) extends AstNode with BlockMember {
    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)
  }

}