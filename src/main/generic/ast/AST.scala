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

  abstract class Attribute extends AstNode

  abstract class Declaration extends AstNode with BlockMember

  abstract class Statement extends AstNode with BlockMember

  abstract class Expression extends AstNode

  /**
    * A function declaration
    */

  class Function(name: String, ret: Type, params: List[ParamDecl],
                 body: Block) extends Declaration {
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
        (body.visit(_)(visitFun))
    }
  }

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

//  def visitExpressionsInBlock(block: Block, fun: Expression => Unit): Unit = {
//    visitExpressionsInNode(block)

//    def visitExpressionsInNode(node: AstNode): Unit = {
//      callFunOnExpression(node)
//
//      node match {
//        case e: Expression => visitExpression(e)
//        case s: Statement => visitStatement(s)
//        case d: Declaration => visitDeclaration(d)
//        case Comment(_) =>
//      }
//    }
//    def callFunOnExpression(node: AstNode): Unit = {
//      node match {
//        case e: Expression => fun(e)
//        case _: Statement =>
//        case _: Declaration =>
//        case Comment(_) =>
//      }
//    }
//    def visitExpression(e: Expression): Unit = e match {
//      case _: ArithExpression =>
//      case _: OpenCLExpression =>
//      case a: AssignmentExpression =>
//        visitExpressionsInNode(a.value)
//        visitExpressionsInNode(a.to)
//      case c: Cast =>
//        visitExpressionsInNode(c.v)
//      case pc: PointerCast =>
//        visitExpressionsInNode(pc.v)
//      case c: CondExpression =>
//        visitExpressionsInNode(c.lhs)
//        visitExpressionsInNode(c.rhs)
//      case BinaryExpression(lhs, rhs, _) =>
//        visitExpressionsInNode(lhs)
//        visitExpressionsInNode(rhs)
//      case TernaryExpression(cond, trueExpr, falseExpr) =>
//        visitExpression(cond)
//        visitExpression(trueExpr)
//        visitExpression(falseExpr)
//      case f: FunctionCall =>
//        f.args.foreach(visitExpressionsInNode)
//      case l: Load =>
//        visitExpressionsInNode(l.v)
//        visitExpressionsInNode(l.offset)
//      case s: Store =>
//        visitExpressionsInNode(s.v)
//        visitExpressionsInNode(s.value)
//        visitExpressionsInNode(s.offset)
//      case s: StructConstructor =>
//        s.args.foreach(visitExpressionsInNode)
//      case v: VarRef =>
//        if (v.arrayIndex != null) visitExpressionsInNode(v.arrayIndex)
//    }
//    def visitStatement(s: Statement): Unit = s match {
//      case b: Block => b.content.foreach(visitExpressionsInNode)
//      case es: ExpressionStatement => visitExpressionsInNode(es.e)
//      case f: ForLoop =>
//        visitExpressionsInNode(f.init)
//        visitExpressionsInNode(f.cond)
//        visitExpressionsInNode(f.increment)
//        visitExpressionsInNode(f.body)
//      case ifte: IfThenElse =>
//        visitExpressionsInNode(ifte.cond)
//        visitExpressionsInNode(ifte.trueBody)
//        visitExpressionsInNode(ifte.falseBody)
//      case w: WhileLoop =>
//        visitExpressionsInNode(w.loopPredicate)
//        visitExpressionsInNode(w.body)
//      case GOTO(_) | TupleAlias(_, _) | TypeDef(_) | Break() =>
//    }
//    def visitDeclaration(d: Declaration): Unit = d match {
//      case f: Function => visitExpressionsInNode(f.body)
//      case v: VarDecl => if (v.init != null) visitExpressionsInNode(v.init)
//      case Label(_) | ParamDecl(_, _, _, _) =>
//    }
//  }
//  def visitBlocks(node: OclAstNode, fun: Block => Unit): Unit = {
//    node match {
//      case _: Expression => // there are no blocks inside any expressions
//
//      case s: Statement => s match {
//        case b: Block =>
//          fun(b)
//          b.content.foreach(visitBlocks(_, fun))
//        case fl: ForLoop => visitBlocks(fl.body, fun)
//        case wl: WhileLoop => visitBlocks(wl.body, fun)
//        case ifte: IfThenElse =>
//          visitBlocks(ifte.trueBody, fun)
//          visitBlocks(ifte.falseBody, fun)
//        case GOTO(_) | Barrier(_) | TypeDef(_) | TupleAlias(_, _) | ExpressionStatement(_) | Break() =>
//      }
//
//      case d: Declaration => d match {
//        case f: Function => visitBlocks(f.body, fun)
//        case Label(_) | VarDecl(_, _, _, _, _) | ParamDecl(_, _, _, _) =>
//      }
//
//      case Comment(_) | OpenCLCode(_) | OpenCLExtension(_) | RequiredWorkGroupSize(_) =>
//    }
//  }
//}

}