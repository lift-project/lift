package generic.ast

import ir.{TupleType, Type}
import lift.arithmetic._
import utils.PrintContext

import scala.language.implicitConversions

object GenericAST {

  // define an implicit class called pipe that lets us write visitors slightly more cleanly
  implicit class Pipe[A](a: A) {
    def |>[B](f: A => B): B = f(a)

    def pipe[B](f: A => B): B = f(a)
  }

  /*
  * The overall design of the AST is as follows: We define an "overall" AST
  * trait which we use to define types that are members of the/an AST. We
  * extend that with kinds of node (e.g. Attributes, Declarations, Statements,
  * Expressions etc) expressed as traits, which we further extend with
  * specifics, e.g. function declarations, function calls, blocks, etc.
  *
  * It is that final extension where the "magic" of this design lies: by
  * expressing leaf nodes as traits themselves, they can be extended by using
  * inheritance, and "default" implementations can easily be defined.
  *
  * A very basic example (for a function declaration):
  *
  * AstNode     BlockMember
  *    ^            ^
  *    |            |
  *    +------------+
  *    |
  * Declaration
  *    ^
  *    |
  *    |
  * FunctionT
  *    ^
  *    +---------------------+
       |                     |
  * GenericFunction     OpenCLFunction
  *
  * */

  trait AstNode {
    def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)

    def prePostVisit[T](z: T)(preVisit: (T, AstNode) ⇒ T, postVisit: (T,
      AstNode) ⇒ T): T = {
      z |> (preVisit(_, this)) |> (postVisit(_, this))
    }

    def print(pc: PrintContext) : Unit
  }

  trait BlockMember

  trait AttributeT extends AstNode

  trait DeclarationT extends AstNode with BlockMember

  trait StatementT extends AstNode with BlockMember

  trait ExpressionT extends AstNode

  /**
    * Function Declarations
    */
  trait FunctionT extends DeclarationT {
    def name: String

    def ret: Type

    def params: List[ParamDecl]

    def body: BlockT

    def attribute: Option[AttributeT]

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
        (body.visit(_)(visitFun)) |>
        // TODO: Does this attribute visitor actually work?
        (acc => attribute match {
          case Some(a) => a.visit(acc)(visitFun)
          case None => acc
        })
    }
  }

  case class Function(name: String, ret: Type, params: List[ParamDecl],
                      body: BlockT, attribute: Option[AttributeT] = None)
    extends FunctionT

  /**
    * Variables, generally
    */
  trait VarT extends DeclarationT {
    val v: lift.arithmetic.Var
//    val t: Type

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z, this)
  }

  case class CVar(v: lift.arithmetic.Var /*, t: Type*/) extends VarT

  object CVar {
    implicit def createVar(v: lift.arithmetic.Var): CVar = CVar(v)
  }

  /**
    * Variable Declarations
    */

  trait VarDeclT extends DeclarationT {
    val v: CVar
    val init: AstNode
    val length: Long

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        // visit the VarDecl object
        (visitFun(_, this)) |>
        // visit the initial value
        (visitFun(_, init))
    }
  }

  case class VarDecl(v: CVar,
                     t: Type,
                     init: AstNode = null,
                     length: Long = 0) extends VarDeclT

  /** Parameter declaration. These have to be separated from variable
    * declaration since the vectorization has to be handled differently
    * in the OpenCL AST
    */

  trait ParamDeclT extends DeclarationT {
    val name: String
    val t: Type
    val const: Boolean // = false
  }

  case class ParamDecl(name: String, t: Type,
                           const: Boolean = false) extends ParamDeclT


  /**
    * List of nodes enclosed in a bock. This behaves like (and emits) a C block.
    */

  trait BlockT extends StatementT {
    // TODO: How do we handle default values when they're vals?
    val content: Vector[AstNode with BlockMember] // = Vector.empty
    val global: Boolean // = false

    def :+(node: AstNode with BlockMember): BlockT

    def ::(node: AstNode with BlockMember): BlockT

    def ++(nodes: Vector[AstNode with BlockMember]) : BlockT

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

  case class Block(content: Vector[AstNode with BlockMember] = Vector(),
                   global: Boolean = false) extends BlockT {
    /** Append a sub-node. Could be any node, including a sub-block.
      *
      * @param node The node to add to this block.
      */
    def :+(node: AstNode with BlockMember): Block = this.copy(content = content :+ node)

    def ::(node: AstNode with BlockMember): Block = this.copy(content = node +: content)

    def ++(nodes: Vector[AstNode with BlockMember]) : Block = this.copy(content = content ++ nodes)
  }

  trait ForLoopT extends StatementT {
    val init: DeclarationT
    val cond: ExpressionStatement
    val increment: ExpressionT
    val body: BlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this))

    }
  }

  case class ForLoop(init: DeclarationT,
                     cond: ExpressionStatement,
                     increment: ExpressionT,
                     body: BlockT) extends ForLoopT


  trait WhileLoopT extends StatementT {
    val loopPredicate: Predicate
    val body: BlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, body))
    }
  }

  case class WhileLoop(loopPredicate: Predicate,
                       body: BlockT) extends WhileLoopT

  /**
    * An If-then-else sequence
    */
  trait IfThenElseT extends StatementT {
    val cond: ExpressionT
    val trueBody: BlockT
    val falseBody: BlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, cond)) |>
        (visitFun(_, trueBody)) |>
        (visitFun(_, falseBody))
    }
  }

  case class IfThenElse(cond: ExpressionT,
                        trueBody: BlockT,
                        falseBody: BlockT) extends IfThenElseT

  /** A goto statement, targeting the label with corresponding name
    * TODO: Think of a better way of describing goto labels
    */
  trait GOTOT extends StatementT {
    val nameVar: CVar

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, nameVar))
    }
  }

  case class GOTO(nameVar: CVar) extends GOTOT

  /**
    * A Label, targeted by a corresponding goto
    */
  trait LabelT extends DeclarationT {
    val nameVar: CVar

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, nameVar))
    }
  }

  case class Label(nameVar: CVar) extends LabelT

  /**
    * A break statement (e.g. for exiting a loop)
    */
  trait BreakT extends StatementT

  case class Break() extends BreakT

  /**
    * Typedef statements? Type declarations?
    */
  trait TypeDefT extends StatementT {
    val t: Type
  }

  case class TypeDef(t: Type) extends TypeDefT

  /**
    * ??? Tuple aliases?
    */
  trait TupleAliasT extends StatementT {
    val t: Type
    val name: String
  }

  case class TupleAlias(t: Type, name: String) extends TupleAliasT

  /**
    * Expression statements??
    */
  trait ExpressionStatementT extends StatementT {
    val e: ExpressionT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, e))
    }
  }

  case class ExpressionStatement(e: ExpressionT) extends ExpressionStatementT

  implicit def exprToStmt(e: ExpressionT): ExpressionStatement =
    ExpressionStatement(e)

  trait FunctionCallT extends ExpressionT {
    val name: String
    val args: List[AstNode]

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

  case class FunctionCall(name: String,
                          args: List[GenericAST.AstNode]) extends FunctionCallT

  /**
    * A reference to a declared variable
    */
  trait VarRefT extends ExpressionT {
    val v: CVar
    val suffix: String
    val arrayIndex: ArithExpression

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      visitFun(z, this) |> (visitFun(_, v))
    }
  }

  case class VarRef(v: CVar,
                    suffix: String = null,
                    arrayIndex: ArithExpression = null) extends VarRefT

  /**
    * A load from a variable, with (potentially) an offset
    */
  trait LoadT extends ExpressionT {
    val v: VarRef
    val offset: ArithExpression

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, v))
    }
  }

  case class Load(v: VarRef,
                  offset: ArithExpression) extends LoadT

  /**
    * A Store into a variable with (potentially) an offset
    */
  trait StoreT extends ExpressionT {
    val v: VarRef
    val value: AstNode
    val offset: ArithExpression

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, v)) |>
        (visitFun(_, value))
    }
  }

  case class Store(v: VarRef,
                   value: AstNode,
                   offset: ArithExpression) extends StoreT

  /**
    * Represent an assignment.
    */
  trait AssignmentExpressionT extends ExpressionT {
    val to: AstNode
    val value: AstNode

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, to)) |>
        (visitFun(_, value))
    }
  }

  case class AssignmentExpression(to: AstNode, value: AstNode) extends
    AssignmentExpressionT

  /**
    * Wrapper for arithmetic expression
    */
  trait ArithExpressionT extends ExpressionT {
    val content: ArithExpr
  }

  case class ArithExpression(content: ArithExpr) extends ArithExpressionT

  /**
    * Binary expressions
    */
  trait BinaryExpressionT extends ExpressionT {
    val lhs: ExpressionT
    val rhs: ExpressionT
    val op: BinaryExpressionT.Operator.Operator

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, lhs)) |>
        (visitFun(_, rhs))
    }
  }

  object BinaryExpressionT {

    object Operator extends Enumeration {
      type Operator = Value
      val + : BinaryExpressionT.Operator.Value = Value("+")
      val - : BinaryExpressionT.Operator.Value = Value("-")
      val * : BinaryExpressionT.Operator.Value = Value("*")
      val / : BinaryExpressionT.Operator.Value = Value("/")
      val % : BinaryExpressionT.Operator.Value = Value("%")
      val < : BinaryExpressionT.Operator.Value = Value("<")
      val > : BinaryExpressionT.Operator.Value = Value(">")
      val <= : BinaryExpressionT.Operator.Value = Value("<=")
      val >= : BinaryExpressionT.Operator.Value = Value(">=")
      val != : BinaryExpressionT.Operator.Value = Value("!=")
      val == : BinaryExpressionT.Operator.Value = Value("==")
      val || : BinaryExpressionT.Operator.Value = Value("||")
      val && : BinaryExpressionT.Operator.Value = Value("&&")
    }

  }

  case class BinaryExpression(lhs: ExpressionT,
                              op: BinaryExpressionT.Operator.Operator, rhs: ExpressionT)
    extends BinaryExpressionT

  implicit def predicateToCondExpression(p: Predicate): BinaryExpression = {
    BinaryExpression(
      ArithExpression(p.lhs),
      p.op match {
        case Predicate.Operator.!= => BinaryExpressionT.Operator.!=
        case Predicate.Operator.< => BinaryExpressionT.Operator.<
        case Predicate.Operator.<= => BinaryExpressionT.Operator.<=
        case Predicate.Operator.== => BinaryExpressionT.Operator.==
        case Predicate.Operator.> => BinaryExpressionT.Operator.>
        case Predicate.Operator.>= => BinaryExpressionT.Operator.>=
      },
      ArithExpression(p.rhs)
    )
  }

  /**
    * Ternary Expressions (i.e. cond ? trueExpr : falseExpr )
    */
  trait TernaryExpressionT extends ExpressionT {
    val cond: BinaryExpressionT
    val trueExpr: ExpressionT
    val falseExpr: ExpressionT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, cond)) |>
        (visitFun(_, trueExpr)) |>
        (visitFun(_, falseExpr))
    }
  }

  case class TernaryExpression(cond: BinaryExpressionT, trueExpr: ExpressionT, falseExpr: ExpressionT)
    extends TernaryExpressionT

  /**
    * Force a cast of a variable to the given type. This is used to
    */
  trait CastT extends ExpressionT {
    val v: VarRef
    val t: Type

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, v))
    }
  }

  /**
    * TODO: Can we actually do this? What will break :D
    */
  case class Cast(v: VarRef, t: Type) extends CastT

  case class PointerCast(v: VarRef, t: Type) extends CastT

  trait StructConstructorT extends ExpressionT {
    val t: TupleType
    val args: Vector[AstNode]

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

  case class StructConstructor(t: TupleType, args: Vector[AstNode]) extends
    StructConstructorT

  /**
    * Snippets of raw code that we might want to embed in our program
    */
  trait RawCodeT extends ExpressionT {
    val code: String
  }

  case class RawCode(code: String) extends RawCodeT

  /**
    * Inline comment block.
    */
  trait CommentT extends AstNode with BlockMember {
    val content: String
  }

  case class Comment(content: String) extends CommentT

  /**
    * An empty block member, as a placeholder for when we want a node, but
    * don't want to print any code.
    */
  case class EmptyNode() extends AstNode with BlockMember

}