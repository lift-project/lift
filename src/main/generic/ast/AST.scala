package generic.ast

import ir.{ArrayType, TupleType, Type}
import lift.arithmetic._
import opencl.generator.UseCastsForVectors

import utils.Printer

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
  *    |
  * FunctionT
  *    ^
  *    |
  *    +---------------------+
  *    |                     |
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

    // TODO: This is not the way I want to do this, but it will do for now
    def print(pc: PrintContext): Unit
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
          case None    => acc
        })
    }

    override def print(pc: PrintContext): Unit = {
      if (attribute.isDefined) attribute.get.print(pc)

      pc += Printer.toString(ret)

      pc += s" ${name}("
      params.foreach(p ⇒ {
        p.print(pc)
        pc += ", "
      })
      pc += ")"

      body.print(pc)
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

    override def print(ct: PrintContext): Unit = {
      ct += v.toString
    }
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
    val t: Type
    val init: AstNode
    val length: Long

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        // visit the VarDecl object
        (visitFun(_, this)) |>
        // visit the initial value
        (visitFun(_, init))
    }

    override def print(pc: PrintContext): Unit = {
      pc += s"${Type.getBaseType(t)} "
      v.print(pc)
      if (init != null) {
        pc += " = "
        init.print(pc)
      }
      pc += ";"
      pc.endln()
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

    override def print(pc: PrintContext): Unit = t match {
      case ArrayType(_) ⇒
        // Const restricted pointers to read-only global memory. See issue #2.
        val (constS, restrict) = if (const) ("const", "restrict") else ("", "")
        pc += constS + " " + Printer.toString(Type.devectorize(t)) +
          " " + restrict + " " + name

      case _ =>
        pc += Printer.toString(t) + " " + name
    }
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

    def ++(nodes: Vector[AstNode with BlockMember]): BlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (content.foldLeft(_) {
          case (acc, node) => {
            node.visit(acc)(visitFun)
          }
        })
    }

    override def print(pc: PrintContext): Unit = {
      pc ++= "{"
      +pc
      content.foreach(c ⇒ c.print(pc))
      -pc
      pc ++= "}"
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

    def ++(nodes: Vector[AstNode with BlockMember]): Block = this.copy(content = content ++ nodes)
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

    override def print(pc: PrintContext): Unit = {
      pc += "for ("
      init.print(pc)
      cond.print(pc)
      increment.print(pc)
      pc += ") "
      body.print(pc)
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

    override def print(pc: PrintContext): Unit = {
      pc += "while("
      pc += Printer.toString(loopPredicate)
      pc += ")"
      body.print(pc)
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

    override def print(pc: PrintContext): Unit = {
      pc += "if ("
      cond.print(pc)
      pc += ")"
      trueBody.print(pc)
      if (falseBody != Block()) {
        pc += " else "
        falseBody.print(pc)
      }
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

    override def print(pc: PrintContext): Unit = {
      pc += "goto "
      nameVar.print(pc)
      pc += ";"
      pc.endln()
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

    override def print(pc: PrintContext): Unit = {
      nameVar.print(pc)
      pc += ": ;"
      pc.endln()
    }
  }

  case class Label(nameVar: CVar) extends LabelT

  /**
    * A break statement (e.g. for exiting a loop)
    */
  trait BreakT extends StatementT {
    override def print(pc: PrintContext): Unit = pc ++= "break;"
  }

  case class Break() extends BreakT

  /**
    * Typedef statements? Type declarations?
    */
  trait TypeDefT extends StatementT {
    val t: Type

    override def print(pc: PrintContext): Unit = t match {
      case tt: TupleType ⇒
        tt.elemsT.foreach(t ⇒ TypeDef(t).print(pc))
        val name = Type.name(tt)
        val fields = tt.elemsT.zipWithIndex.map({ case (ty, i) => Type.name(ty) + " _" + i })
        pc +=
          s"""#ifndef ${name}_DEFINED
             |#define ${name}_DEFINED
             |typedef struct __attribute__((aligned(${tt.alignment._1}))) {
             |  ${fields.reduce(_ + ";\n  " + _)};
             |} $name;
             |#endif
             |""".stripMargin
      case _             ⇒ Comment("NOTE: trying to print unprintable type").print(pc)
    }
  }

  case class TypeDef(t: Type) extends TypeDefT

  /**
    * ??? Tuple aliases?
    */
  trait TupleAliasT extends StatementT {
    val t: Type
    val name: String

    override def print(pc: PrintContext): Unit = t match {
      case tt: TupleType ⇒
        pc ++= s"typedef ${Type.name(tt)} ${name};"
      case _             ⇒ Comment("NOTE: trying to print unprintable tuplealias").print(pc)
    }
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

    override def print(pc: PrintContext): Unit = {
      e.print(pc)
      pc += "; "
      pc.endln()
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

    override def print(pc: PrintContext): Unit = {
      pc += name
      pc += "("
      args.foreach({
        a ⇒
          a.print(pc)
          pc += ", "
      })
      pc += ")"
    }
  }

  case class FunctionCall(name: String,
                          args: List[GenericAST.AstNode]) extends FunctionCallT

  /**
    * A reference to a declared variable
    */
  trait VarRefT extends ExpressionT {
    val v: CVar
    val t: Type
    val suffix: String
    val arrayIndex: ArithExpression

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      visitFun(z, this) |> (visitFun(_, v))
    }

    override def print(pc: PrintContext): Unit = {
      v.print(pc)
      if (arrayIndex != null) {
        pc += "["
        arrayIndex.print(pc)
        pc += "]"
      }
      if (suffix != null) {
        pc += suffix
      }
    }
  }

  case class VarRef(v: CVar,
                    t: Type,
                    suffix: String = null,
                    arrayIndex: ArithExpression = null) extends VarRefT

  /**
    * A load from a variable, with (potentially) an offset
    */
  trait LoadT extends ExpressionT {
    val v: VarRef
    val t: Type
    val offset: ArithExpression

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, v))
    }

    override def print(pc: PrintContext): Unit = {
      if (!UseCastsForVectors()) {
        pc += s"vload${Type.getLength(t)}("
        offset.print(pc)
        pc += ","
        v.print(pc)
        pc += ")"
      } else {
        pc += s"*( ((${t}*)"
        v.print(pc)
        pc += ") + "
        offset.print(pc)
        pc += ")"
      }
    }
  }

  case class Load(v: VarRef,
                  t: Type,
                  offset: ArithExpression) extends LoadT

  /**
    * A Store into a variable with (potentially) an offset
    */
  trait StoreT extends ExpressionT {
    val v: VarRef
    val t: Type
    val value: AstNode
    val offset: ArithExpression

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, v)) |>
        (visitFun(_, value))
    }

    override def print(pc: PrintContext): Unit = {
      if (!UseCastsForVectors()) {
        pc += s"vstore${Type.getLength(t)}("
        value.print(pc)
        pc += ","
        offset.print(pc)
        pc += ","
        v.print(pc)
        pc += ")"
      } else {
        pc += s"*( ((${t}*)"
        v.print(pc)
        pc += ") + "
        offset.print(pc)
        pc += ") = "
        value.print(pc)
      }
    }
  }

  case class Store(v: VarRef,
                   t: Type,
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

    override def print(pc: PrintContext): Unit = {
      to.print(pc)
      pc += " = "
      value.print(pc)
    }
  }

  case class AssignmentExpression(to: AstNode, value: AstNode) extends
    AssignmentExpressionT

  /**
    * Wrapper for arithmetic expression
    */
  trait ArithExpressionT extends ExpressionT {
    val content: ArithExpr

    override def print(pc: PrintContext): Unit = pc += Printer.toString(content)
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

    override def print(pc: PrintContext): Unit = {
      pc += "("
      lhs.print(pc)
      pc += s" ${op.toString()} "
      rhs.print(pc)
      pc += ")"
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
        case Predicate.Operator.<  => BinaryExpressionT.Operator.<
        case Predicate.Operator.<= => BinaryExpressionT.Operator.<=
        case Predicate.Operator.== => BinaryExpressionT.Operator.==
        case Predicate.Operator.>  => BinaryExpressionT.Operator.>
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

    override def print(pc: PrintContext): Unit = {
      pc += "("
      cond.print(pc)
      pc += " ? "
      trueExpr.print(pc)
      pc += " : "
      falseExpr.print(pc)
      pc += ")"
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

    override def print(pc: PrintContext): Unit = {
      pc += s"(${t})"
      v.print(pc)
    }
  }

  /**
    * TODO: Can we actually do this? What will break :D
    */
  case class Cast(v: VarRef, t: Type) extends CastT

  case class PointerCast(v: VarRef, t: Type) extends CastT {
    override def print(pc: PrintContext): Unit = {
      pc += "("
      pc += s"(${t}*)"
      pc += Printer.toString(v.v.v)
      pc += ")"
      if (v.arrayIndex != null) {
        pc += "["
        v.arrayIndex.print(pc)
        pc += "]"
      }
      if (v.suffix != null) {
        pc += v.suffix
      }
    }
  }

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

    override def print(pc: PrintContext): Unit = {
      pc += s"(${Printer.toString(t)}){"
      args.foreach({ n ⇒
        n.print(pc)
        pc += ", "
      })
      pc += "}"
    }
  }

  case class StructConstructor(t: TupleType, args: Vector[AstNode]) extends
    StructConstructorT

  /**
    * Snippets of raw code that we might want to embed in our program
    */
  trait RawCodeT extends ExpressionT {
    val code: String

    override def print(pc: PrintContext): Unit = {
      pc += code
    }
  }

  case class RawCode(code: String) extends RawCodeT

  /**
    * Inline comment block.
    */
  trait CommentT extends AstNode with BlockMember {
    val content: String

    override def print(pc: PrintContext): Unit = {
      // TODO: Assert that the comment doesn't contain newlines
      pc += "// "
      pc += content
      pc.endln()
    }
  }

  case class Comment(content: String) extends CommentT

  /**
    * An empty block member, as a placeholder for when we want a node, but
    * don't want to print any code.
    */
  case class EmptyNode() extends AstNode with BlockMember {
    override def print(pc: PrintContext): Unit = {}
  }

}