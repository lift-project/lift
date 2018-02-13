package generic.ast

import ir.{ArrayType, TupleType, Type}
import lift.arithmetic._
import opencl.generator.UseCastsForVectors

import PrettyPrinter._

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

  sealed trait AstNode {
    def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)

    def prePostVisit[T](z: T)(preVisit: (T, AstNode) ⇒ T, postVisit: (T,
      AstNode) ⇒ T): T = {
      z |> (preVisit(_, this)) |> (postVisit(_, this))
    }

    def print(): Doc
  }

  trait BlockMember

  trait AttributeT extends AstNode

  trait DeclarationT extends AstNode with BlockMember

  trait StatementT extends AstNode with BlockMember

  trait ExpressionT extends AstNode

  /*
  Function Declaration trait
   */
  trait FunctionT extends DeclarationT {
    def name: String

    def ret: Type

    def params: List[ParamDeclT]

    def body: MutableBlockT

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

    override def print(): Doc = {
      // print the attribute if it's defined
      (attribute match {
        case Some(a) ⇒ a.print() <> " "
        case None    ⇒ Empty()
      }) <>
        // print the return type and name
        Printer.toString(ret) <+> name <>
        // print parameters
        "(" <>
        intersperse(params.map(_.print())) <>
        ")" <>
        // print the body
        bracket("{", body.print(), "}")

    }
  }

  case class Function(name: String, ret: Type, params: List[ParamDeclT],
                      body: MutableBlockT, attribute: Option[AttributeT] = None)
    extends FunctionT

  /*
  Variables trait
   */
  trait VarT extends DeclarationT {
    val v: lift.arithmetic.Var
    //    val t: Type

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z, this)

    override def printStatefully(ct: PrintContext): Unit = {
      ct += Printer.toString(v)
    }

    override def print(): Doc = {
      text(Printer.toString(v))
    }
  }

  case class CVar(v: lift.arithmetic.Var /*, t: Type*/) extends VarT

  object CVar {
    implicit def createVar(v: lift.arithmetic.Var): CVar = CVar(v)
  }

  /*
  Variable declarations
   */
  trait VarDeclT extends DeclarationT {
    val v: CVar
    val t: Type
    val init: Option[AstNode]
    val length: Long

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        // visit the VarDecl object
        (visitFun(_, this)) |>
        // visit the initial value
        (_z ⇒ init.map(visitFun(_z, _)).getOrElse(_z))
    }

    override def printStatefully(pc: PrintContext): Unit = {
      pc.newln()
      pc += s"${Type.getBaseType(t)} "
      v.printStatefully(pc)
      init match {
        case Some(i) ⇒ {
          pc += " = "
          i.printStatefully(pc)
        }
        case None    ⇒
      }
      pc += ";"
    }

    override def print(): Doc = {
      // print the type
      Printer.toString(Type.getBaseType(t)) <+>
        //print the variable name
        v.print() <>
        // print the assignment
        init.map(text(" = ") <> _.print()).getOrElse(nil) <>
        // end the line
        ";"
    }
  }

  case class VarDecl(v: CVar,
                     t: Type,
                     init: Option[AstNode] = None,
                     length: Long = 0) extends VarDeclT

  /*
  Parameter declaration. These have to be separated from variable
  declaration since the vectorization has to be handled differently
  in the OpenCL AST
    */
  trait ParamDeclT extends DeclarationT {
    val name: String
    val t: Type
    val const: Boolean // = false

    override def printStatefully(pc: PrintContext): Unit = t match {
      case ArrayType(_) ⇒
        // Const restricted pointers to read-only global memory. See issue #2.
        val (constS, restrict) = if (const) ("const", "restrict") else ("", "")
        pc += constS + " " + Printer.toString(Type.devectorize(t)) +
          " " + restrict + " " + name

      case _ =>
        pc += Printer.toString(t) + " " + name
    }

    override def print(): Doc = t match {
      case ArrayType(_) ⇒
        val (constS, restrict) = if (const) (text("const"), text("restrict"))
        else (nil,
          nil)
        // const type restrict name
        constS <+>
          Printer.toString(Type.devectorize(t)) <+>
          restrict <+>
          name
      case _            ⇒
        text(Printer.toString(t)) <+> name
    }
  }

  case class ParamDecl(name: String, t: Type,
                       const: Boolean = false) extends ParamDeclT


  /*
  List of nodes enclosed in a bock. This behaves like (and emits) a C block.
   */
  trait MutableBlockT extends StatementT {
    // TODO: How do we handle default values when they're vals?
    var content: Vector[AstNode with BlockMember] // = Vector.empty
    val global: Boolean // = false

    def :+(node: AstNode with BlockMember): MutableBlockT

    def ::(node: AstNode with BlockMember): MutableBlockT

    def ++(nodes: Vector[AstNode with BlockMember]): MutableBlockT

    def +=(node: AstNode with BlockMember): Unit = {
      content = content :+ node
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

    override def printStatefully(pc: PrintContext): Unit = {
      if (!global) pc ++= "{"
      +pc
      content.foreach({
        pc.newln()
        c ⇒ c.printStatefully(pc)
      })
      -pc
      if (!global) pc ++= "}"
    }

    override def print(): Doc = {
      // pre-calculate our inner block
      val innerBlock = intersperse(content.map(_.print()).toList,
        Line())
      // if we're global, bracket it, otherwise, don't
      if (global) {
        innerBlock
      } else {
        bracket("{", innerBlock, "}")
      }
    }
  }

  case class MutableBlock(override var content: Vector[AstNode with
    BlockMember] = Vector(), global: Boolean = false) extends MutableBlockT {
    /** Append a sub-node. Could be any node, including a sub-block.
      *
      * @param node The node to add to this block.
      */
    def :+(node: AstNode with BlockMember): MutableBlock = this.copy(content = content :+ node)

    def ::(node: AstNode with BlockMember): MutableBlock = this.copy(content = node +: content)

    def ++(nodes: Vector[AstNode with BlockMember]): MutableBlock = this.copy(content = content ++ nodes)
  }

  /*
  For loop
   */
  trait ForLoopT extends StatementT {
    val init: DeclarationT
    val cond: ExpressionStatement
    val increment: ExpressionT
    val body: MutableBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this))

    }

    override def printStatefully(pc: PrintContext): Unit = {
      pc += "for ("
      init.printStatefully(pc)
      cond.printStatefully(pc)
      increment.printStatefully(pc)
      pc += ") "
      pc.newln()
      body.printStatefully(pc)
    }

    override def print(): Doc = {
      text("for (") <>
        init.print <> cond.print <> increment.print <>
        ")" <> body.print
    }
  }

  case class ForLoop(init: DeclarationT,
                     cond: ExpressionStatement,
                     increment: ExpressionT,
                     body: MutableBlockT) extends ForLoopT

  /*
  While loop
   */
  trait WhileLoopT extends StatementT {
    val loopPredicate: Predicate
    val body: MutableBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, body))
    }

    override def print(): Doc = {
      "while(" <> Printer.toString(loopPredicate) <> ")" <>
        body.print
    }
  }

  case class WhileLoop(loopPredicate: Predicate,
                       body: MutableBlockT) extends WhileLoopT

  /*
  An If-then-else sequence
   */
  trait IfThenElseT extends StatementT {
    val cond: ExpressionT
    val trueBody: MutableBlockT
    val falseBody: MutableBlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, cond)) |>
        (visitFun(_, trueBody)) |>
        (visitFun(_, falseBody))
    }

    override def print(): Doc = {
      text("if (") <> cond.print <> ")" <> trueBody.print <>
        (if (falseBody != MutableBlock()) {
          text(" else ") <> falseBody.print()
        } else {
          nil
        })
    }
  }

  case class IfThenElse(cond: ExpressionT,
                        trueBody: MutableBlockT,
                        falseBody: MutableBlockT) extends IfThenElseT

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

    override def print(): Doc = {
      "goto " <> nameVar.print <> ";"
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

    override def print(): Doc = {
      nameVar.print <> ": ;"
    }
  }

  case class Label(nameVar: CVar) extends LabelT

  /**
    * A break statement (e.g. for exiting a loop)
    */
  trait BreakT extends StatementT {
    override def print(): Doc = "break;"
  }

  case class Break() extends BreakT

  /**
    * Typedef statements? Type declarations?
    */
  trait TypeDefT extends StatementT {
    val t: Type

    override def print(): Doc = t match {
      case tt: TupleType ⇒
        val name = Type.name(tt)
        spread(tt.elemsT.map(t ⇒ TypeDef(t).print).toList) <>
          s"#ifndef ${name}_DEFINED" </>
          s"#define ${name}_DEFINED" </>
          s"typedef struct __attribute__((aligned(${tt.alignment._1})))" <>
          bracket("{",
            stack(
              tt.elemsT.zipWithIndex.map({ case (ty, i) ⇒
                Type.name(ty) <> " _" <> i.toString <> ";"
              }).toList
            ),
            s"} $name;") </>
          "#endif" <> line
      case _             ⇒ Comment(s"NOTE: trying to print unprintable " +
        s"type: ${Printer.toString(t)}").print <> line
    }
  }

  case class TypeDef(t: Type) extends TypeDefT

  /**
    * ??? Tuple aliases?
    */
  trait TupleAliasT extends StatementT {
    val t: Type
    val name: String

    override def print() = t match {
      case tt: TupleType ⇒ text(s"typedef ") <> Type.name(tt) <+> name <> ";"
      case _             ⇒ Comment("NOTE: trying to print unprintable tuplealias").print
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

    override def print(): Doc = {
      e.print <> "; "
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

    override def print(): Doc = {
      name <> "(" <> intersperse(args.map(_.print)) <> ")"
    }
  }

  case class FunctionCall(name: String,
                          args: List[GenericAST.AstNode]) extends FunctionCallT

  /**
    * A reference to a declared variable
    */
  trait VarRefT extends ExpressionT {
    val v: CVar
    //    val t: Type
    val suffix: String //Option[String]
    val arrayIndex: ArithExpression //Option[ArithExpression]

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      visitFun(z, this) |> (visitFun(_, v))
    }

    override def print(): Doc = {

      val accessD = arrayIndex match {
        case null ⇒ nil
        case _    ⇒ text("[") <> arrayIndex.print <> text("]")
      }

      val suffixD = suffix match {
        case null ⇒ nil
        case _    ⇒ text(suffix)
      }

      v.print <> accessD <> suffixD

    }
  }

  case class VarRef(v: CVar,
                    //                    t: Type,
                    suffix: String = null,//Option[String] = None,
                    arrayIndex: ArithExpression = null//
                    // Option[ArithExpression] = None
                   ) extends VarRefT

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

    override def print(): Doc = {
      // TODO: Should we switch these, so we don't need the negation?
      if (!UseCastsForVectors()) {
        text(s"vload${Type.getLength(t)}(") <>
          offset.print <>
          "," <>
          v.print() <> ")"
      } else {
        text(s"*( ((${t}*)") <>
          v.print <>
          ") + " <>
          offset.print <>
          ")"
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

    override def print(): Doc = {
      if (!UseCastsForVectors()) {
        text(s"vstore${Type.getLength(t)}(") <>
          value.print <> "," <>
          offset.print <> "," <>
          v.print() <> ")"
      } else {
        s"*( ((${t}*)" <>
          v.print <> ") + " <>
          offset.print <> ") = " <>
          value.print
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

    override def print(): Doc = {
      to.print <+> "=" <+> value.print
    }
  }

  case class AssignmentExpression(to: AstNode, value: AstNode) extends
    AssignmentExpressionT

  /**
    * Wrapper for arithmetic expression
    */
  trait ArithExpressionT extends ExpressionT {
    val content: ArithExpr

    override def print(): Doc = Printer.toString(content)
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

    override def print(): Doc = {
      "(" <> lhs.print <+> op.toString <+> rhs.print <> ")"
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

    def print(): Doc = {
      "(" <>
        cond.print <+> "?" <+> trueExpr.print() <+> ":" <+> falseExpr.print() <>
        ")"
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

    override def print(): Doc = {
      "(" <> t.toString <> ")" <> v.print()
    }
  }

  /**
    * TODO: Can we actually do this? What will break :D
    */
  case class Cast(v: VarRef, t: Type) extends CastT

  case class PointerCast(v: VarRef, t: Type) extends CastT {

    override def print(): Doc = {
      "((" <> t.toString <> "*)" <> Printer.toString(v.v.v) <> ")" <>
        (v.arrayIndex match {
          case null ⇒ nil
          case _    ⇒ "[" <> v.arrayIndex.print <> "]"
        }) <>
        (v.suffix match {
          case null ⇒ nil
          case _    ⇒ text(v.suffix)
        })
    }
  }

  /**
    * Constructors for structs (e.g. when initialising)
    */
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

    override def print(): Doc = {
      "(" <> Printer.toString(t) <> "){" <>
        intersperse(args.map(_.print).toList) <>
        "}"
    }
  }

  case class StructConstructor(t: TupleType, args: Vector[AstNode]) extends
    StructConstructorT

  /**
    * Snippets of raw code that we might want to embed in our program
    */
  trait RawCodeT extends ExpressionT {
    val code: String

    override def print(): Doc = code
  }

  case class RawCode(code: String) extends RawCodeT

  /**
    * Inline comment block.
    */
  trait CommentT extends AstNode with BlockMember {
    val content: String

    override def print(): Doc = {
      // an alternative is << "//" <+> content >> but this might break if we
      // do any line length optimisations...
      s"// ${content}"
    }
  }

  case class Comment(content: String) extends CommentT

  /**
    * An empty block member, as a placeholder for when we want a node, but
    * don't want to print any code.
    */
  case class EmptyNode() extends AstNode with BlockMember {
    override def print(): Doc = nil
  }

}