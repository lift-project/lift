package core.generator

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
    def visit[T](z: T)(fun: (T, AstNode) => T): T

    def visit(pre: AstNode => Unit, post: AstNode => Unit = { _ => } ) : Unit ={
      pre(this)
      this._visit(pre,post)
      post(this)
    }
    /**
      * Traverses the AST and rebuilds a new AST. Before entering a node, the pre function is called.
      * Then the visitor is called recursively on every child and a new node is new AST node is instantiated if there are any children.
      * Finally, the post function is called on the node.
      * @param pre is called before visiting the children of this
      * @param post is called after visiting the children of this
      * @return the rebuilt node
     */
    final def visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode = {n => n}) : AstNode = {
      post(pre(this)._visitAndRebuild(pre,post))
    }

    protected def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit
    /**
      * This visitor should never be called directly but is for internal use only.
      * It should be overridden by any subclasses.
      * If a node has children, it should call visitAndRebuild on all the children and recreate a new instance of the node.
      * Otherwise, it should simply return this.
      */
    protected def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode

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

      override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        // visit the function object
        (fun(_, this)) |>
        // visit the parameters
        (params.foldLeft(_) {
          case (acc, node) => node.visit(acc)(fun)
        }) |>
        // visit the body
        (body.visit(_)(fun)) |>
        (acc => attribute match {
          case Some(a) => a.visit(acc)(fun)
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
    extends FunctionT {

    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      Function(name, ret, params.map(_.visitAndRebuild(pre, post).asInstanceOf[ParamDeclT]),
        body.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT],
        attribute match {
          case Some(a) => Some(a.visitAndRebuild(pre, post).asInstanceOf[AttributeT])
          case None => None
        })
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      params.foreach(_.visit(pre, post))
      body.visit(pre, post)
      attribute match {
        case Some(a) => a.visit(pre, post)
        case None =>
      }

    }
  }

  /*
  Variables trait
   */
  trait VarT extends DeclarationT {
    val v: lift.arithmetic.Var
    //    val t: Type

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = fun(z, this)

    override def print(): Doc = {
      text(Printer.toString(v))
    }
  }

  case class CVar(v: lift.arithmetic.Var /*, t: Type*/) extends VarT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      this
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {}
  }

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

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        // visit the VarDecl object
        (fun(_, this)) |>
        // visit the initial value
        (_z ⇒ init.map(fun(_z, _)).getOrElse(_z))
    }


    override def print(): Doc = {
      // print the type
      Printer.toString(Type.getBaseType(t)) <+>
        //print the variable name
        v.print() <>
        // print the assignment
        init.map(text(" = ") <> _.print()).getOrElse(empty) <>
        // end the line
        ";"
    }
  }

  case class VarDecl(v: CVar,
                     t: Type,
                     init: Option[AstNode] = None,
                     length: Long = 0) extends VarDeclT {

    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      VarDecl(v.visitAndRebuild(pre, post).asInstanceOf[CVar], t,
        init match {
          case Some(i) => Some(i.visitAndRebuild(pre, post))
          case None => None
        }, length)
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      v.visit(pre, post)
      init match {
          case Some(i) => i.visit(pre, post)
          case None =>
        }
    }

  }

  /*
  Parameter declaration. These have to be separated from variable
  declaration since the vectorization has to be handled differently
  in the OpenCL AST
    */
  trait ParamDeclT extends DeclarationT {
    val name: String
    val t: Type
    val const: Boolean // = false

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = fun(z, this)

    override def print(): Doc = t match {
      case ArrayType(_) ⇒
        val (constS, restrict) = if (const) (text("const"), text("restrict"))
        else (empty,
          empty)
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
                       const: Boolean = false) extends ParamDeclT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
        this
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {}
  }


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

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (content.foldLeft(_) {
          case (acc, node) => node.visit(acc)(fun)
        })
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

    def _visitAndRebuild(pre: AstNode => AstNode,  post: AstNode => AstNode) : AstNode = {
      MutableBlock(content.map(_.visitAndRebuild(pre, post).asInstanceOf[AstNode with BlockMember]), global)
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      content.foreach(_.visit(pre, post))
    }

    // TODO: these functions are not mutabing the block ... which is quite a bit confusing for a class called "MutableBlock"
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

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        // Visit internal expressions of a for loop
        (init.visit(_)(fun)) |>
        (cond.visit(_)(fun)) |>
        (increment.visit(_)(fun)) |>
        (body.visit(_)(fun))
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
                     body: MutableBlockT) extends ForLoopT {
    def _visitAndRebuild(pre: AstNode => AstNode,  post: AstNode => AstNode) : AstNode = {
      ForLoop(init.visitAndRebuild(pre, post).asInstanceOf[DeclarationT],
                   cond.visitAndRebuild(pre, post).asInstanceOf[ExpressionStatement],
                   increment.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
                   body.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT])
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      init.visit(pre, post)
      cond.visit(pre, post)
      increment.visit(pre, post)
      body.visit(pre, post)
    }
  }

  /*
  While loop
   */
  trait WhileLoopT extends StatementT {
    val loopPredicate: Predicate
    val body: MutableBlockT

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (body.visit(_)(fun))
    }

    override def print(): Doc = {
      "while(" <> Printer.toString(loopPredicate) <> ")" <>
        body.print
    }
  }

  case class WhileLoop(loopPredicate: Predicate,
                       body: MutableBlockT) extends WhileLoopT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      WhileLoop(loopPredicate,
        body.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT])
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      body.visit(pre, post)
    }
  }

  /*
  An If-then-else sequence
   */
  trait IfThenElseT extends StatementT {
    val cond: ExpressionT
    val trueBody: MutableBlockT
    val falseBody: MutableBlockT

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (cond.visit(_)(fun)) |>
        (trueBody.visit(_)(fun)) |>
        (falseBody.visit(_)(fun))
    }

    override def print(): Doc = {
      text("if (") <> cond.print <> ")" <> trueBody.print <>
        (if (falseBody != MutableBlock()) {
          text(" else ") <> falseBody.print()
        } else {
          empty
        })
    }
  }

  case class IfThenElse(cond: ExpressionT,
                        trueBody: MutableBlockT,
                        falseBody: MutableBlockT) extends IfThenElseT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      IfThenElse(cond.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        trueBody.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT],
        falseBody.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT])
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      cond.visit(pre, post)
      trueBody.visit(pre, post)
      falseBody.visit(pre, post)
    }
  }

  /** A goto statement, targeting the label with corresponding name
    * TODO: Think of a better way of describing goto labels
    */
  trait GOTOT extends StatementT {
    val nameVar: CVar

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (nameVar.visit(_)(fun))
    }

    override def print(): Doc = {
      "goto " <> nameVar.print <> ";"
    }
  }

  case class GOTO(nameVar: CVar) extends GOTOT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      GOTO(nameVar.visitAndRebuild(pre, post).asInstanceOf[CVar])
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      nameVar.visit(pre, post)
    }
  }

  /**
    * A Label, targeted by a corresponding goto
    */
  trait LabelT extends DeclarationT {
    val nameVar: CVar

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (nameVar.visit(_)(fun))
    }

    override def print(): Doc = {
      nameVar.print <> ": ;"
    }
  }

  case class Label(nameVar: CVar) extends LabelT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      Label(nameVar.visitAndRebuild(pre, post).asInstanceOf[CVar])
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      nameVar.visit(pre, post)
    }
  }

  /**
    * A break statement (e.g. for exiting a loop)
    */
  trait BreakT extends StatementT {
    override def print(): Doc = "break;"

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = fun(z, this)
  }

  case class Break() extends BreakT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      this
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {}
  }

  /**
    * Typedef statements? Type declarations?
    */
  trait TypeDefT extends StatementT {
    val t: Type

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = fun(z, this)

    override def print(): Doc = t match {
      case tt: TupleType ⇒
        val name = Type.name(tt)
        spread(tt.elemsT.map(t ⇒ TypeDef(t).print()).toList) <>
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
      case _  => Comment(s"NOTE: trying to print unprintable " +
        s"type: ${Printer.toString(t)}").print <> line
    }
  }

  case class TypeDef(t: Type) extends TypeDefT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      this
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {}
  }

  /**
    * ??? Tuple aliases?
    */
  trait TupleAliasT extends StatementT {
    val t: Type
    val name: String

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = fun(z, this)

    override def print(): Doc = t match {
      case tt: TupleType ⇒ text(s"typedef ") <> Type.name(tt) <+> name <> ";"
      case _             ⇒ Comment("NOTE: trying to print unprintable tuplealias").print()
    }
  }

  case class TupleAlias(t: Type, name: String) extends TupleAliasT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      this
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {}
  }

  /**
    * Expression statements??
    */
  trait ExpressionStatementT extends StatementT {
    val e: ExpressionT

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (e.visit(_)(fun))
    }

    override def print(): Doc = {
      e.print <> "; "
    }
  }

  case class ExpressionStatement(e: ExpressionT) extends ExpressionStatementT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode): AstNode = {
      ExpressionStatement(e.visitAndRebuild(pre, post).asInstanceOf[ExpressionT])
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      e.visit(pre, post)
    }
  }

  implicit def exprToStmt(e: ExpressionT): ExpressionStatement =
    ExpressionStatement(e)

  trait FunctionCallT extends ExpressionT {
    val name: String
    val args: List[AstNode]

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (args.foldLeft(_) {
          case (acc, node) => node.visit(acc)(fun)
        })
    }

    override def print(): Doc = {
      name <> "(" <> intersperse(args.map(_.print())) <> ")"
    }
  }

  case class FunctionCall(name: String,
                          args: List[GenericAST.AstNode]) extends FunctionCallT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      FunctionCall(name, args.map(_.visitAndRebuild(pre, post)))
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      args.foreach(_.visit(pre, post))
    }
  }

  /**
    * A reference to a declared variable
    */
  trait VarRefT extends ExpressionT {
    val v: CVar
    //    val t: Type
    val suffix: Option[String]
    val arrayIndex: Option[ArithExpression]

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (v.visit(_)(fun)) |>
        (z => arrayIndex.map(_.visit(z)(fun)).getOrElse(z))
    }

    override def print(): Doc = {

      val accessD = arrayIndex match {
        case None     ⇒ empty
        case Some(ix) ⇒ "[" <> ix.print <> "]"
      }

      val suffixD = suffix match {
        case None     ⇒ empty
        case Some(sf) ⇒ text(sf)
      }

      v.print <> accessD <> suffixD

    }
  }

  case class VarRef(v: CVar,
                    //                    t: Type,
                    suffix: Option[String] = None,
                    arrayIndex: Option[ArithExpression] = None
                   ) extends VarRefT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      VarRef(v.visitAndRebuild(pre, post).asInstanceOf[CVar], suffix,
        arrayIndex match {
          case Some(ai) => Some(ai.visitAndRebuild(pre, post).asInstanceOf[ArithExpression])
          case None => None
        })

    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      v.visit(pre, post)
      arrayIndex match {
          case Some(ai) => ai.visit(pre, post)
          case None =>
        }
    }
  }

  /**
    * A load from a variable, with (potentially) an offset
    */
  trait LoadT extends ExpressionT {
    val v: VarRef
    val t: Type
    val offset: ArithExpression

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (v.visit(_)(fun))
    }

    override def print(): Doc = {
      // TODO: Should we switch these, so we don't need the negation?
      if (!UseCastsForVectors()) {
        text(s"vload${Type.getLength(t)}(") <>
          offset.print <>
          "," <>
          v.print() <> ")"
      } else {
        text(s"*( (($t*)") <>
          v.print <>
          ") + " <>
          offset.print <>
          ")"
      }
    }
  }

  case class Load(v: VarRef,
                  t: Type,
                  offset: ArithExpression) extends LoadT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      Load(v.visitAndRebuild(pre, post).asInstanceOf[VarRef], t,
        offset.visitAndRebuild(pre, post).asInstanceOf[ArithExpression])
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      v.visit(pre, post)
      offset.visit(pre, post)
    }
  }

  /**
    * A Store into a variable with (potentially) an offset
    */
  trait StoreT extends ExpressionT {
    val v: VarRef
    val t: Type
    val value: AstNode
    val offset: ArithExpression

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (v.visit(_)(fun)) |>
        (value.visit(_)(fun))
    }

    override def print(): Doc = {
      if (!UseCastsForVectors()) {
        text(s"vstore${Type.getLength(t)}(") <>
          value.print <> "," <>
          offset.print <> "," <>
          v.print() <> ")"
      } else {
        s"*( (($t*)" <>
          v.print <> ") + " <>
          offset.print <> ") = " <>
          value.print
      }
    }
  }

  case class Store(v: VarRef,
                   t: Type,
                   value: AstNode,
                   offset: ArithExpression) extends StoreT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      Store(v.visitAndRebuild(pre, post).asInstanceOf[VarRef],t,
        value.visitAndRebuild(pre, post),
        offset.visitAndRebuild(pre, post).asInstanceOf[ArithExpression])
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      v.visit(pre, post)
      value.visit(pre, post)
      offset.visit(pre, post)
    }
  }

  /**
    * Represent an assignment.
    */
  trait AssignmentExpressionT extends ExpressionT {
    val to: AstNode
    val value: AstNode

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (to.visit(_)(fun)) |>
        (value.visit(_)(fun))
    }

    override def print(): Doc = {
      to.print <+> "=" <+> value.print
    }
  }

  case class AssignmentExpression(to: AstNode, value: AstNode) extends
    AssignmentExpressionT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      AssignmentExpression(to.visitAndRebuild(pre, post),
        value.visitAndRebuild(pre, post))
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      to.visit(pre, post)
      value.visit(pre,post)
    }
  }

  /**
    * Wrapper for arithmetic expression
    */
  trait ArithExpressionT extends ExpressionT {
    val content: ArithExpr

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = fun(z, this)

    override def print(): Doc = Printer.toString(content)
  }

  case class ArithExpression(content: ArithExpr) extends ArithExpressionT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      this
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {}
  }

  /**
    * Binary expressions
    */
  trait BinaryExpressionT extends ExpressionT {
    val lhs: ExpressionT
    val rhs: ExpressionT
    val op: BinaryExpressionT.Operator.Operator

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (lhs.visit(_)(fun)) |>
        (rhs.visit(_)(fun))
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
    extends BinaryExpressionT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      BinaryExpression(lhs.visitAndRebuild(pre, post).asInstanceOf[ExpressionT], op,
        rhs.visitAndRebuild(pre, post).asInstanceOf[ExpressionT])
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      lhs.visit(pre, post)
      rhs.visit(pre, post)
    }
  }

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

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (cond.visit(_)(fun)) |>
        (trueExpr.visit(_)(fun)) |>
        (falseExpr.visit(_)(fun))
    }

    def print(): Doc = {
      "(" <>
        cond.print <+> "?" <+> trueExpr.print() <+> ":" <+> falseExpr.print() <>
        ")"
    }
  }

  case class TernaryExpression(cond: BinaryExpressionT, trueExpr: ExpressionT, falseExpr: ExpressionT)
    extends TernaryExpressionT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      TernaryExpression(cond.visitAndRebuild(pre, post).asInstanceOf[BinaryExpressionT],
        trueExpr.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        falseExpr.visitAndRebuild(pre, post).asInstanceOf[ExpressionT])
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      cond.visit(pre, post)
      trueExpr.visit(pre, post)
      falseExpr.visit(pre, post)
    }
  }

  /**
    * Force a cast of a variable to the given type. This is used to
    */
  trait CastT extends ExpressionT {
    val v: VarRef
    val t: Type

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (v.visit(_)(fun))
    }

    override def print(): Doc = {
      "(" <> t.toString <> ")" <> v.print()
    }
  }

  /**
    * TODO: Can we actually do this? What will break :D
    */
  case class Cast(v: VarRef, t: Type) extends CastT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      Cast(v.visitAndRebuild(pre, post).asInstanceOf[VarRef],t)
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      v.visit(pre, post)
    }
  }

  case class PointerCast(v: VarRef, t: Type) extends CastT {

    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      PointerCast(v.visitAndRebuild(pre, post).asInstanceOf[VarRef],t)
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      v.visit(pre, post)
    }

    override def print(): Doc = {
      "((" <> t.toString <> "*)" <> Printer.toString(v.v.v) <> ")" <>
        (v.arrayIndex match {
          case None ⇒ empty
          case Some(ix)    ⇒ "[" <> ix.print <> "]"
        }) <>
        (v.suffix match {
          case None ⇒ empty
          case Some(sf)    ⇒ text(sf)
        })
    }
  }

  /**
    * Constructors for structs (e.g. when initialising)
    */
  trait StructConstructorT extends ExpressionT {
    val t: TupleType
    val args: Vector[AstNode]

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = {
      z |>
        (fun(_, this)) |>
        (args.foldLeft(_) {
          case (acc, node) => node.visit(acc)(fun)
        })
    }

    override def print(): Doc = {
      "(" <> Printer.toString(t) <> "){" <>
        intersperse(args.map(_.print()).toList) <>
        "}"
    }
  }

  case class StructConstructor(t: TupleType, args: Vector[AstNode]) extends
    StructConstructorT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      StructConstructor(t, args.map(_.visitAndRebuild(pre, post)))
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {
      args.foreach(_.visit(pre, post))
    }
  }

  /**
    * Snippets of raw code that we might want to embed in our program
    */
  trait RawCodeT extends ExpressionT {
    val code: String

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = fun(z, this)

    override def print(): Doc = code
  }

  case class RawCode(code: String) extends RawCodeT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode): AstNode = {
      this
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {}
  }

  /**
    * Inline comment block.
    */
  trait CommentT extends AstNode with BlockMember {
    val content: String

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = fun(z, this)

    override def print(): Doc = {
      // an alternative is << "//" <+> content >> but this might break if we
      // do any line length optimisations...
      s"// $content"
    }
  }

  case class Comment(content: String) extends CommentT {
    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      this
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {}
  }

  /**
    * An empty block member, as a placeholder for when we want a node, but
    * don't want to print any code.
    */
  case class EmptyNode() extends AstNode with BlockMember {

    override def visit[T](z: T)(fun: (T, AstNode) => T): T = fun(z, this)

    override def print(): Doc = empty

    def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode) : AstNode = {
      this
    }

    def _visit(pre: AstNode => Unit, post: AstNode => Unit) : Unit = {}
  }

}