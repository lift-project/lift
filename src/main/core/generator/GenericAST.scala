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
    def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z,
      this)

    def prePostVisit[T](z: T)(preVisit: (T, AstNode) ⇒ T, postVisit: (T,
      AstNode) ⇒ T): T = {
      z |> (preVisit(_, this)) |> (postVisit(_, this))
    }


    def visitBy(pre: (AstNode) => Unit, post: (AstNode) => Unit = { _ => } ) : Unit ={
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
    final def visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode = {n => n}) : AstNode = {
      post(pre(this)._visitAndRebuild(pre,post))
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit
    /**
      * This visitor should never be called directly but is for internal use only.
      * It should be overriden by any subclasses.
      * If a node has children, it should call visitAndRebuild on all the children and recreate a new instance of the node.
      * Otherwise, it should simply return this.
      */
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode

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
          case (acc, node) => node.visit(acc)(visitFun)
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
    extends FunctionT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      Function(name, ret, params.map(_.visitAndRebuild(pre, post).asInstanceOf[ParamDeclT]),
        body.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT],
        attribute match {
          case Some(a) => Some(a.visitAndRebuild(pre, post).asInstanceOf[AttributeT])
          case None => None
        })
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      params.map(_.visitBy(pre, post))
      body.visitBy(pre, post)
      attribute match {
        case Some(a) => a.visitBy(pre, post)
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

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = visitFun(z, this)

    override def print(): Doc = {
      text(Printer.toString(v))
    }
  }

  case class CVar(v: lift.arithmetic.Var /*, t: Type*/) extends VarT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
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

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        // visit the VarDecl object
        (visitFun(_, this)) |>
        // visit the initial value
        (_z ⇒ init.map(visitFun(_z, _)).getOrElse(_z))
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

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      VarDecl(v.visitAndRebuild(pre, post).asInstanceOf[CVar], t,
        init match {
          case Some(i) => Some(i.visitAndRebuild(pre, post))
          case None => None
        }, length)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
      init match {
          case Some(i) => i.visitBy(pre, post)
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
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
        this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
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

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (content.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
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

    def _visitAndRebuild(pre: (AstNode) => AstNode,  post: (AstNode) => AstNode) : AstNode = {
      MutableBlock(content.map(_.visitAndRebuild(pre, post).asInstanceOf[AstNode with BlockMember]), global)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      content.map(_.visitBy(pre, post))
    }

    /** Append a sub-node. Could be any node, including a sub-block.
      *
      * @param node The node to add to this block.
      */
    def :+(node: AstNode with BlockMember): MutableBlock = this.copy(content = content :+ node)

    def ::(node: AstNode with BlockMember): MutableBlock = this.copy(content = node +: content)

    def ++(nodes: Vector[AstNode with BlockMember]): MutableBlock = this.copy(content = content ++ nodes)

    def :++(mb: Block) : MutableBlock = this.copy(content = content ++ mb.content )

    def toBlock: Block = Block(content, global = this.global)
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
        (visitFun(_, this)) |>
        // Visit internal expressions of a for loop
        (init.visit(_)(visitFun)) |>
        (cond.visit(_)(visitFun)) |>
        (increment.visit(_)(visitFun)) |>
        (body.visit(_)(visitFun))
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
    def _visitAndRebuild(pre: (AstNode) => AstNode,  post: (AstNode) => AstNode) : AstNode = {
      ForLoop(init.visitAndRebuild(pre, post).asInstanceOf[DeclarationT],
                   cond.visitAndRebuild(pre, post).asInstanceOf[ExpressionStatement],
                   increment.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
                   body.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      init.visitBy(pre, post)
      cond.visitBy(pre, post)
      increment.visitBy(pre, post)
      body.visitBy(pre, post)
    }
  }

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
                       body: MutableBlockT) extends WhileLoopT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      WhileLoop(loopPredicate,
        body.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      body.visitBy(pre, post)
    }
  }

  trait IfThenElseImT extends StatementT {
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

    override def print(): Doc = {
      text("if (") <> cond.print <> ")" <> trueBody.print <>
        (if (falseBody != Block()) {
          text(" else ") <> falseBody.print()
        } else {
          empty
        })
    }
  }

  case class IfThenElseIm(cond: ExpressionT, trueBody: BlockT, falseBody: BlockT) extends IfThenElseImT {


    override def _visitAndRebuild(pre: (AstNode) => AstNode,  post: (AstNode) => AstNode) : AstNode = {
      IfThenElseIm(cond.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        trueBody.visitAndRebuild(pre, post).asInstanceOf[BlockT],
        falseBody.visitAndRebuild(pre, post).asInstanceOf[BlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      cond.visitBy(pre, post)
      trueBody.visitBy(pre, post)
      falseBody.visitBy(pre, post)
    }


  }


  trait IfThenElifImT extends StatementT {

    val conds: List[ExpressionT]
    val trueBodys: List[BlockT]
    val falseBody: BlockT


    override def print(): Doc = {
      /*
      text("if (") <> cond.print <> ")" <> trueBody.print <>
        (if (falseBody != Block()) {
          text(" else ") <> falseBody.print()
        } else {
          empty
        })*/

      val elif_conds = conds.drop(1)
      val elif_trueBodys = trueBodys.drop(1)
      val zipped = elif_conds zip elif_trueBodys
      val docs = zipped.map{ case (cond, body) => text("else if ( ") <> cond.print <> text(" )") <> body.print }
      val final_docs = (text("") /: docs ) ( </> )

      text("if (") <> conds(0).print <> ")" <> trueBodys(0).print </> final_docs <>
        (if (falseBody != Block()) {
          text(" else ") <> falseBody.print()
        } else {
          empty
        })
    }

  }


  case class IfThenElifIm(conds: List[ExpressionT], trueBodys: List[BlockT], falseBody: BlockT) extends IfThenElifImT {


    override def _visitAndRebuild(pre: (AstNode) => AstNode,  post: (AstNode) => AstNode) : AstNode = {
      IfThenElifIm(conds.map(_.visitAndRebuild(pre, post).asInstanceOf[ExpressionT]),
        trueBodys.map(_.visitAndRebuild(pre, post).asInstanceOf[BlockT]),
        falseBody.visitAndRebuild(pre, post).asInstanceOf[BlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      conds.foreach(_.visitBy(pre, post))
      trueBodys.foreach(_.visitBy(pre, post))
      falseBody.visitBy(pre, post)
    }


  }


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
          empty
        })
    }
  }

  case class IfThenElse(cond: ExpressionT,
                        trueBody: MutableBlockT,
                        falseBody: MutableBlockT) extends IfThenElseT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      IfThenElse(cond.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        trueBody.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT],
        falseBody.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      cond.visitBy(pre, post)
      trueBody.visitBy(pre, post)
      falseBody.visitBy(pre, post)
    }
  }

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

  case class GOTO(nameVar: CVar) extends GOTOT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      GOTO(nameVar.visitAndRebuild(pre, post).asInstanceOf[CVar])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      nameVar.visitBy(pre, post)
    }
  }

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

  case class Label(nameVar: CVar) extends LabelT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      Label(nameVar.visitAndRebuild(pre, post).asInstanceOf[CVar])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      nameVar.visitBy(pre, post)
    }
  }

  /**
    * A break statement (e.g. for exiting a loop)
    */
  trait BreakT extends StatementT {
    override def print(): Doc = "break;"
  }

  case class Break() extends BreakT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

  /**
    * Typedef statements? Type declarations?
    */
  trait TypeDefT extends StatementT {
    val t: Type

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
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

  trait TypeDefHostT extends StatementT {
    val t: Type

    override def print(): Doc = t match {
      case tt: TupleType ⇒
        val name = Type.name(tt)
        spread(tt.elemsT.map(t ⇒ TypeDef(t).print()).toList) <>
          s"typedef struct" <>
          bracket("{",
            stack(
              tt.elemsT.zipWithIndex.map({ case (ty, i) ⇒
                Type.name(ty) <> " _" <> i.toString <> ";"
              }).toList
            ),
            s"} $name;") <> line
      case _  => Comment(s"NOTE: trying to print unprintable " +
        s"type: ${Printer.toString(t)}").print <> line
    }
  }

  case class TypeDefHost(t: Type) extends TypeDefHostT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

  /**
    * ??? Tuple aliases?
    */
  trait TupleAliasT extends StatementT {
    val t: Type
    val name: String

    override def print(): Doc = t match {
      case tt: TupleType ⇒ text(s"typedef ") <> Type.name(tt) <+> name <> ";"
      case _             ⇒ Comment("NOTE: trying to print unprintable tuplealias").print()
    }
  }

  case class TupleAlias(t: Type, name: String) extends TupleAliasT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

  /**
    * Expression statements??
    */
  trait ExpressionStatementT extends StatementT {
    val e: ExpressionT
    val neglectSemiColon: Boolean

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (visitFun(_, e))
    }

    override def print(): Doc = {
      e.print <> (if(neglectSemiColon) "" else "; " )
    }
  }

  case class ExpressionStatement(e: ExpressionT, neglectSemiColon: Boolean = false) extends ExpressionStatementT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      ExpressionStatement(e.visitAndRebuild(pre, post).asInstanceOf[ExpressionT])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      e.visitBy(pre, post)
    }
  }

  implicit def exprToStmt(e: ExpressionT): ExpressionStatement =
    ExpressionStatement(e)

  trait FunctionCallT extends ExpressionT {
    val name: String
    val args: List[AstNode]
    val template_types: List[CTypeT]

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (args.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
        })
    }

    override def print(): Doc = {
      name <> (if (template_types.size == 0) "" else "<" <> intersperse(template_types.map(_.print())) <> ">") <> "(" <> intersperse(args.map(_.print)) <> ")"
    }
  }

  case class FunctionCall(name: String,
                          args: List[GenericAST.AstNode], val template_types: List[CTypeT]= List()) extends FunctionCallT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      FunctionCall(name, args.map(_.visitAndRebuild(pre, post)), template_types.map(_.visitAndRebuild(pre,post).asInstanceOf[CTypeT]))
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      args.foreach(_.visitBy(pre, post))
      template_types.foreach(_.visitBy(pre,post))
    }
  }

  trait MethodInvocationT extends ExpressionT {
    //val object_name:String
    val object_name:ExpressionT
    val method_name:String
    val args: List[GenericAST.AstNode]
    val isPointer: Boolean

    override def print(): Doc = {
      object_name.print() <>
        (if(isPointer) "->" else ".") <>
        ( method_name ++ "(")  <>
        intersperse(args.map(_.print)) <> ")"
    }
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = {
      object_name.visitBy(pre,post)
      args.map(_.visitBy(pre,post))
    }

  }

  //case class MethodInvocation(object_name:String,
  case class MethodInvocation(object_name:ExpressionT,
                              method_name:String,
                              args: List[GenericAST.AstNode],
                              isPointer: Boolean = false) extends MethodInvocationT
  {
    override def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode): AstNode =
      MethodInvocation(
        object_name.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        method_name,
        args.map(_.visitAndRebuild(pre,post)),
        isPointer)

  }


  trait TupleExprT extends ExpressionT {

    val args: List[AstNode]

    override def print(): Doc = {
     "{" <> intersperse(args.map(_.print)) <> "}"
    }

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = {
      args.map(_.visitBy(pre,post))
    }

  }

  case class TupleExpr( args: List[GenericAST.AstNode] ) extends TupleExprT {

    override def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode): AstNode =
      TupleExpr( args.map(_.visitAndRebuild(pre,post)) )

  }

  /**
    * A reference to a declared variable
    */
  trait VarRefT extends ExpressionT {
    val v: CVar
    //    val t: Type
    val suffix: Option[String]
    val arrayIndex: Option[ArithExpression]

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      visitFun(z, this) |> (visitFun(_, v))
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
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      VarRef(v.visitAndRebuild(pre, post).asInstanceOf[CVar], suffix,
        arrayIndex match {
          case Some(ai) => Some(ai.visitAndRebuild(pre, post).asInstanceOf[ArithExpression])
          case None => None
        })

    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
      arrayIndex match {
          case Some(ai) => ai.visitBy(pre, post)
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
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      Load(v.visitAndRebuild(pre, post).asInstanceOf[VarRef], t,
        offset.visitAndRebuild(pre, post).asInstanceOf[ArithExpression])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
      offset.visitBy(pre, post)
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
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      Store(v.visitAndRebuild(pre, post).asInstanceOf[VarRef],t,
        value.visitAndRebuild(pre, post),
        offset.visitAndRebuild(pre, post).asInstanceOf[ArithExpression])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
      value.visitBy(pre, post)
      offset.visitBy(pre, post)
    }
  }

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
    AssignmentExpressionT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      AssignmentExpression(to.visitAndRebuild(pre, post),
        value.visitAndRebuild(pre, post))
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      to.visitBy(pre, post)
      value.visitBy(pre,post)
    }
  }

  /**
    * Wrapper for arithmetic expression
    */
  trait ArithExpressionT extends ExpressionT {
    val content: ArithExpr

    override def print(): Doc = Printer.toString(content)
  }

  case class ArithExpression(content: ArithExpr) extends ArithExpressionT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

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
    extends BinaryExpressionT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      BinaryExpression(lhs.visitAndRebuild(pre, post).asInstanceOf[ExpressionT], op,
        rhs.visitAndRebuild(pre, post).asInstanceOf[ExpressionT])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      lhs.visitBy(pre, post)
      rhs.visitBy(pre, post)
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
    extends TernaryExpressionT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      TernaryExpression(cond.visitAndRebuild(pre, post).asInstanceOf[BinaryExpressionT],
        trueExpr.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        falseExpr.visitAndRebuild(pre, post).asInstanceOf[ExpressionT])
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      cond.visitBy(pre, post)
      trueExpr.visitBy(pre, post)
      falseExpr.visitBy(pre, post)
    }
  }

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
  case class Cast(v: VarRef, t: Type) extends CastT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      Cast(v.visitAndRebuild(pre, post).asInstanceOf[VarRef],t)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
    }
  }

  case class PointerCast(v: VarRef, t: Type) extends CastT {

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      PointerCast(v.visitAndRebuild(pre, post).asInstanceOf[VarRef],t)
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      v.visitBy(pre, post)
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

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this)) |>
        (args.foldLeft(_) {
          case (acc, node) => node.visit(acc)(visitFun)
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
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      StructConstructor(t, args.map(_.visitAndRebuild(pre, post)))
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      args.map(_.visitBy(pre, post))
    }
  }


  def vl (l: String, r: Doc): Doc = l match {
    case "" => r
    case _ => l </> r
  }

  def vr (l: Doc, r: String): Doc = r match {
    case "" => l
    case _ => l </> r
  }

  /**
    * Snippets of raw code that we might want to embed in our program
    */
  trait RawCodeT extends ExpressionT {
    val code: String
    val pre1: String
    val pre2: String
    val post1: String
    val post2: String

    override def print(): Doc = vr( vr( vl(pre1, vl(pre2, code) ),  post1 ), post2)
  }

  case class RawCode(code: String = "", pre1: String = "", pre2: String ="", post1: String ="", post2: String = "") extends RawCodeT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

  /**
    * Inline comment block.
    */
  trait CommentT extends AstNode with BlockMember {
    val content: String

    override def print(): Doc = {
      // an alternative is << "//" <+> content >> but this might break if we
      // do any line length optimisations...
      s"// $content"
    }
  }

  case class Comment(content: String) extends CommentT {
    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

  /**
    * An empty block member, as a placeholder for when we want a node, but
    * don't want to print any code.
    */
  case class EmptyNode() extends AstNode with BlockMember {
    override def print(): Doc = empty

    def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode) : AstNode = {
      this
    }

    def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {}
  }

  /**
    * The following are extensions for host code generator
    */

  trait UnaryExpressionT extends ExpressionT

  trait BlockT extends StatementT {
    // TODO: How do we handle default values when they're vals?
    val content: Vector[AstNode with BlockMember] // = Vector.empty
    val global: Boolean // = false

    def :+(node: AstNode with BlockMember): BlockT

    def ::(node: AstNode with BlockMember): BlockT

    def ++(nodes: Vector[AstNode with BlockMember]): BlockT

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = content.map(_.visitBy(pre,post))


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

  case class Block(override val content: Vector[AstNode with
    BlockMember] = Vector(), global: Boolean = false) extends BlockT {
    /** Append a sub-node. Could be any node, including a sub-block.
      *
      * @param node The node to add to this block.
      */
    def :+(node: AstNode with BlockMember): Block = this.copy(content = content :+ node)

    def +:(node: AstNode with BlockMember): Block = this.copy(content = node +: content)
    def ::(node: AstNode with BlockMember): Block = this.copy(content = node +: content)

    def ++(nodes: Vector[AstNode with BlockMember]): Block = this.copy(content = content ++ nodes)

    def ++:(mb: MutableBlock) : Block = this.copy(content = mb.content ++ content)

    def :++(mb: Block) : Block = this.copy(content = content ++ mb.content, global = true )

    override def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode): AstNode =
      Block(content.map(_.visitAndRebuild(pre,post).asInstanceOf[AstNode with BlockMember]), global = this.global)
  }

  trait AccessPropertyT extends AstNode {
    val name:String
    override def print(): Doc = name
  }

  trait CTypeT extends AstNode {
    val name: String
    override def print(): Doc = text(name)
  }

  trait PrimitiveTypeT extends CTypeT
  trait ReferenceTypeT extends CTypeT
  trait ClassOrStructTypeT extends CTypeT {
    val name: String
    val template_types: List[CTypeT]

    override def print(): Doc = {
      name <> (if (template_types.size == 0) "" else "<" <> intersperse(template_types.map(_.print())) <> ">")
    }
  }
  case class ClassOrStructType(name:String, val template_types: List[CTypeT]= List() ) extends ClassOrStructTypeT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }
  trait VoidTypeT extends PrimitiveTypeT
  case class VoidType(name:String) extends VoidTypeT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }
  object VoidType {
    def apply() = new VoidType("void")
  }


  trait NumericTypeT extends PrimitiveTypeT
  trait IntegerTypeT extends NumericTypeT
  case class IntegerType(name:String) extends IntegerTypeT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }
  object IntegerType {
    def apply() = new IntegerType(name="int")
  }

  //used by SDH push and pop cast
  trait Uint32_t_T extends IntegerTypeT
  case class Uint32_t(name:String) extends Uint32_t_T {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }
  object Uint32_t {
    def apply() = new Uint32_t(name="uint32_t")
  }

  trait Uintptr_t_T extends IntegerTypeT
  case class Uintptr_t(name:String) extends Uintptr_t_T {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }
  object Uintptr_t {
    def apply() = new Uint32_t(name="uintptr_t")
  }

  trait FloatingPointTypeT extends NumericTypeT

  trait FloatTypeT extends FloatingPointTypeT

  case class FloatType(name: String) extends FloatTypeT {

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()

    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }

  object FloatType {
    def apply() = new FloatType("float")
  }

  trait DoubleTypeT extends FloatingPointTypeT

  case class DoubleType(name: String) extends DoubleTypeT {

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()

    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }

  object DoubleType {
    def apply() = new FloatType("double")
  }

  trait PointerTypeT extends CTypeT

  case class PointerType(name: String, t: CTypeT) extends PointerTypeT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
    override def print(): Doc = t.print() <> text(" " ++ name)
  }

  object PointerType {
    def apply(bodyType: CTypeT) = new PointerType("*", bodyType)
  }

  trait RefTypeT extends CTypeT

  case class RefType(name: String, t: CTypeT) extends RefTypeT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
    override def print(): Doc = t.print() <> text(" " ++ name)
  }

  object RefType {
    def apply(bodyType: CTypeT) = new RefType("&", bodyType)
  }


  trait CArrayTypeT extends CTypeT

  case class CArrayType(name: String, elem_t: CTypeT) extends CArrayTypeT{
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this

  }

  trait CHostArrayTypeT extends CArrayTypeT {
    val name: String
    val elem_t: CTypeT

    override def print(): Doc = elem_t.print() <> "*"
  }

  case class CHostArrayType(name: String, elem_t: CTypeT) extends CHostArrayTypeT{

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }

  trait COclArrayTypeT extends CArrayTypeT {
    val name: String
    val elem_t: CTypeT

    override def print(): Doc = "cl::Buffer"
  }

  case class COclArrayType(name: String, elem_t: CTypeT) extends COclArrayTypeT{

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }


  trait MarkT extends AstNode with BlockMember

  trait OutlineMarkT extends MarkT

  case class OutlineMark(outline_struct: Block) extends OutlineMarkT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = outline_struct.visitBy(pre, post)
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this

    override def print(): Doc = outline_struct.print()
  }

  trait ValueT extends ExpressionT

  trait StringConstantT extends ValueT {
    val value: String

    override def print(): Doc = value
  }

  case class StringConstant(value: String) extends StringConstantT{
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this

  }

  trait IntConstantT extends ValueT {
    val value: Int

    override def print(): Doc = value.toString()
  }

  case class IntConstant(value: Int) extends IntConstantT{
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this

  }


  trait CVarWithTypeT extends DeclarationT with UnaryExpressionT {
    val name: String
    val t: CTypeT
    override def print(): Doc = text(name)
  }

  case class CVarWithType(name:String, t: CTypeT) extends CVarWithTypeT {
    override def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode): AstNode =
      CVarWithType(
        name,
        t.visitAndRebuild(pre, post).asInstanceOf[CTypeT]
      )
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()

  }

  trait HostAccessPropertyT extends AccessPropertyT

  case class HOST_MEM_READ_ONLY(name: String = "HOST_MEM_READ_ONLY") extends HostAccessPropertyT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }

  case class HOST_MEM_WRITE_ONLY(name: String = "HOST_MEM_WRITE_ONLY") extends HostAccessPropertyT {

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }

  case class HOST_MEM_READ_WRITE(name: String = "HOST_MEM_READ_WRITE") extends HostAccessPropertyT {

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }

  trait DeviceAccessPropertyT extends AccessPropertyT

  case class DEVICE_MEM_READ_ONLY(name: String = "CL_MEM_READ_ONLY") extends DeviceAccessPropertyT {

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }

  case class DEVICE_MEM_WRITE_ONLY(name: String = "CL_MEM_WRITE_ONLY") extends DeviceAccessPropertyT {

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }

  case class DEVICE_MEM_READ_WRITE(name: String = "CL_MEM_READ_WRITE") extends DeviceAccessPropertyT {

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }

  trait NativeVarT extends CVarWithTypeT {
    val size: ArithExpr
  }

  trait HostBufferT extends NativeVarT {
    val access: HostAccessPropertyT
  }

  case class HostBuffer(name: String, t: CTypeT, size: ArithExpr, access: HostAccessPropertyT) extends HostBufferT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
    override def print(): Doc = name
  }

  trait OclBufferT extends NativeVarT {
    val access: DeviceAccessPropertyT
  }

  case class OclBuffer(name: String, t: CTypeT, size: ArithExpr, access: DeviceAccessPropertyT) extends OclBufferT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
    override def print(): Doc = name
  }

  trait UnknownBufferT extends CVarWithTypeT

  case class UnknownBuffer(name: String, t: CTypeT = VoidType()) extends UnknownBufferT{
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this

  }

  trait VarDeclPureT extends DeclarationT

  case class VarDeclPure(v:CVarWithType, t:CTypeT, init: Option[AstNode] = None) extends VarDeclPureT {
    override def print(): Doc = t.print() <> text(" ") <> v.print() <> init.map(text(" = ") <> _.print()).getOrElse(empty)  <> ";"
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this

  }

  trait ObjectDeclT extends DeclarationT{
    val v: CVarWithType
    val t: CTypeT
    val args: List[AstNode]

    override def print(): Doc = t.print() <> " " <> v.print() <> {
      args.size match {
        case 0 => ""
        case _ => "(" <> intersperse(args.map(_.print)) <> ")"
      }
    } <> ";"
  }


  case class ObjectDecl(v:CVarWithType, t: CTypeT, args:List[AstNode]) extends ObjectDeclT{
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this

  }

  trait VarRefPureT extends UnaryExpressionT {
    val v: CVarWithTypeT
    //    val t: Type
    val suffix: Option[String]
    val arrayIndex: Option[ArithExpression]


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


  case class VarRefPure(v: CVarWithTypeT,
                        //                    t: Type,
                        suffix: Option[String] = None,
                        arrayIndex: Option[ArithExpression] = None
                       ) extends VarRefPureT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: AstNode => AstNode, post: AstNode => AstNode): AstNode =
      VarRefPure(
        v.visitAndRebuild(pre, post).asInstanceOf[CVarWithTypeT],
        suffix,
        arrayIndex
      )


  }

  trait UnaryOperatorT

  case class UnaryExpression(op: String, operand: UnaryExpressionT) extends UnaryExpressionT {

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = operand.visitBy(pre, post)

    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this

    override def print(): Doc = "(" <> op <> operand.print() <> ")"
  }


  trait ForLoopImT extends StatementT {
    val init: DeclarationT
    val cond: ExpressionStatement
    val increment: ExpressionT
    val body: BlockT

    override def visit[T](z: T)(visitFun: (T, AstNode) => T): T = {
      z |>
        (visitFun(_, this))

    }

    override def print(): Doc = {
      text("for (") <>
        init.print <> cond.print <> increment.print <>
        ")" <> body.print
    }
  }

  case class ForLoopIm(init: DeclarationT,
                       cond: ExpressionStatement,
                       increment: ExpressionT,
                       body: BlockT) extends ForLoopImT {
    override def _visitAndRebuild(pre: (AstNode) => AstNode,  post: (AstNode) => AstNode) : AstNode = {
      ForLoop(init.visitAndRebuild(pre, post).asInstanceOf[DeclarationT],
        cond.visitAndRebuild(pre, post).asInstanceOf[ExpressionStatement],
        increment.visitAndRebuild(pre, post).asInstanceOf[ExpressionT],
        body.visitAndRebuild(pre, post).asInstanceOf[MutableBlockT])
    }

    override def _visit(pre: (AstNode) => Unit, post: (AstNode) => Unit) : Unit = {
      init.visitBy(pre, post)
      cond.visitBy(pre, post)
      increment.visitBy(pre, post)
      body.visitBy(pre, post)
    }
  }

  trait ParamDeclPureT extends DeclarationT {
    val name: String
    val t: CTypeT
    val const: Boolean // = false

    override def print(): Doc = {const match {case true => "const "; case _ => ""} } <> t.print() <> " " <> name

  }


  case class ParamDeclPure(name: String, t: CTypeT, const: Boolean = false) extends ParamDeclPureT{

    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this
  }

  trait FunctionPureT extends DeclarationT {
    def name: String

    def ret: CTypeT

    def params: List[ParamDeclPureT]

    def body: BlockT

    def attribute: Option[AttributeT]

    override def print(): Doc = ret.print() <> " " <> name <> "(" <>
      intersperse(params.map(_.print())) <> ")" <>
      bracket("{", body.print(), "}")
  }

  case class FunctionPure(name: String, ret: CTypeT, params: List[ParamDeclPureT],
                          body: BlockT, attribute: Option[AttributeT] = None) extends FunctionPureT {
    override def _visit(pre: AstNode => Unit, post: AstNode => Unit): Unit = ()
    override def _visitAndRebuild(pre: (AstNode) => AstNode, post: (AstNode) => AstNode): AstNode = this

  }
}