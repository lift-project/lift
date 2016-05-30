package opencl.generator

import apart.arithmetic.{Predicate, ArithExpr, Var}
import ir.{Type, TupleType, VectorType}
import opencl.ir.{UndefAddressSpace, OpenCLAddressSpace, OpenCLMemory}

import scala.language.implicitConversions

object OpenCLAST {

  /** Base class for all OpenCL AST nodes.*/
  abstract sealed class OclAstNode

  trait BlockMember

  implicit def exprToStmt(e: Expression) : ExpressionStatement = ExpressionStatement(e)
  implicit def predicateToCondExpression(p: Predicate) : CondExpression = {
    CondExpression(ArithExpression(p.lhs), ArithExpression(p.rhs), p.op match {
      case Predicate.Operator.!= => CondExpression.Operator.!=
      case Predicate.Operator.< => CondExpression.Operator.<
      case Predicate.Operator.<= => CondExpression.Operator.<=
      case Predicate.Operator.== => CondExpression.Operator.==
      case Predicate.Operator.> => CondExpression.Operator.>
      case Predicate.Operator.>= => CondExpression.Operator.>=
    })
  }


  abstract class Statement extends OclAstNode with BlockMember
  abstract class Expression extends OclAstNode

  /**
   * List of nodes enclosed in a bock. This behaves like (and emits) a C block.
   */
  case class Block(var content: Vector[OclAstNode with BlockMember] = Vector.empty,
                   global: Boolean = false) extends Statement {
    /** Append a sub-node. Could be any node, including a sub-block.
      *
      * @param node The node to add to this block.
      */
    def +=(node: OclAstNode with BlockMember): Unit = {
      content = content :+ node
    }

    def add(node: OclAstNode with BlockMember) : Unit = {
      this.content :+ node
    }

    def ::(node: OclAstNode with BlockMember): Unit = {
      content = node +: content
    }
  }

  /** A function declaration
    *
    * @param name Name of the function.
    * @param ret Return type.
    * @param params List of parameter declaration.
    * @param body Body of the function.
    * @param kernel Flag set if the function is a kernel
    */
  case class Function(name: String,
                      ret: Type, params: List[ParamDecl],
                      body: Block,
                      kernel: Boolean = false) extends Statement

  case class FunctionCall(name: String,
                          args: List[OpenCLAST.OclAstNode]) extends Expression


  /**
    *
    * @param init: should either be an ExpressionStatement or VarDecl
    * @param cond
    * @param increment
    * @param body
    */
  case class ForLoop(init : Statement,
                     cond : ExpressionStatement,
                     increment: Expression,
                     body: Block) extends Statement

  /*case class Loop(indexVar: Var,
                  iter: ArithExpr,
                  body: Block,
                  unrollHint: Boolean = false) extends Statement*/

  /** An alternative looping construct, using a predicate - a 'while' loop
    *  
    * @param loopPredicate the predicate the loop tests each iteration
    * @param body the body of the loop
    */
  case class WhileLoop(loopPredicate: Predicate,
                       body: Block) extends Statement

  /** An if-then-else set of statements, with two branches. 
    *
    * @param cond the condition
    * @param trueBody the body evaluated if switchPredicate is true
    * @param falseBody the body evaluated if switchPredicate is false
    */
  case class IfThenElse(cond: Expression,
                        trueBody: Block,
                        falseBody: Block = Block()) extends Statement

  /** A Label, targeted by a corresponding goto
    * 
    * @param nameVar the name of label to be declared
    */
  case class Label(nameVar: Var) extends OclAstNode with BlockMember

  /** A goto statement, targeting the label with corresponding name
    * TODO: Think of a better way of describing goto labels
    *
    * @param nameVar the name of the label to go to
    */
  case class GOTO(nameVar: Var) extends Statement

  case class Barrier(mem: OpenCLMemory) extends Statement

  case class TypeDef(t: Type) extends Statement

  case class TupleAlias(t: Type, name: String) extends Statement

  case class VarUse(v: Var) extends Expression

  case class VarDecl(v: Var,
                     t: Type,
                     init: OclAstNode = null,
                     addressSpace: OpenCLAddressSpace = UndefAddressSpace,
                     length: Int = 0) extends Statement

  case class Load(v: VarRef,
                  t: VectorType,
                  offset: Expression) extends Expression

  case class Store(v: VarRef,
                   t: VectorType,
                   value: OclAstNode,
                   offset: Expression) extends Expression

  /** Force a cast of a variable to the given type. This is used to
    *
    * @param v A referenced variable.
    * @param t The type to cast the variable into.
    */
  case class Cast(v: VarRef, t: Type) extends Expression

  case class VectorLiteral(t: VectorType, vs: VarRef*) extends OclAstNode

  case class StructConstructor(t: TupleType, args: Vector[OclAstNode]) extends OclAstNode

  /** Parameter declaration. These have to be separated from variable
    * declaration since the vectorization has to be handled differently
    */
  case class ParamDecl(name: String, t: Type,
                       addressSpace: OpenCLAddressSpace = UndefAddressSpace,
                       const: Boolean = false) extends OclAstNode

  /** A reference to a declared variable
    *
    * @param v The variable referenced.
    * @param suffix An optional suffix appended to the name.
    *               Used e.g. for unrolled variables in private memory.
    * @param arrayIndex Offset used to index from pointers, if any.
    * @note This uses a String instead of a Var because some nodes (like user
    *       functions), inject variables from string.
    */
  case class VarRef(v: Var,
                    suffix: String = null,
                    arrayIndex: Expression = null) extends Expression

  /** Represent an assignment.
    *
    * @param to Left-hand side.
    * @param value Right-hand side.
    * @note Vectors are using Store instead of assignment.
    */
  case class AssignmentExpression(to: OclAstNode, value: OclAstNode) extends Expression

  /** Inline native code block. Used mainly for UserFun, which are currently
    * represented as strings
    *
    * @param code Native code to insert
    */
  case class OpenCLCode(code: String) extends OclAstNode with BlockMember

  /** Inline comment block.
    *
    * @param content Comment string*
    */
  case class Comment(content: String) extends OclAstNode with BlockMember

  case class Extension(content: String) extends OclAstNode with BlockMember

  case class ExpressionStatement(e: Expression) extends Statement


  /** Wrapper for arithmetic expression
    *
    * @param content The arithmetic expression.
    */
  case class ArithExpression(var content: ArithExpr) extends Expression

  case class CondExpression(lhs: Expression, rhs: Expression, cond: CondExpression.Operator.Operator) extends Expression

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
    }
  }



  def visitExpressionsInBlock(block: Block, fun: Expression => Unit): Unit = {

    block.content.foreach {
      case e: Expression => visitExpression(e)
      case o: OclAstNode => visitOclAstNode(o)
    }

    def visitOclAstNode(node: OclAstNode): Unit = {
      node match {
        case e: ExpressionStatement => visitExpression(e.e)
        case f: ForLoop => visitOclAstNode(f.body)
        case b: Block => visitExpressionsInBlock(b, fun)
        case v: VarDecl if v.init != null => visitOclAstNode(v.init)
        case o: OpenCLCode =>
        case v: VarDecl =>
        case c: Comment =>
        // TODO: implement the rest (simply recurse down)
      }
    }

    def visitExpression(node: Expression): Unit = {
      node match {
        case v: VarRef if v.arrayIndex != null => visitExpression(v.arrayIndex)
        case l: Load => fun(l.offset)
        case s: Store =>
          visitOclAstNode(s.value)
          fun(s.offset)
        case f: FunctionCall => f.args.foreach(visitOclAstNode(_))
        case c: Cast => visitExpression(c.v)
        case a: AssignmentExpression =>
          visitOclAstNode(a.value)
          visitOclAstNode(a.to)
          // TODO: implement the rest (simply recurse down)
      }
    }
  }

  def visitBlocks(node: OclAstNode, fun: Block => Unit): Unit = {
    node match {
      case b: Block =>
        fun(b)
        b.content.foreach(visitBlocks(_, fun))
      case f: Function => visitBlocks(f.body, fun)
     // case l: Loop => visitBlocks(l.body, fun)
      case fl: ForLoop => visitBlocks(fl.body, fun)
      case wl: WhileLoop => visitBlocks(wl.body, fun)
      case _ =>
    }
  }
}
