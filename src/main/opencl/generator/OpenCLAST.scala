package opencl.generator

import apart.arithmetic.{Predicate, ArithExpr, Var}
import ir.{VectorType, Type}
import opencl.ir.{UndefAddressSpace, OpenCLAddressSpace, OpenCLMemory}

object OpenCLAST {

  /** Base class for all OpenCL AST nodes.*/
  abstract class OclAstNode

  /**
   * List of nodes enclosed in a bock. This behaves like (and emits) a C block.
   */
  case class Block(var content: Vector[OclAstNode] = Vector.empty,
                   global: Boolean = false) extends OclAstNode {
    /** Append a sub-node. Could be any node, including a sub-block.
      * @param node The node to add to this block.
      */
    def +=(node: OclAstNode): Unit = {
      content = content :+ node
    }

    def ::(node: OclAstNode): Unit = {
      content = node +: content
    }
  }

  /** A function declaration
    * @param name Name of the function.
    * @param ret Return type.
    * @param params List of parameter declaration.
    * @param body Body of the function.
    * @param kernel Flag set if the function is a kernel
    */
  case class Function(name: String,
                      ret: Type, params: List[ParamDecl],
                      body: Block,
                      kernel: Boolean = false) extends OclAstNode

  case class FunctionCall(name: String,
                          args: List[OpenCLAST.OclAstNode]) extends OclAstNode

  case class Loop(indexVar: Var,
                  iter: ArithExpr,
                  body: Block,
                  unrollHint: Boolean = false) extends OclAstNode

  /** An alternative looping construct, using a predicate - a `while' loop
    *  
    * @param loopPredicate the predicate the loop tests each iteration
    * @Param body the body of the loop
    */
  case class WhileLoop(loopPredicate: Predicate,
                       body: Block) extends OclAstNode

  /** An if-then-else set of statements, with two branches. 
    *
    * @param switchPredicate the predicate in the conditional
    * @param trueBody the body evaluated if switchPredicate is true
    * @param falseBody the body evaluated if switchPredicate is false
    */
  case class Conditional(switchPredicate: Predicate,
                         trueBody: Block,
                         falseBody: Block = Block()) extends OclAstNode

  /** A Label, targeted by a corresponding goto
    * 
    * @param name the name of label to be declared
    */
  case class Label(name: String) extends OclAstNode

  /** A goto statement, targeting the label with corresponding name
    * TODO: Think of a better way of describing goto labels
    *
    * @param name the name of the label to go to
    */
  case class GOTO(name: String) extends OclAstNode

  case class Barrier(mem: OpenCLMemory) extends OclAstNode

  case class TypeDef(t: Type) extends OclAstNode

  case class TupleAlias(t: Type, name: String) extends OclAstNode

  case class VarDecl(name: String,
                     t: Type,
                     init: OclAstNode = null,
                     addressSpace: OpenCLAddressSpace = UndefAddressSpace,
                     length: Int = 0) extends OclAstNode

  case class Load(v: VarRef,
                  t: VectorType,
                  offset: Expression) extends OclAstNode

  case class Store(v: VarRef,
                   t: VectorType,
                   value: OclAstNode,
                   offset: Expression) extends OclAstNode

  /** Force a cast of a variable to the given type. This is used to
    *
    * @param v A referenced variable.
    * @param t The type to cast the variable into.
    */
  case class Cast(v: VarRef, t: Type) extends OclAstNode

  /** Parameter declaration. These have to be separated from variable
    * declaration since the vectorization has to be handled differently
    */
  case class ParamDecl(name: String, t: Type,
                       addressSpace: OpenCLAddressSpace = UndefAddressSpace,
                       const: Boolean = false) extends OclAstNode

  /** A reference to a declared variable
    * @param v The variable referenced.
    * @param suffix An optional suffix appended to the name.
    *               Used e.g. for unrolled variables in private memory.
    * @param arrayIndex Offset used to index from pointers, if any.
    * @note This uses a String instead of a Var because some nodes (like user
    *       functions), inject variables from string.
    */
  case class VarRef(v: Var,
                    suffix: String = null,
                    arrayIndex: Expression = null) extends OclAstNode

  /** Represent an assignment.
    * @param to Left-hand side.
    * @param value Right-hand side.
    * @note Vectors are using Store instead of assignment.
    */
  case class Assignment(to: OclAstNode, value: OclAstNode) extends OclAstNode

  /** Inline native code block. Used mainly for UserFun, which are currently
    * represented as strings
    * @param code Native code to insert
    */
  case class OpenCLCode(code: String) extends OclAstNode

  /** Inline comment block
    * @param content Comment string
    */
  case class Comment(content: String) extends OclAstNode

  /** Wrapper for arithmetic expression
    * @param content The arithmetic expression.
    */
  case class Expression(var content: ArithExpr) extends OclAstNode


  case class Extension(content: String) extends OclAstNode

  def visitExpressionsInBlock(block: Block, fun: Expression => Unit): Unit = {

    block.content.foreach(visitExpression)

    def visitExpression(node: OclAstNode): Unit = {
      node match {
        case e: Expression => fun(e)
        case v: VarRef if v.arrayIndex != null => visitExpression(v.arrayIndex)
        case v: VarDecl if v.init != null => visitExpression(v.init)
        case l: Load => fun(l.offset)
        case s: Store =>
          visitExpression(s.value)
          fun(s.offset)
        case f: FunctionCall => f.args.foreach(visitExpression)
        case c: Cast => visitExpression(c.v)
        case a: Assignment =>
          visitExpression(a.value)
          visitExpression(a.to)
        case _ =>
      }
    }
  }

  def visitBlocks(node: OclAstNode, fun: Block => Unit): Unit = {
    node match {
      case b: Block =>
        fun(b)
        b.content.foreach(visitBlocks(_, fun))
      case f: Function => visitBlocks(f.body, fun)
      case l: Loop => visitBlocks(l.body, fun)
      case wl: WhileLoop => visitBlocks(wl.body, fun)
      case _ =>
    }
  }
}
