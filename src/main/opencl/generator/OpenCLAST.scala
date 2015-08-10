package opencl.generator

import java.io._

import apart.arithmetic.{Predicate, IfThenElse, ArithExpr, Var}
import ir.{TupleType, VectorType, Type}
import opencl.ir.{UndefAddressSpace, OpenCLAddressSpace, OpenCLMemory}

object OpenCLAST {

  /** Base class for all OpenCL AST nodes.*/
  abstract class OclAstNode

  /** List of nodes enclosed in a bock. This behaves like (and emits) a C block. */
  case class Block(var content: List[OclAstNode] = List.empty, global: Boolean = false) extends OclAstNode{
    /** Append a sub-node. Could be any node, including a sub-block.
      * @param node The node to add to this block.
      */
    def +=(node: OclAstNode): Unit = content = content :+ node
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
                          params: List[OpenCLAST.OclAstNode]) extends OclAstNode

  case class Loop(indexVar: Var,
                  iter: ArithExpr,
                  body: Block,
                  unrollHint: Boolean = false) extends OclAstNode

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

  /** Parameter declaration. These have to be separated from variable declaration since the
    * vectorization has to be handled differently
    */
  case class ParamDecl(name: String, t: Type,
                       addressSpace: OpenCLAddressSpace = UndefAddressSpace,
                       const: Boolean = false) extends OclAstNode

  /** A reference to a declared variable
    * @param name The name of the variable referenced.
    * @param offset Offset used to index from pointers, if any.
    * @note This uses a String instead of a Var because some nodes (like user
    *       functions), inject variables from string.
    */
  case class VarRef(name: String, offset: Expression = null) extends OclAstNode

  /** Represent an assignment.
    * @param to Left-hand side.
    * @param value Right-hand side.
    * @note Vectors are using Store instead of assignment.
    */
  case class Assignment(to: OclAstNode, value: OclAstNode) extends OclAstNode

  /** Inline native code block. Used mainly for UserFun, which are currently represented as strings
    * @param code Native code to insert
    */
  case class Inline(code: String) extends OclAstNode

  /** Inline comment block
    * @param content Comment string
    */
  case class Comment(content: String) extends OclAstNode

  /** Wrapper for arithmetic expression
    * @param content The arithmetic expression.
    */
  case class Expression(content: ArithExpr) extends OclAstNode
}
