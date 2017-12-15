package generic.ast

import ir.Type
import lift.arithmetic.Var


object GenericAST {

  trait AstNode

  trait BlockMember

  abstract class Attribute extends AstNode

   abstract class Declaration extends AstNode with BlockMember

   abstract class Statement extends AstNode with BlockMember

   abstract class Expression extends AstNode

  /**
    * A function declaration
    */

  class Function(name: String, ret: Type, params: List[ParamDecl],
                 body: Block) extends Declaration

  case class VarDecl(v: Var,
                     t: Type,
                     init: AstNode = null,
                     length: Long = 0) extends Declaration

  /** Parameter declaration. These have to be separated from variable
    * declaration since the vectorization has to be handled differently
    */
  abstract class ParamDecl(name: String, t: Type,
                       const: Boolean = false) extends Declaration


  /**
    * List of nodes enclosed in a bock. This behaves like (and emits) a C block.
    */
  abstract class Block(var content: Vector[AstNode with BlockMember] = Vector.empty,
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
  }

}