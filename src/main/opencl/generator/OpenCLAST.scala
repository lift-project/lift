package opencl.generator

import java.io._

import apart.arithmetic.{Predicate, IfThenElse, ArithExpr, Var}
import ir.{TupleType, VectorType, Type}
import opencl.ir.{UndefAddressSpace, OpenCLAddressSpace, OpenCLMemory}

object OpenCLAST {

  /**
   * Base class for all OpenCL AST nodes
   */
  abstract class OclAstNode {
    val id = {
      OclAstNode.counter += 1
      OclAstNode.counter
    }
  }
  object OclAstNode {
    var counter = 0
  }

  case class Block(var content: List[OclAstNode] = List.empty, global: Boolean = false) extends OclAstNode{
    def +=(node: OclAstNode) = {
      content = content :+ node
      this
    }

    override def toString = "Block"
  }

  /** A function declaration
    * @param name Name of the function.
    * @param ret Return type.
    * @param params List of parameter declaration.
    * @param body Body of the function.
    * @param kernel Flag set if the function is a kernel
    *
    * @note The type system does not have a `void` type, a return type set to `null` represents a `void` type.
    */
  case class Function(name: String, ret: Type, params: List[ParamDecl], body: Block, kernel: Boolean = false) extends OclAstNode {
    override def toString = "Function"
  }

  case class FunctionCall(name: String, params: List[OpenCLAST.OclAstNode]) extends OclAstNode {
    override def toString = "FunctionCall"
  }

  case class Loop(indexVar: Var, iter: ArithExpr, body: Block, unrollHint: Boolean = false) extends OclAstNode {
    override def toString = "Loop"
  }

  case class Selection(condition: Predicate, then_block: Block, else_block: Block = null) extends OclAstNode {
    override def toString = "Selection"
  }

  case class Barrier(mem: OpenCLMemory) extends OclAstNode {
    override def toString = "Barrier"
  }

  case class TypeDef(t: Type) extends OclAstNode {
    override def toString = "Typedef"
  }

  case class TupleAlias(t: Type, name: String) extends OclAstNode

  case class VarDecl(name: String,
                     t: Type,
                     init: OclAstNode = null,
                     addressSpace: OpenCLAddressSpace = UndefAddressSpace,
                     length: Int = 0) extends OclAstNode {
    override def toString = "VarDecl"
  }

  case class Load(v: VarRef,
                  t: VectorType,
                  offset: Expression) extends OclAstNode

  case class Store(v: VarRef,
                   t: VectorType,
                   value: OclAstNode,
                   offset: Expression) extends OclAstNode

  case class Cast(v: VarRef, t: Type) extends OclAstNode

  /**
   * Parameter declaration. These have to be separated from variable declaration since the
   * vectorization has to be handled differently
   */
  case class ParamDecl(name: String, t: Type,
                       init: OclAstNode = null,
                       addressSpace: OpenCLAddressSpace = UndefAddressSpace,
                       const: Boolean = false)
    extends OclAstNode
  {
    override def toString = "VarDecl"
  }

  case class VarRef(name: String, offset: Expression = null) extends OclAstNode {
    override def toString = "Function"
  }

  case class Assignment(to: OclAstNode, value: OclAstNode) extends OclAstNode {
    override def toString = "Assignment"
  }

  /** Inline native code block. Used mainly for UserFun, which are currently represented as strings
    */
  case class Inline(code: String) extends OclAstNode {
    override def toString = "Inline"
  }

  case class Comment(content: String) extends OclAstNode {
    override def toString = "/*...*/"
  }

  case class Expression(content: ArithExpr) extends OclAstNode {
    override def toString = "Expression"
  }

  object SimpleDot {
    var dotcount = 0
    var count = 0
    var bw: BufferedWriter = null

    def addNode(label: String, shape: String = "ellipse"): Int = {
      count += 1
      val id = count
      bw.write(
        s"""  $id [label = "$label", shape = "$shape"]
         """.stripMargin)
      id
    }

    def addLink(from: Int, to: Int, label: String = ""): Unit = {
      if(from >= 0 && to >= 0)
        bw.write(
          s"""  $from -> $to [label = "$label", dir=back]
           """.stripMargin)
    }

    def visit(node: OclAstNode): Int = {
      // Current node
      val id = node match {
        case c: Comment => return -1
        case l: Loop => addNode(s"{Loop|ind. var: ${l.indexVar}\\niter: ${l.iter}}", shape = "record")
        case f: Function => addNode(s"{Function${if(f.kernel) " (K)" else ""}|name: ${f.name}}", shape = "record")
        case v: VarDecl => addNode(s"{${v.toString}|name: ${v.name}\\ntype: ${v.t}}", shape = "record")
        case x => addNode(x.toString)
      }

      // Traverse
      node match {
        case l: Loop =>
          addLink(id, visit(l.body))
        case f: Function =>
          f.params.foreach(x => {
            addLink(id, visit(x), "param")
          })
          addLink(id, visit(f.body), "body")
        case b: Block => b.content.foreach(x => {
          addLink(id, visit(x))
        })
        case x =>
      }
      id
    }

    def apply(node: OclAstNode): Unit = {
      count = 0
      dotcount += 1
      val file = new File(s"PLOT$dotcount.dot")
      bw = new BufferedWriter(new FileWriter(file))
      bw.write("""digraph {
          |  edge [fontsize = "10"]
          |  node [fontsize = "10", shape = "box", style="filled", fillcolor="aquamarine"];
        """.stripMargin)
      visit(node)
      bw.write("}\n")
      bw.close()
    }
  }
}
