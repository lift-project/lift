package opencl.generator

import generic.ast.GenericAST._
import lift.arithmetic.{ArithExpr, Predicate, Var}
import ir.{TupleType, Type, VectorType}
import opencl.ir.{OpenCLAddressSpace, OpenCLMemory, UndefAddressSpace}

import scala.language.implicitConversions

object OpenCLAST {

//  /** Base class for all OpenCL AST nodes. */
//  sealed trait OclAstNode
//
//  trait BlockMember
//
//  implicit def exprToStmt(e: Expression): ExpressionStatement = ExpressionStatement(e)
//
//  implicit def predicateToCondExpression(p: Predicate): BinaryExpression = {
//    BinaryExpression(
//      ArithExpression(p.lhs),
//      p.op match {
//        case Predicate.Operator.!= => BinaryExpression.Operator.!=
//        case Predicate.Operator.< => BinaryExpression.Operator.<
//        case Predicate.Operator.<= => BinaryExpression.Operator.<=
//        case Predicate.Operator.== => BinaryExpression.Operator.==
//        case Predicate.Operator.> => BinaryExpression.Operator.>
//        case Predicate.Operator.>= => BinaryExpression.Operator.>=
//      },
//      ArithExpression(p.rhs)
//    )
//  }
//
//  sealed abstract class Attribute extends OclAstNode
//
//  sealed abstract class Declaration extends OclAstNode with BlockMember
//
//  sealed abstract class Statement extends OclAstNode with BlockMember
//
//  sealed abstract class Expression extends OclAstNode

  case class RequiredWorkGroupSize(localSize: NDRange) extends AttributeT

  /*
   OpenCL specific traits
   */
  trait IsKernel {
    val kernel : Boolean
  }

  trait CLAddressSpace {
    val addressSpace : OpenCLAddressSpace
  }

  /** A function declaration
    *
    * @param name   Name of the function.
    * @param ret    Return type.
    * @param params List of parameter declaration.
    * @param body   Body of the function.
    * @param kernel Flag set if the function is a kernel
    */
  case class OclFunction(name: String,
                         ret: Type,
                         params: List[ParamDeclT],
                         body: Block,
                         attribute: Option[AttributeT] = None, kernel: Boolean =
                      false) extends
    FunctionT with IsKernel

  case class OclVar(v: lift.arithmetic.Var, t: Type,
                    addressSpace: OpenCLAddressSpace) extends VarT with CLAddressSpace

  case class OclVarDecl(v: OclVar,
                     t: Type,
                     init: AstNode = null,
                     length: Long = 0,
                     addressSpace: OpenCLAddressSpace = UndefAddressSpace)
    extends VarDeclT with CLAddressSpace

  /** Parameter declaration. These have to be separated from variable
    * declaration since the vectorization has to be handled differently
    */
  case class OclParamDecl(name: String, t: Type,
                       const: Boolean = false,
                       addressSpace: OpenCLAddressSpace = UndefAddressSpace)
    extends ParamDeclT with CLAddressSpace


  case class OclLoad(v: VarRef, offset: ArithExpression,
                     addressSpace: OpenCLAddressSpace) extends LoadT
    with CLAddressSpace

  // TODO: Can we just get the address space from the var ref?
  case class OclStore(v: VarRef,
                      value: AstNode,
                      offset: ArithExpression,
                      addressSpace: OpenCLAddressSpace) extends StoreT
    with CLAddressSpace


  case class VectorLiteral(t: VectorType, vs: VarRef*) extends ExpressionT


  /** Inline native code block. Used mainly for UserFun, which are currently
    * represented as strings
    *
    * @param code Native code to insert
    */
  case class OpenCLCode(code: String) extends RawCodeT

  case class OpenCLExtension(content: String) extends AstNode with BlockMember

//
//  def visitExpressionsInBlock(block: Block, fun: Expression => Unit): Unit = {
//    visitExpressionsInNode(block)
//
//    def visitExpressionsInNode(node: OclAstNode): Unit = {
//      callFunOnExpression(node)
//
//      node match {
//        case e: Expression => visitExpression(e)
//        case s: Statement => visitStatement(s)
//        case d: Declaration => visitDeclaration(d)
//        case Comment(_) | OpenCLCode(_) | OpenCLExtension(_) | RequiredWorkGroupSize(_) =>
//      }
//    }
//
//    def callFunOnExpression(node: OclAstNode): Unit = {
//      node match {
//        case e: Expression => fun(e)
//        case _: Statement =>
//        case _: Declaration =>
//        case Comment(_) | OpenCLCode(_) | OpenCLExtension(_) | RequiredWorkGroupSize(_) =>
//      }
//    }
//
//    def visitExpression(e: Expression): Unit = e match {
//      case _: ArithExpression =>
//      case _: OpenCLExpression =>
//      case a: AssignmentExpression =>
//        visitExpressionsInNode(a.value)
//        visitExpressionsInNode(a.to)
//      case c: Cast =>
//        visitExpressionsInNode(c.v)
//      case pc : PointerCast =>
//        visitExpressionsInNode(pc.v)
//      case BinaryExpression(lhs, _, rhs) =>
//        visitExpressionsInNode(lhs)
//        visitExpressionsInNode(rhs)
//      case TernaryExpression(cond, trueExpr, falseExpr) =>
//        visitExpression(cond)
//        visitExpression(trueExpr)
//        visitExpression(falseExpr)
//      case f: FunctionCall =>
//        f.args.foreach(visitExpressionsInNode)
//      case l: Load =>
//        visitExpressionsInNode(l.v)
//        visitExpressionsInNode(l.offset)
//      case s: Store =>
//        visitExpressionsInNode(s.v)
//        visitExpressionsInNode(s.value)
//        visitExpressionsInNode(s.offset)
//      case s: StructConstructor =>
//        s.args.foreach(visitExpressionsInNode)
//      case v: VarRef =>
//        if (v.arrayIndex != null) visitExpressionsInNode(v.arrayIndex)
//      case v: VectorLiteral =>
//        v.vs.foreach(visitExpressionsInNode)
//    }
//
//    def visitStatement(s: Statement): Unit = s match {
//      case b: Block => b.content.foreach(visitExpressionsInNode)
//      case es: ExpressionStatement => visitExpressionsInNode(es.e)
//      case f: ForLoop =>
//        visitExpressionsInNode(f.init)
//        visitExpressionsInNode(f.cond)
//        visitExpressionsInNode(f.increment)
//        visitExpressionsInNode(f.body)
//      case ifte: IfThenElse =>
//        visitExpressionsInNode(ifte.cond)
//        visitExpressionsInNode(ifte.trueBody)
//        visitExpressionsInNode(ifte.falseBody)
//      case w: WhileLoop =>
//        visitExpressionsInNode(w.loopPredicate)
//        visitExpressionsInNode(w.body)
//      case Barrier(_) | GOTO(_) | TupleAlias(_, _) | TypeDef(_) | Break() =>
//    }
//
//    def visitDeclaration(d: Declaration): Unit = d match {
//      case f: OclFunction => visitExpressionsInNode(f.body)
//      case v: VarDecl => if (v.init != null) visitExpressionsInNode(v.init)
//      case Label(_) | ParamDecl(_, _, _, _) =>
//    }
//  }
//
//  def visitBlocks(node: OclAstNode, fun: Block => Unit): Unit = {
//    node match {
//      case _: Expression => // there are no blocks inside any expressions
//
//      case s: Statement => s match {
//        case b: Block =>
//          fun(b)
//          b.content.foreach(visitBlocks(_, fun))
//        case fl: ForLoop => visitBlocks(fl.body, fun)
//        case wl: WhileLoop => visitBlocks(wl.body, fun)
//        case ifte: IfThenElse =>
//          visitBlocks(ifte.trueBody, fun)
//          visitBlocks(ifte.falseBody, fun)
//        case GOTO(_) | Barrier(_) | TypeDef(_) | TupleAlias(_, _) | ExpressionStatement(_) | Break() =>
//      }
//
//      case d: Declaration => d match {
//        case f: OclFunction => visitBlocks(f.body, fun)
//        case Label(_) | VarDecl(_, _, _, _, _) | ParamDecl(_, _, _, _) =>
//      }
//
//      case Comment(_) | OpenCLCode(_) | OpenCLExtension(_) | RequiredWorkGroupSize(_) =>
//    }
//  }
}
