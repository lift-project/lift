package c

import apart.arithmetic.{ArithExpr, Var}
import ir.Type

/**
  * Created by Federico on 14-Jun-16.
  */
object CAST {
  sealed trait CNode
  trait BlockMember

  sealed abstract class Declaration extends CNode with BlockMember
  sealed abstract class Statement extends CNode with BlockMember
  sealed abstract class Expression extends CNode

  case class Function(name:String,
                      returnType:Type,
                      parameters:List[ParameterDecl],
                      body:Block) extends Declaration

  case class ParameterDecl(name:String,
                           t:Type) extends Declaration
  case class VarDecl(v:Var,
                     t:Type,
                     init:Expression) extends Declaration

  case class Block(contents:List[CNode with BlockMember]) extends Statement


  case class DeclStatement(decl:Declaration) extends Statement
  case class ExpressionStatement(e: Expression) extends Statement

  case class WhileLoop(cond:Expression, body:Statement) extends Statement
  case class IfThenElse(cond:Expression,
                       trueBranch:Statement,
                       falseBranch:Statement) extends Statement

  case class FunctionCall(name: String,
                          args: List[Expression]) extends Expression
  case class ArithExpression(var content: ArithExpr) extends Expression

}
