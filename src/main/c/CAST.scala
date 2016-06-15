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

  /**
    * Represents a C function declaration
    * @param name The name of the function
    * @param returnType The return type of the function
    * @param parameters The list of parameters
    * @param body The body of statements
    */
  case class FunctionDecl(name:String,
                          returnType:Type,
                          parameters:List[ParameterDecl],
                          body:Block) extends Declaration

  /**
    * Represents a parameter in a function parameter list
    * @param name The parameter variable name
    * @param t The type of the parameter
    */
  case class ParameterDecl(name:String,
                           t:Type) extends Declaration

  /**
    * Represents a C variable declaration. Scalar and Arrays are both implemented with this node, just set
    * size = 0 for scalar. The init block is optional by setting it to null
    * @param v The variable of the declaration
    * @param t The type of the variable
    * @param size The size of the variable, 0 for a scalar, else array size
    * @param init An optional initializing expression
    */
  case class VarDecl(v:Var,
                     t:Type,
                     size:ArithExpr,
                     init:Expression) extends Declaration

  /**
    * Represents a C { } block, also body of function
    * @param contents
    */
  case class Block(contents:List[CNode with BlockMember]) extends Statement

  /**
    * Wraps a declaration in a statement. Intended as an adapter for VarDecl
    * @param decl
    */
  case class DeclStatement(decl:Declaration) extends Statement

  /**
    * Turns an expression into a statement
    * @param e
    */
  case class ExpressionStatement(e: Expression) extends Statement

  /**
    * A C pragma directive. Pass in only the specific part of the pragma, the #pragma part is automatically
    * added
    * @param str
    */
  case class PragmaStatement(str:String) extends Statement

  /**
    * Node for a while loop
    * @param cond The conditional expression
    * @param body The body of the loop
    */
  case class WhileLoop(cond:Expression, body:Statement) extends Statement
  case class IfThenElse(cond:Expression,
                       trueBranch:Statement,
                       falseBranch:Statement) extends Statement

  case class FunctionCall(name: String,
                          args: List[Expression]) extends Expression
  case class ArithExpression(var content: ArithExpr) extends Expression
}
