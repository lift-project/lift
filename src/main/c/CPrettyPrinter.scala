package c
import CAST._
import apart.arithmetic.{Cst, Sum, Var}
import c.CType.Int32
/**
  * Created by Federico on 14-Jun-16.
  */
object CPrettyPrinter {

  def prettyPrint(node:CNode): String = node match {
    case Function(name,returnType,parameters,body) =>
      returnType.toString ++ " " ++ name ++ parens(commaList(parameters)) ++ prettyPrint(body)
    case ParameterDecl(name,t) => t.toString ++ " " ++ name
    case VarDecl(v,t,init) => {
      val rhs = (if (init == null) "" else (" = " ++ prettyPrint(init))) ++ ";\n"
      t.toString ++ " " ++ v.name ++ rhs
    }
    case Block(contents) => brakets(contents.map(prettyPrint(_)).foldLeft("")(_ ++ _))
    case DeclStatement(decl) => prettyPrint(decl)
    case ExpressionStatement(exp) => prettyPrint(exp) ++ ";\n"
    case WhileLoop(cond,body) => "while " ++ parens(prettyPrint(cond)) ++ brakets(prettyPrint(body))
    case IfThenElse(cond,tb,fb) => {
      val trueBranch = brakets(prettyPrint(tb))
      val falseBranch = if(fb == null) "" else("else " ++ brakets(prettyPrint(fb)))
      "if " ++ parens(prettyPrint(cond)) ++ trueBranch ++ falseBranch
    }
    case FunctionCall(name,args) => name ++ parens(commaList(args))
    case ArithExpression(arithExpr) => arithExpr.toString
  }

  private def commaList(ls:Seq[CNode]) = {
    val strs = ls.map(prettyPrint(_))
    if(ls.isEmpty)
      ""
    else
      strs.reduce(_ ++ ", " ++ _)
  }

  private def parens(str:String):String = "(" ++ str ++ ")"
  private def brakets(str:String):String = "{\n" ++ str ++ "}\n"

  def main(args:Array[String]) = {
      val ast = Function("main", Int32, List(ParameterDecl("p1",Int32)),
        Block(List(
          VarDecl(Var("x"),Int32,ArithExpression(Cst(4))),
          WhileLoop(ArithExpression(Sum(List(Cst(0),Cst(1)))), ExpressionStatement(FunctionCall("test",List.empty))),
          IfThenElse(
            ArithExpression(Cst(1)),
            DeclStatement(VarDecl(Var("y"),Int32,ArithExpression(Cst(1)))),
            ExpressionStatement(FunctionCall("otherTest",List(ArithExpression(Cst(1))))))
        ))
      )
      println(prettyPrint(ast))
  }
}
