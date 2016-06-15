package c
import CAST._
import apart.arithmetic.{Cst, Sum, Var}
import c.CType.Int32
/**
  * Created by Federico on 14-Jun-16.
  */
object CPrettyPrinter {

  def prettyPrint(node:CNode): String = node match {
    case FunctionDecl(name,returnType,parameters,body) =>
      returnType.toString ++ " " ++ name ++ parens(commaList(parameters)) ++ prettyPrint(body)
    case ParameterDecl(name,t) => t.toString ++ " " ++ name
    case x:VarDecl => prettyPrintVar(x)
    case Block(contents) => brakets(contents.map(prettyPrint(_)).foldLeft("")(_ ++ _))
    case DeclStatement(decl) => prettyPrint(decl)
    case ExpressionStatement(exp) => prettyPrint(exp) ++ ";\n"
    case PragmaStatement(str) => "#pragma " ++ str
    case WhileLoop(cond,body) => "while " ++ parens(prettyPrint(cond)) ++ brakets(prettyPrint(body))
    case IfThenElse(cond,tb,fb) => {
      val trueBranch = brakets(prettyPrint(tb))
      val falseBranch = if(fb == null) "" else("else " ++ brakets(prettyPrint(fb)))
      "if " ++ parens(prettyPrint(cond)) ++ trueBranch ++ falseBranch
    }
    case FunctionCall(name,args) => name ++ parens(commaList(args))
    case ArithExpression(arithExpr) => arithExpr.toString
  }

  private def prettyPrintVar(v: VarDecl) = {
    val size = v.size.eval
    val arrayPart = if(v.size.eval == 0) "" else s"[$size]"
    val lhs = s"${v.t} ${v.v.name}$arrayPart"
    val rhs = (if (v.init == null) "" else (" = " ++ prettyPrint(v.init))) ++ ";\n"
    s"$lhs $rhs"
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
      val ast = FunctionDecl("main", Int32, List(ParameterDecl("p1",Int32)),
        Block(List(
          VarDecl(Var("x"),Int32, Cst(4),ArithExpression(Cst(4))),
          WhileLoop(ArithExpression(Sum(List(Cst(0),Cst(1)))), ExpressionStatement(FunctionCall("test",List.empty))),
          IfThenElse(
            ArithExpression(Cst(1)),
            DeclStatement(VarDecl(Var("y"),Int32,Cst(0),ArithExpression(Cst(1)))),
            ExpressionStatement(FunctionCall("otherTest",List(ArithExpression(Cst(1))))))
        ))
      )
      println(prettyPrint(ast))
  }
}
