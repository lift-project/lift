+package c
import c.AST._
import c.Pattern.MapSeq
import ir.ast.{Expr, FunCall, FunDecl, Lambda, Param, Value}
/**
  * Created by Federico on 16-Jun-16.
  */
object Generator {
  private var freshNameCounter = -1
  def generate(f:Lambda):FunctionDecl = {
    val name = freshName("fun") //Constant for now, need fresh name generator
    val returnType = f.body.t
    val params = f.params.map(generateParam(_)).toList
    val body = generateBody(f.body)
    FunctionDecl(name, returnType, params, body)
  }

  private def generateParam(param:Param):ParameterDecl = ParameterDecl(freshName("param"), param.t)

  private def generateBody(expr:Expr):Block = Block(List(generateStatement(expr)))
  private def generateStatement(expr:Expr):Statement = expr match {
    case f:FunCall => generateFunCall(f)

  }

  private def generateFunCall(call:FunCall):Statement = {
    //First, we generate code to compute the arguments
    val argsStms = call.args.map(generateStatement(_)).toList
    //If this is a lambda, 'explode' it here
    val fStms = call match {
      case lambda:Lambda => generateBody(lambda.body)
      case map:MapSeq => generateMapSeq(map)
    }
    //Finally, package the two in a C block and return
    Block(argsStms ++ List(fStms))
  }

  private def generateMapSeq(map:MapSeq):Statement = {
    //To generate the map sequential, we need to generate a for-loop

    //TOASK: Is the assumption that variable x in lambda body is the same as ARRAY x in expression to which the lambda
    //is applied? If so, map (x -> x + 1) (e(ls)) should result in some C like

    //x = e(ls) <- generated in generateFunCall, x is an array
    //for(int i = 0; i < sizeof(x), i++) {
    //x[i] = x[i] + 1
    //}
    ???
  }

  private def freshName(kind:String) = {
    freshNameCounter += 1
    s"${kind}_$freshNameCounter"
  }
}
