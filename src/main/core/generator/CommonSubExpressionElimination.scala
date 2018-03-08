package core.generator

import core.generator.GenericAST._
import lift.arithmetic._
import opencl.ir.Int
import opencl.generator.DeadCodeElimination
//import opencl.ir.{Int, PrivateMemory}

import scala.collection.mutable

object CommonSubexpressionElimination {
  def apply(node: AstNode) : Unit = {

    // visit each of the blocks in node:
    node.visit[Unit](())({
      case (_, b: MutableBlock) => processBlock(b)
      case _                    => // do nothing, it's not a block, so we don't want to process it
    })

    def processBlock(block: MutableBlock) : Unit = {
      // traverse the block, and accumulate the arithmetic expressions
      val expressions = block.visit(Seq[ArithExpression]())({
        case (exprs, ae: ArithExpression) => exprs :+ ae
        case (exprs, _) => exprs
      })

      // create a map to count how often subterms appear
      val counts = mutable.Map[ArithExpr, Int]()

      // count how many times a subterm appears in the expressions
      expressions.foreach({
        case ae: ArithExpression =>
          ArithExpr.visit(ae.content, subterm => {
            counts get subterm match {
              case None => counts put(subterm, 1)
              case Some(c) => counts put(subterm, c + 1)
            }
          })
        case _ =>
      })

      // just choose the subterms which appear more than once and are not
      // instances of Var (i.e. are already variables)
      val subterms = counts.filter(_._2 > 1)
        .filter(!_._1.isInstanceOf[Var])
        .filter(!_._1.isInstanceOf[Cst])
        .filter(!_._1.isInstanceOf[ArithExprFunction])
        .filter(_._1 match {
          // don't choose pow(b, -1), as this might be
          // printed as "/b" inside of a product
          case Pow(_, Cst(-1)) => false
          case _ => true
        })

      val substitutions = mutable.Map[ArithExpr, ArithExpr]()

      val newVarDecls: mutable.Iterable[VarDecl] =
      // for every subterm p._1 ...
        subterms.map(p => {
          // ... create a new variable ...
          val newVar = Var("")
          // ... and prepare the substitution of the old expression with the
          //     new variable
          substitutions put(p._1, newVar)

          VarDecl(
            v = CVar(newVar),
            t = Int,
            init = Some(ArithExpression(p._1))
          )
        })

      // update the Expression nodes to
      val updatedExpressions = expressions.map{
        case ae: ArithExpression => ArithExpression(ArithExpr.substitute(ae
          .content,
          substitutions.toMap))
      }

      // find actually used variables
      val usedVars: mutable.Set[VarDecl] = mutable.Set()
      updatedExpressions.foreach(expr => {
        ArithExpr.visit(expr.content, {
          case v: Var => newVarDecls.find(_.v == v) match {
            case Some(s) => usedVars += s
            case None =>
          }
          case _ => // just interested in used vars
        })
      })

      // introduce new var decls at the beginning of the current block
      if (DeadCodeElimination())
        usedVars.foreach(_ :: (block: MutableBlock))
      else
        newVarDecls.foreach(_ :: (block: MutableBlock))
    }
  }
}
