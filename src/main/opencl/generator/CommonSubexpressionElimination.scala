package opencl.generator

import core.generator.GenericAST._
import lift.arithmetic._
import opencl.generator.OpenCLAST._
import opencl.ir.{Int, PrivateMemory}

import scala.collection.mutable

object CommonSubexpressionElimination {
  def apply(block: MutableBlock) : MutableBlock = {
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

    val newVarDecls: mutable.Iterable[OclVarDecl] =
    // for every subterm p._1 ...
      subterms.map(p => {
        // ... create a new variable ...
        val newVar = Var()
        // ... and prepare the substitution of the old expression with the
        //     new variable
        substitutions put(p._1, newVar)

        //          VarDecl(CVar(newVar),
        //            t = Int,
        //            init = ArithExpression(p._1))

        OclVarDecl(
          v = CVar(newVar),
          t = Int,
          init = Some(ArithExpression(p._1)),
          addressSpace = PrivateMemory
        )
      })

    // update the Expression nodes to
    val updatedExpressions = expressions.map {
      ae: ArithExpression => ArithExpression(ArithExpr.substitute(ae.content, substitutions.toMap))
    }

    // find actually used variables
    var usedVars: mutable.Set[OclVarDecl] = mutable.Set()
    updatedExpressions.foreach(expr => {
      ArithExpr.visit(expr.content, {
        case v: Var => newVarDecls.find(_.v.v == v) match {
          case Some(s) => usedVars += s
          case None =>
        }
        case _ => // just interested in used vars
      })
    })

    // introduce new var decls at the beginning of the current block
    if (DeadCodeElimination())
      block.content = usedVars.toVector.sortBy(_.v.v.toString) ++ block.content
    else
      block.content = newVarDecls.toVector.sortBy(_.v.v.toString) ++ block.content

    block.visitAndRebuild {
      case ae: ArithExpression =>
        if (expressions.contains(ae))
          ArithExpression(ArithExpr.substitute(ae.content, substitutions.toMap))
        else
          ae
      case x => x
    }.asInstanceOf[MutableBlock]
  }
}