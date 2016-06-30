package c.generator

import apart.arithmetic.{ArithExpr, Cst, Pow, Var}
import c.generator.CAst._
import opencl.generator.{DeadCodeElimination, OclFunction}
import opencl.ir.{Int, PrivateMemory}

import scala.collection.mutable

object CommonSubexpressionElimination {
  def apply(block: Block): Unit = {
    visitBlocks(block, processBlock)

    def processBlock(block: Block): Unit = {
      // get all the arithmetic expressions from this block
      var expressions = Seq[ArithExpression]()
      visitExpressionsInBlock(block, {
        case e: ArithExpression => expressions = expressions :+ e
        case _ =>
      })

      // map for counting how often subterms appear
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
        .filter(!_._1.isInstanceOf[OclFunction])
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

          CAst.VarDecl(newVar,
            t = Int,
            init = CAst.ArithExpression(p._1),
            addressSpace = PrivateMemory)
        })

      // update the Expression nodes to
      expressions.foreach {
        case ae: ArithExpression => ae.content = ArithExpr.substitute(ae.content, substitutions.toMap)
      }

      // find actually used variables
      val usedVars: mutable.Set[VarDecl] = mutable.Set()
      expressions.foreach(expr => {
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
        usedVars.foreach(_ :: (block: Block))
      else
        newVarDecls.foreach(_ :: (block: Block))
    }
  }
}
