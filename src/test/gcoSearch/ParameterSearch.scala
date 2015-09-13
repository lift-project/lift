package gcoSearch

import apart.arithmetic.{Cst, Var, ArithExpr}
import exploration.Utils
import ir.{ArrayType, Type}
import ir.ast.{Split, FunCall, Lambda}
import scala.collection.immutable.{Map => ScalaImmMap}

import scala.collection.immutable.Map

/** Return all the possible parameter sets for a given expression. */
object ParameterSearch {
  // A substitution map is a collection of var/value pairs
  type SubstitutionMap = scala.collection.immutable.Map[ArithExpr, ArithExpr]

  // A substitution table represents all valid substitution maps
  type SubstitutionTable = List[SubstitutionMap]

  // Propagate the input types of a lambda
  def replaceInputTypes(lambda: Lambda): Unit = {
    val vars = lambda.params.flatMap(_.t.varList).distinct

    var st: SubstitutionMap = Map.empty

    vars.foreach(v => {
      st = st.updated(v, AppParams.matrix_size)
    })

    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))
  }

  // recursively build the substitution table.
  // It takes the first node to tune and recurse with all its possible values.
  def substitute(splits: List[(ArithExpr, ArithExpr)], substitutions: SubstitutionMap, table: SubstitutionTable): SubstitutionTable = {

    def propagate(splits: List[(ArithExpr, ArithExpr)], m: ScalaImmMap[ArithExpr, ArithExpr]): List[(ArithExpr, ArithExpr)] = {
      splits.map((x) => (ArithExpr.substitute(x._1, m), ArithExpr.substitute(x._2, m)))
    }

    if (splits.nonEmpty) {
      splits.head match {
        // If the stride is not set and the input length is constant, compute all divisors
        case (v: Var, Cst(len)) =>
          (2 to len - 1).filter {
            len % _ == 0
          }.foldLeft(table)((table, x) => substitute(propagate(splits.tail, ScalaImmMap(v -> x)), substitutions + (v -> x), table))

        // If the input AND the stride are already set, make sure they are multiple
        case (Cst(chunk), Cst(len)) if len % chunk == 0 =>
          substitute(splits.tail, substitutions, table)

        // Otherwise we cannot set the parameter or the current combination is invalid
        case (x, y) =>
          println(s"failed: $x, $y")
          table
      }
    }
    else // if we propagated all the nodes, we have a (hopefully valid) substitution table
      substitutions :: table
  }

  def apply(lambda: Lambda): SubstitutionTable = {
    // find all the nodes using variables
    val tunableNodes = Utils.findTunableNodes(lambda)

    // from that, isolate only the splits
    val splits = tunableNodes.collect { case f@FunCall(Split(cs), x) => (cs, x.t.asInstanceOf[ArrayType].len) }

    substitute(splits, Map.empty, List.empty)
  }
}
