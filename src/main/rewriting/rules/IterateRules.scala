package rewriting.rules

import ir.ast.{FunCall, Iterate}

object IterateRules {

  val iterate1 = Rule("Iterate(1, x) => x", {
    case FunCall(Iterate(n, f, _, _, _), arg) if n.eval == 1 => f(arg)
  })

  val iterateId = Rule("Iterate(0, _) => Epsilon", {
    case FunCall(Iterate(n, _ , _, _, _), arg) if n.eval == 0 => arg
  })

  /* Iterate decomposition rule */

  // TODO
}
