package test

object Exploration {

  /*
   * Returns a list of alternatives functions equivalent to f.
   * The return list does not include f. 
   */
  def alternatives(f: Fun, inputType: Type, level: Int,
    constraints: Constraints = new Constraints(3, false)): Set[Fun] = {

    var c = constraints

    // setup the context
    //if (f.context == null)
      Context.updateContext(f, new Context())
    //else
      //Context.updateContext(f)

    // setup the types
    Type.check(f, inputType)

    Rules.derivsWithOneRule(f, c, level).toSet;
  }
  
  def bfs(f: Fun, inputType: Type, maxDepth: Int,      
      constraints: Constraints = new Constraints(3, false), level: Int = 0) : Set[Fun] = {

    var c = constraints
    
    if (level > maxDepth)
      return Set(f)

    var cnt = 0
    var alts = alternatives(f, inputType, level)
    var newAlts = alts
    while (newAlts.nonEmpty) {
      if (cnt > 0)
        c = new Constraints(c.maxMapDepth, true)
      newAlts = newAlts.map(r => alternatives(r, inputType, level, c)).flatten
      alts = alts ++ newAlts
      cnt = cnt+1
    }
    
    val results = alts.map(r => bfs(r, inputType, maxDepth, constraints, level+1)).flatten
    
    results + f
  }
  
}