package test

object Exploration {

  def eval(f: Fun) : Double = {    
    // TODO: a few random descent to evaluate performance
    0.0
  }
  
  def searchBest(topF: Fun, f: Fun, inputType: Type, c: Constraints = new Constraints(3, false)) : Fun = {

    Type.check(f, inputType)
    Context.updateContext(f, new Context())
    
    // first horizontally
    val bestH = f match {
      case cf: CompFun => {
        val newFuns = cf.funs.map(inF => searchBest(topF, inF, inF.inT, c))        
        if (newFuns.length == 1)
          return newFuns(0)
        else
          CompFun(newFuns: _*)
      }
      case p : Pattern => {
        val choices = Rules.outerDerivations(p, c)
        val minIdx = choices.map(choice => {
          val updatedFun = Fun.replaceRef(topF, f, choice)
          eval(updatedFun)
          }).zipWithIndex.minBy({case(d,i) => d})(Ordering[Double])._2
        choices(minIdx)
      }
      case _ => f
    }

    Type.check(bestH, inputType)
    Context.updateContext(bestH, new Context())
    
    // then vertically
    val bestV = bestH match {
      case fp :FPattern => fp.getClass().getConstructor(classOf[Fun]).newInstance(searchBest(topF, fp.fun, fp.inT, c))      
      case _ => bestH        
    }
    
    bestV

  }
  
  /*
   * Returns a list of alternatives functions equivalent to f.
   * The return list does not include f. 
   */
  private def alternatives(f: Fun, inputType: Type, level: Int,
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
      if (cnt > 2)
        c = new Constraints(c.maxMapDepth, true)
      newAlts = newAlts.map(r => alternatives(r, inputType, level, c)).flatten
      alts = alts ++ newAlts
      cnt = cnt+1
    }
    
    val results = alts.map(r => bfs(r, inputType, maxDepth, constraints, level+1)).flatten
    
    results + f
  }
  
}