package test

import scala.util.Random

object Exploration {

  def evalPerf(f: Fun) : Double = {
    val perf = Random.nextDouble
    println(perf+" : "+f)
    perf
  }
  
  def bestChoice(topF: Fun, oriF: Fun, choices: Seq[Fun], c: Constraints) : Fun = {    
    // TODO: a few random descent to evaluate performance
    
    if (c.randomOnly == true) {
    	val constraints : Constraints = new Constraints(c.maxMapDepth, c.onlyTerminal, true)
    	return choices(Random.nextInt(choices.length))
    }
    //zipWithIndex.minBy({case(d,i) => d})(Ordering[Double])._2
    
    // generate a few random top level function with the oriF in place
    val newConstraints : Constraints = new Constraints(c.maxMapDepth, true, true)
    newConstraints.fixedFuns = newConstraints.fixedFuns + oriF
    var perfMap = scala.collection.mutable.Map[Fun, List[Double]]()
    for (i <- 1 to 10) {
    	val rndFun = searchBest(topF, topF, topF.inT, newConstraints)
    	choices.map(choice => {
    	  val perf = evalPerf(Fun.replaceRef(rndFun, oriF, choice))
    	  perfMap.update(choice, perfMap.getOrElse(choice, List[Double]()) :+ perf)
    	})    	
    }
    val medians = perfMap.map({case (key, vals) => {
      val sortedVals = vals.sorted
      val median = sortedVals(sortedVals.length/2)
      (key,median)
    }})
    
    medians.reduce((x,y) => if (x._2 < y._2) x else y)._1    
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
        //        val minIdx = choices.map(choice => {
        //          val updatedFun = Fun.replaceRef(topF, f, choice)
        //          eval(updatedFun)
        //          }).zipWithIndex.minBy({case(d,i) => d})(Ordering[Double])._2
        if (choices.isEmpty)
          p
        else
          bestChoice(topF, f, choices, c)         
      }
      case _ => f
    }
    // TODO: repeat the horizontal expansion

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