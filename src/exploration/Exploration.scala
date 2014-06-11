package exploration

import scala.util.Random
import ir.CompFun
import ir.Context
import ir.FPattern
import ir.Fun
import ir.Pattern
import ir.Type
import ir.UndefType

object Exploration {
  
  val verbose = true

  private def evalPerf(f: Fun, c: Constraints) : Double = {
    
    if (verbose) {
      println("------------------------------------------------------")
      println("Evaluating performance of "+f)
    }
    
    assert (f.inT != UndefType)      
    
    var perfs = List[Double]()
    val seen = scala.collection.mutable.Set[Fun]()
    for (i <- 0 to 0) {      
      val rndFun = search(f, new Constraints(c.maxMapDepth, true, true))

      if (!seen.contains(rndFun)) {
        seen += rndFun
        val perf = Random.nextDouble
        if (verbose) 
        	println(perf + " : " + rndFun)
        perfs = perfs :+ perf
      }
    }
    
    if (perfs.isEmpty)
      return Double.MaxValue  
      
    val sortedPerfs = perfs.sorted
    val median = sortedPerfs(sortedPerfs.length/2)
    
    median
    
    //Random.nextDouble
  }
  
  private def choose(topF: Fun, oriF: Fun, choices: Seq[Fun], c: Constraints) : Fun = {    
        
    if (choices.length == 1)
      return choices(0)
    
    if (c.randomOnly == true)
    	return choices(Random.nextInt(choices.length))  
    
    val rndTerFixed : Constraints = new Constraints(c.maxMapDepth, true, true)
    rndTerFixed.addFixedFun(oriF)
    
    val perfMap = scala.collection.mutable.Map[Fun, List[Double]]()
    val seen = scala.collection.mutable.Set[Fun]()
    
    // generate a few random top level function with the oriF in place
    for (i <- 0 to 3) {
      //println("---------------- "+i)
      val rndFun = search(topF, rndTerFixed)
      
      if (!seen.contains(rndFun)) {
        seen += rndFun
        
        choices.map(choice => {          
          val f = Fun.replaceRef(rndFun, oriF, choice)
          Type.check(f, topF.inT)
          Context.updateContext(f, topF.context)
          val perf = evalPerf(f, c)
          perfMap.update(choice, perfMap.getOrElse(choice, List[Double]()) :+ perf)
        })
      }        
    }
    
    val medians = perfMap.map({case (key, vals) => {
      val sortedVals = vals.sorted
      val median = sortedVals(sortedVals.length/2)
      (key,median)
    }})
    
    medians.reduce((x,y) => if (x._2 < y._2) x else y)._1    
  }
      
  private def search(topF: Fun, f: Fun, c:Constraints) : Fun = {
	assert (f.inT    != UndefType)
    assert (topF.inT != UndefType)
    assert (f.context != null)
    assert (topF.context != null)    

    val best = f match {
	  
      case cf: CompFun => {
        val newFuns = cf.funs.map(inF =>
          search(topF, inF, c))
        if (newFuns.length == 1)
          return newFuns(0)
        else
          CompFun(newFuns: _*)
      }
      
      case p: Pattern => {
        
        if (!c.canDerive(f))
    	   return p
      
        var choices = Rules.outerDerivations(p, c)
        if (p.isGenerable())
          choices = choices :+ p
        assert (choices.length > 0, "p="+p)
        // TODO: in case we don't have a choice, throw an exception since it is not possible to derive this expression
        
        val bestChoice = choose(topF, f, choices, c)
        Type.check(bestChoice, f.inT)
        Context.updateContext(bestChoice, f.context)    
        
        if (bestChoice == p) {
          assert(p.isGenerable)
          p match {
            case fp: FPattern => fp.getClass().getConstructor(classOf[Fun]).newInstance(search(topF, fp.fun, c))
            case _ => p
          }  
        } 
        else {
          val newTopF = Fun.replaceRef(topF, f, bestChoice)
          Type.check(newTopF, topF.inT)
          Context.updateContext(newTopF, topF.context)
          search(newTopF, bestChoice, c)    
        }
      }
      case _ => f
    } 
  
    Type.check(best, f.inT)
    Context.updateContext(best, f.context)
        
    best  
  }
  
  def search(f: Fun, c: Constraints = new Constraints(3, false)) : Fun = {
      
    assert (f.inT    != UndefType)
    assert (f.context != null)

    search(f,f,c)   
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