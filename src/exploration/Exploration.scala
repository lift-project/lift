package exploration

import scala.util.Random
import ir.CompFun
import ir.Context
import ir.FPattern
import ir.Fun
import ir.Pattern
import ir.Type
import ir.UndefType

case class UngenerableException(msg: String) extends Exception(msg) {
  def this(f: Fun) = this("impossible to generate "+f)
}

object Exploration {
  
  val verbose = false

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
  
  private def choose(topF: Fun, oriF: Fun, choices: Seq[Fun], c: Constraints, depth:Int) : Fun = {    

    assert(choices.length > 0)

    // //TODO: remove this!
    //return choices(Random.nextInt(choices.length))


    if (choices.length == 1)
      return choices(0)
    
    if (c.randomOnly)
    	return choices(Random.nextInt(choices.length))  
    	
    if (verbose) {
      println("######################################")
      println("Choosing options for "+oriF+" (topF="+topF+", options="+choices+")")
    }    	
    
    val rndTerFixed : Constraints = new Constraints(c.maxMapDepth, true, true)
    rndTerFixed.addFixedFun(oriF)
    
    val perfMap = scala.collection.mutable.Map[Fun, List[Double]]()
    val seen = scala.collection.mutable.Set[Fun]()
    
    // generate a few random top level function with the oriF in place
    for (i <- 0 to 3) {
      //println("---------------- "+i)

      val rndFun = search(topF, rndTerFixed, depth + 1)
      
      if (!seen.contains(rndFun)) {
        seen += rndFun
        
        choices.map(choice => {          
          val f = Fun.replaceRef(rndFun, oriF, choice)
          Type.check(f, topF.inT)
          Context.updateContext(f, topF.context)
          try{
            val perf = evalPerf(f, c)
            perfMap.update(choice, perfMap.getOrElse(choice, List[Double]()) :+ perf)
          } catch {
            case _:UngenerableException => {
              // we cannot generate code for this choice, ignore it
              Double.MaxValue
            }
          }
        })
      }        
    }

    assert (perfMap.nonEmpty)

    val medians = perfMap.map({case (key, vals) => {
      val sortedVals = vals.sorted
      val median = sortedVals(sortedVals.length/2)
      (key,median)
    }})
    
    val bestMedian = medians.reduce((x,y) => if (x._2 < y._2) x else y)._1
    if (bestMedian == Double.MaxValue) {
      // in case none of the options lead to generable code
      throw new UngenerableException("choose: "+topF)
    }

    bestMedian
  }
    
  // returns true if function f appears inside topF (using .eq for comparison)
  private def isInside(topF: Fun, f:Fun) = {
    Fun.visit(false)(topF, (inF,result) => {
      result || f.eq(inF)
    })      
  }

  private def searchPattern(topF: Fun, p: Pattern, choices: Seq[Fun], c:Constraints, depth:Int) : Fun = {

    assert (choices.length > 0)

    val bestChoice = choose(topF, p, choices, c, depth)
    Type.check(bestChoice, p.inT)
    Context.updateContext(bestChoice, p.context)

    val newTopF = Fun.replaceRef(topF, p, bestChoice)
    Type.check(newTopF, topF.inT)
    Context.updateContext(newTopF, topF.context)

    try {
      if (bestChoice == p) {
        assert(p.isGenerable)
        p match {
          // ready to go inside pattern p (if possible)
          case fp: FPattern => fp.getClass().getConstructor(classOf[Fun]).newInstance(search(newTopF, fp.f, c, depth + 1))
          case _ => p // ok we are done with pattern p
        }
      }
      else {
        // pattern p is not generable, this means we need to transform it further
        search(newTopF, bestChoice, c, depth + 1)
      }
    } catch  {
      case ue: UngenerableException => {
        if (choices.length == 1) {
          //throw ue // no other choice available, bail out
          throw ue
        }
        // redo the search by removing the choice leading to ungenerable code

        println("caught  UngenerableException "+depth+" "+p+" "+choices)
        searchPattern(topF, p, choices.filterNot(c => c.eq(bestChoice)), c, depth)
      }
    }
  }

  private def search(topF: Fun, f: Fun, c:Constraints, depth:Int) : Fun = {
    
	  assert (f.inT    != UndefType)
    assert (topF.inT != UndefType)
    assert (f.context != null)
    assert (topF.context != null)    

    // TODO: remove this assertion as it is costly
    assert (isInside(topF, f))

     if (verbose)
        println("search topF="+topF+" f="+f+" "+c.converge+" "+c.randomOnly+" "+c.fixedFuns)
    
    if (!c.canDerive(f))
      return f
    
    val best = f match {
	  
      case cf: CompFun => {
        val newFuns = {
          cf.funs.map(inF =>
          search(topF, inF, c, depth+1))
        }
        if (newFuns.length == 1)
          return newFuns(0)
        else
          CompFun(newFuns: _*)
      }
      
      case p: Pattern => {        
      
        var choices = Rules.outerDerivations(p, c)
        if (p.isGenerable())
          choices = choices :+ p
         
        if (choices.length == 0)
          throw new UngenerableException("topF= "+topF+" f = "+f)

        searchPattern(topF, p, choices, c, depth)

      }
      case _ => f
    } 
  
    Type.check(best, f.inT)
    Context.updateContext(best, f.context)
        
    best  
  }
  
  def search(f: Fun, c: Constraints = new Constraints(3, false), depth:Int=0) : Fun = search(f,f,c,depth)  
  
}