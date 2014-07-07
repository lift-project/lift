package exploration

import opencl.generator.OpenCLGenerator
import generator.Dispatcher

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

        Type.check(rndFun, f.inT)
        Context.updateContext(rndFun, f.context)

        // generate code for the function
        println("OpenCL kernels generation for "+rndFun)
        Dispatcher.execute(rndFun, Array.fill(16384)(1.0f))

        val perf = Random.nextDouble()
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
  
  private def chooseMedianFastest(topF: Fun, oriF: Fun, choices: Seq[Fun], c: Constraints, depth:Int) : Fun = {

    assert(choices.length > 0)

    // //TODO: remove this!
    //return choices(Random.nextInt(choices.length))


    if (choices.length == 1)
      return choices(0)
    

    	
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
            case _:UngenerableException =>  // we cannot generate code for this choice, ignore it
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
    if (medians.isEmpty) {
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

  private def choose(topF: Fun, p: Pattern, choices: Seq[Fun], c:Constraints, depth:Int) : Fun = {

    assert (choices.length > 0)

    val choice =
      if (c.randomOnly)
        choices(Random.nextInt(choices.length))
      else
        chooseMedianFastest(topF, p, choices, c, depth)

    assert (choices.contains(choice))

    Type.check(choice, p.inT)
    Context.updateContext(choice, p.context)

    choice
  }

  private def derive(topF: Fun, f: Fun, c:Constraints, depth:Int) : Fun = {
    
	  assert (f.inT    != UndefType)
    assert (topF.inT != UndefType)
    assert (f.context != null)
    assert (topF.context != null)    

    // TODO: remove this assertion as it is costly
    //assert (isInside(topF, f))

     if (verbose)
        println("derive topF="+topF+" f="+f+" "+c.converge+" "+c.randomOnly+" "+c.fixedFuns)
    
    if (!c.canDerive(f))
      return f
    
    val bestChoice = f match {
      
      case p: Pattern => {        
      
        var choices = Rules.outerDerivations(p, c)
        if (p.isGenerable())
          choices = choices :+ p
         
        if (choices.length == 0)
          throw new UngenerableException("topF= "+topF+" f = "+f)

        val choice = choose(topF, p, choices, c, depth)

        Type.check(choice, f.inT)
        Context.updateContext(choice, f.context)

        if (choice != f) {
          // try to derive the newly found best
          val newTopF = Fun.replaceRef(topF, f, choice)
          Type.check(newTopF, topF.inT)
          Context.updateContext(newTopF, topF.context)

          derive(newTopF, choice, c, depth+1)
        } else
          f
      }

      case _ => f
    } 

    val newTopF = Fun.replaceRef(topF, f, bestChoice)
    Type.check(newTopF, topF.inT)
    Context.updateContext(newTopF, topF.context)

    // now try to go inside
    assert(bestChoice.isGenerable())
    bestChoice match {
      case fp: FPattern => fp.getClass.getConstructor(classOf[Fun]).newInstance(derive(newTopF, fp.f, c, depth + 1))
      case cf: CompFun => {
        val newFuns = cf.funs.map(inF => derive(newTopF, inF, c, depth+1)) // TODO: starts from the right! (not truely independent if the right most function changes its number of outputs)
        if (newFuns.length == 1)
          derive(newTopF, newFuns(0), c, depth + 1)
        else
          CompFun(newFuns: _*)
      }
      case _ => bestChoice
    }

  }
  
  def search(f: Fun, c: Constraints = new Constraints(3, false), depth:Int=0) : Fun = derive(f,f,c,depth)
  
}