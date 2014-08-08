package exploration

import generator.Dispatcher

import scala.util.Random
import ir._

case class UngenerableException(msg: String) extends Exception(msg) {
  def this(f: FunCall) = this("impossible to generate "+f)
}

object Exploration {
  
  val verbose = true

  private def median(l: List[Double]) : Double = {
    val s = l.sorted
    s(l.length/2)
  }

  private def evalPerf(f: Lambda, inputs: Seq[Any], c: Constraints) : Double = {

    if (verbose) {
      println("------------------------------------------------------")
      println("Evaluating performance of "+f)
    }
    
    f.params.map(p => assert(p.outT != UndefType))
    
    var perfs = List[Double]()
    val seen = scala.collection.mutable.Set[Lambda]()
    for (i <- 0 to 0) {
      val rndFun = search(f, inputs, new Constraints(c.maxMapDepth, true, true))

      if (!seen.contains(rndFun)) {
        seen += rndFun

        Type.check(rndFun.body, NoType)
        Context.updateContext(rndFun.body, f.body.context)

        // generate code for the function
        println("Running "+rndFun)

        var runs = List[Double]()
        for (run <- 1 to 10) {
          val result = Dispatcher.execute(rndFun, inputs)
          val perf = result._1
          runs = runs :+ perf
          println(perf)
        }
        val medRun = Exploration.median(runs)
        perfs = perfs :+ medRun
        println("median = " + medRun)

        //val perf = Random.nextDouble()
        //perfs = perfs :+ perf
      }
    }
    
    if (perfs.isEmpty)
      return Double.MaxValue

    Exploration.median(perfs)
    
    //Random.nextDouble
  }
  
  private def chooseMedianFastest(topF: Lambda, oriF: Lambda, choices: Seq[Lambda], inputs: Seq[Any], c: Constraints, depth:Int) : Lambda = {

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
    
    val perfMap = scala.collection.mutable.Map[Lambda, List[Double]]()
    val seen = scala.collection.mutable.Set[Lambda]()
    
    // generate a few random top level function with the oriF in place
    for (i <- 0 to 3) {
      //println("---------------- "+i)

      val rndFun = search(topF, inputs, rndTerFixed, depth + 1)
      
      if (!seen.contains(rndFun)) {
        seen += rndFun

        choices.map(choice => {          
          val f = FunDecl.replace(rndFun, oriF, choice)
          Type.check(f.body, topF.body.inT)
          Context.updateContext(f.body, topF.body.context)
          try{
            val perf = evalPerf(f, inputs, c)
            perfMap.update(choice, perfMap.getOrElse(choice, List[Double]()) :+ perf)
          } catch {
            case _:UngenerableException =>  // we cannot generate code for this choice, ignore it
          }
        })
      }        
    }

    assert (perfMap.nonEmpty)

    val medians = perfMap.map({case (key, vals) =>
      val sortedVals = vals.sorted
      val median = sortedVals(sortedVals.length/2)
      (key,median)
    })
    
    val bestMedian = medians.reduce((x,y) => if (x._2 < y._2) x else y)._1
    if (medians.isEmpty) {
      // in case none of the options lead to generable code
      throw new UngenerableException("choose: "+topF)
    }

    bestMedian
  }
    
  /*// returns true if function f appears inside topF (using .eq for comparison)
  private def isInside(topF: FunExpr, f:FunExpr) = {
    FunExpr.visit(false)(topF, (inF,result) => {
      result || f.eq(inF)
    })      
  }*/

  private def choose(topF: Lambda, p: Lambda, choices: Seq[Lambda], inputs: Seq[Any], c:Constraints, depth:Int) : Lambda = {

    assert (choices.length > 0)

    val choice =
      if (c.randomOnly)
        choices(Random.nextInt(choices.length))
      else
        chooseMedianFastest(topF, p, choices, inputs, c, depth)

    assert (choices.contains(choice))

    Type.check(choice.body, p.body.inT)
    Context.updateContext(choice.body, p.body.context)

    choice
  }

  private def derive(topF: Lambda, f: Lambda, inputs: Seq[Any], c:Constraints, depth:Int) : Lambda = {
    
	  //assert (f.inT    != UndefType)
    //assert (topF.inT != UndefType)
    assert (f.body.context != null)
    assert (topF.body.context != null)

    // TODO: remove this assertion as it is costly
    //assert (isInside(topF, f))

     if (verbose)
        println("derive topF="+topF+" f="+f+" "+c.converge+" "+c.randomOnly+" "+c.fixedFuns)
    
    if (!c.canDerive(f))
      return f

    val call = f.body match {
      case  call: FunCall => call
    }
    
    val bestChoice = call.f match {
      
      case p: Pattern =>
      
        var choices = Rules.outerDerivations(p, c)
        if (p.isGenerable)
          choices = choices :+ f
         
        if (choices.length == 0)
          throw new UngenerableException("topF= "+topF+" f = "+f)

        val choice = choose(topF, p, choices, inputs, c, depth)

        Type.check(choice.body, f.body.inT)
        Context.updateContext(choice.body, f.body.context)

        if (choice != f) {
          // try to derive the newly found best
          val newTopF = FunDecl.replace(topF, f, choice)
          Type.check(newTopF.body, topF.body.inT)
          Context.updateContext(newTopF.body, topF.body.context)

          derive(newTopF, choice, inputs, c, depth+1)
        } else
          f


      case _ => f
    } 

    val newTopF = FunDecl.replace(topF, f, bestChoice)
    Type.check(newTopF.body, topF.body.inT)
    Context.updateContext(newTopF.body, topF.body.context)

    // now try to go inside
    assert(bestChoice.isGenerable)

    val bestChoiceCall = bestChoice.body match {
      case  call: FunCall => call
    }

    bestChoiceCall.f match {
      case fp: FPattern => fp.getClass.getConstructor(classOf[FunCall]).newInstance(derive(newTopF, fp.f, inputs, c, depth + 1))
      case cf: CompFunDef =>
        val newFuns = cf.funs.map(inF => derive(newTopF, inF, inputs, c, depth+1)) // TODO: starts from the right! (not truely independent if the right most function changes its number of outputs)
        if (newFuns.length == 1)
          derive(newTopF, newFuns(0), inputs, c, depth + 1)
        else
          CompFunDef(newFuns: _*)

      case _ => bestChoice
    }

  }
  
  def search(f: Lambda, inputs: Seq[Any], c: Constraints = new Constraints(3, false), depth:Int=0) : Lambda = derive(f,f,inputs,c,depth)

}