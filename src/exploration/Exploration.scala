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
    
    f.params.map(p => assert(p.t != UndefType))
    
    var perfs = List[Double]()
    val seen = scala.collection.mutable.Set[Lambda]()
    for (i <- 0 to 0) {
      val rndFun = search(f, inputs, new Constraints(c.maxMapDepth, true, true))

      if (!seen.contains(rndFun)) {
        seen += rndFun

        Type.check(rndFun.body)
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
  
  private def chooseMedianFastest(topF: Lambda, oriF: FunCall, choices: Seq[FunCall], inputs: Seq[Any], c: Constraints, depth:Int) : FunCall = {

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
    rndTerFixed.addFixedFunCall(oriF)
    
    val perfMap = scala.collection.mutable.Map[FunCall, List[Double]]()
    val seen = scala.collection.mutable.Set[Lambda]()
    
    // generate a few random top level function with the oriF in place
    for (i <- 0 to 3) {
      //println("---------------- "+i)

      val rndFun = search(topF, inputs, rndTerFixed, depth + 1)
      
      if (!seen.contains(rndFun)) {
        seen += rndFun

        choices.map(choice => {          
          val f = FunDecl.replace(rndFun, oriF, choice)
          val funCall = topF.body match { case call: FunCall => call }
          f.params(0).t = funCall.argsType
          Type.check(f.body)
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

  private def choose(topF: Lambda, f: FunCall, choices: Seq[FunCall], inputs: Seq[Any], c:Constraints, depth:Int) : FunCall = {

    assert (choices.length > 0)

    val choice : FunCall =
      if (c.randomOnly)
        choices(Random.nextInt(choices.length))
      else
        chooseMedianFastest(topF, f, choices, inputs, c, depth)

    assert (choices.contains(choice))

    Type.check(choice)
    Context.updateContext(choice, f.context)

    choice
  }

  private def derive(topF: Lambda, e:Expr, inputs: Seq[Any], c:Constraints, depth:Int) : Expr = {
   e match {
      case call : FunCall => deriveFunCall(topF, call, inputs, c, depth)
      case _ => e
    }
  }

  private def deriveFunCall(topF: Lambda, call: FunCall, inputs: Seq[Any], c:Constraints, depth:Int) : Expr = {

    assert (call.context != null)
    assert (topF.body.context != null)

    // TODO: remove this assertion as it is costly
    //assert (isInside(topF, f))

     if (verbose)
        println("derive topF="+topF+" call="+call+" "+c.converge+" "+c.randomOnly+" "+c.fixedFuns)
    
    if (!c.canDerive(call))
      return call

    
    val bestChoice = call.f match {
      
      case p: Pattern =>
      
        var choices = Rules.derivePatFunCall(call, c)
        if (p.isGenerable)
          choices = choices :+ call
         
        if (choices.length == 0)
          throw new UngenerableException("topF= "+topF+" call = "+call)

        val choice = choose(topF, call, choices, inputs, c, depth)

        Type.check(choice)
        Context.updateContext(choice, call.context)

        if (choice != call) {
          // try to derive the newly found best
          val newTopF = FunDecl.replace(topF, call, choice)
          val funCall = topF.body match { case call: FunCall => call }
          newTopF.params(0).t = funCall.argsType
          Type.check(newTopF.body)
          Context.updateContext(newTopF.body, topF.body.context)

          deriveFunCall(newTopF, choice, inputs, c, depth+1)
        } else
          call


      case _ => call
    } 

    val newTopF = FunDecl.replace(topF, call, bestChoice)
    val funCall = topF.body match { case call: FunCall => call }
    newTopF.params(0).t = funCall.argsType
    Type.check(newTopF.body)
    Context.updateContext(newTopF.body, topF.body.context)

    bestChoice match {
      case call: FunCall =>

        assert(call.f.isGenerable)

        // now try to go inside
        call.f match {
          case fp: FPattern => deriveFunCall(newTopF, call, inputs, c, depth + 1)
          case cf: CompFunDef =>
            val newFuns = cf.funs.map(inF => derive(newTopF, inF.body, inputs, c, depth + 1)) // TODO: starts from the right! (not truely independent if the right most function changes its number of outputs)
            if (newFuns.length == 1)
              derive(newTopF, newFuns(0), inputs, c, depth + 1)
            else {
              val newLambdas = newFuns.zip(cf.funs).map({ case (e,l) => new Lambda(l.params,e) })
              (new CompFunDef(cf.params, newLambdas: _*))(call.args: _*)
            }


          case _ => bestChoice
        }


      case _ => bestChoice
    }


  }
  
  def search(f: Lambda, inputs: Seq[Any], c: Constraints = new Constraints(3, false), depth:Int=0) : Lambda =
    new Lambda(f.params,derive(f,f.body,inputs,c,depth))

}