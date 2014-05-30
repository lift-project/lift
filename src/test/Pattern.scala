package test

import test.Fun
import test.CompFun
import scala.util.Random
import scala.collection.mutable.HashSet
import scala.collection.mutable.LinkedList

abstract class Pattern() extends Fun()

abstract class FPattern(f: Fun) extends Pattern() {
  def fun = f
}
object FPattern {
  def unapply(fp: FPattern): Option[(Fun,Context)] = Some(fp.fun,fp.context)
}


abstract class AbstractMap(f:Fun) extends FPattern(f)
object AbstractMap {
	def unapply(am: AbstractMap): Option[Fun] = Some(am.fun)
}


case class Map(f:Fun) extends AbstractMap(f) 
case class MapSeq(f: Fun) extends AbstractMap(f)
case class MapGlb(f: Fun) extends AbstractMap(f)
case class MapWrg(f: Fun) extends AbstractMap(f)
case class MapLcl(f: Fun) extends AbstractMap(f)

abstract class AbstractReduce(f: Fun) extends FPattern(f) {
	def unapply(ar: AbstractReduce): Option[Fun] = Some(ar.fun)
}
case class Reduce(f: Fun) extends AbstractReduce(f)
case class ReduceSeq(f: Fun) extends AbstractReduce(f)

case class PartRed(f: Fun) extends FPattern(f)

case class oJoin() extends Pattern()
case class iJoin() extends Pattern()
case class oSplit() extends Pattern()
case class iSplit() extends Pattern()

object Pattern {
  
  def unapply(p: Pattern) : Option[Context] = Some(p.context)
    
  def randomDescent(f: Fun, maxDepth : Int, constraints: Constraints = new Constraints(3, false)) : Fun = {
    
    var c = constraints
    
    // setup the context
    if (f.context == null)
      f.updateContext(new Context())
    else
      f.updateContext()
    
    if (maxDepth < 0)
      c = c.setOnlyTerminal
    
    val derivs = Rules.derivsWithOneRule(f,c);
    if (derivs.isEmpty)
      return f;
    
    // select one derivation at random
    val rnd = Random.nextInt(derivs.length)
    //println(rnd+"/"+derivs.length)
    val randomDeriv = derivs(rnd)
    
    
    randomDescent(randomDeriv, maxDepth -1, c)
  }
  
  def evalPerf(f: Fun): Float = {
    0f
  }


  
  
  
  /*def oneRandomDeriv(f: Fun): Fun = {
    if (f.isInstanceOf[CompFun]) {
      val cf = f.asInstanceOf[CompFun];

      val simpleDerivs = cf.getFuns().map(f => outerDerivations(f) ::: {
        if (f.isInstanceOf[FPattern])
          innerDerivations(f.asInstanceOf[FPattern])
        else
          List()
      })
        
      val numSimpleDerivs = simpleDerivs.foldLeft(0)((num,l) => num+l.length)

      val pairs = cf.getFuns().zip(cf.getFuns.tail)
      val pairDerivs = pairs.map({case (x, y) => pairDeriv(x,y)})      
      val numPairDerivs = pairDerivs.foldLeft(0)((num,l) => num+l.length)

      val numDerivs = numSimpleDerivs + numPairDerivs
      if (numDerivs == 0)
        return f;

      val rnd = Random.nextInt(numSimpleDerivs/*numDerivs*/)
      println(rnd + "/" + numSimpleDerivs)
      if (rnd < numSimpleDerivs) {
        val funs = cf.getFuns.zip(simpleDerivs).foldLeft(0, List[Fun]())((state, cur) => {
          val numDerivs = cur._2.length
          val start = state._1;
          val stop = state._1 + numDerivs
          val prevList = state._2;
          if (numDerivs == 0)
            (start, prevList ::: List(cur._1))
          else {
            if (start <= rnd && rnd < stop)
              (stop, prevList ::: List(cur._2(rnd - start)))
            else
              (stop, prevList ::: List(cur._1))
          }
        })._2
        new CompFun(funs : _*)
      } else 
    	 f
    	  
    	  //rnd = Random.nextInt(numSimpleDerivs)
    	  //return new CompFun(cf.getFuns().updated(rnd, oneRandomDeriv(cf.getFuns()(rnd))) : _*)
    }
    else {
           
      
      //var z = pairs.zip(pairDerivs)
      
      //var derivs = new LinkedList[Fun]();
      //cf.getFuns.reduce((x,y) => pairDeriv(x,y))
      
    
    
    var outerDerivs = outerDerivations(f)
    
    if (f.isInstanceOf[FPattern]) {
      val fp = f.asInstanceOf[FPattern];            
      outerDerivs = outerDerivs++innerDerivations(fp);
    }
    
    if (outerDerivs.isEmpty)
      return f;
    	    	
    val rnd = Random.nextInt(outerDerivs.length)
    outerDerivs(rnd)
    }
  }
*/
 


  /*def explore(p: Pattern) {
    var derivs = derivations(p)
    if (p.isInstanceOf[FPattern]) {
      var fp: FPattern = p.asInstanceOf[FPattern]
      var innerDerivs = innerDerivations(fp)
      
    }

  }*/

 /* def explore(f: Fun, width: Int, maxDepth: Int) {
    var seenHashes = new HashSet[Int]();
    explore(f, width, maxDepth, seenHashes);
  }
  
  private def explore(f : Fun, width: Int, maxDepth: Int, seenHashes: HashSet[Int]) {

   if (maxDepth == 0)
     return;
   
   val l = (1 to width).map(_ => oneRandomDeriv(f))
   println(maxDepth + " -------------")
     
    var newFuns = new HashSet[Fun]();
    for (x <- l) {
      if (!seenHashes(x.hashCode())) {
        newFuns += x;        
        seenHashes += x.hashCode()
      }
    }
   
   newFuns.map((fun) => println(fun))
   newFuns.map((fun) => explore(fun, width, maxDepth-1, seenHashes))    
  }*/
}

  
