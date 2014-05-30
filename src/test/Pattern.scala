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
	def unapply(am: AbstractMap): Option[(Fun, Context)] = Some((am.fun, am.context))
}


case class Map(f:Fun) extends AbstractMap(f) 
case class MapSeq(f: Fun) extends AbstractMap(f)
case class MapGlb(f: Fun) extends AbstractMap(f)
case class MapWrg(f: Fun) extends AbstractMap(f)
case class MapLcl(f: Fun) extends AbstractMap(f)

case class Reduce(f: Fun) extends FPattern(f)
case class ReduceSeq(f: Fun) extends FPattern(f)
case class PartRed(f: Fun) extends FPattern(f)

case class oJoin() extends Pattern()
case class iJoin() extends Pattern()
case class oSplit() extends Pattern()
case class iSplit() extends Pattern()

object Pattern {
  
  def unapply(p: Pattern) : Option[Context] = Some(p.context)
    
  def randomDescent(f: Fun, maxDepth : Int, c: Constraints = new Constraints()) : Fun = {
        
    if (maxDepth < 0)
      c.onlyTerminal = true;
    
    val derivs = derivsWithOneRule(f,c);
    if (derivs.isEmpty)
      return f;
    val randomDeriv = derivs(Random.nextInt(derivs.length))
    randomDescent(randomDeriv, maxDepth -1, c)
  }
  
  def evalPerf(f: Fun): Float = {
    0f
  }

  private def derivsWithOneRule(fp: FPattern, c: Constraints): Seq[Fun] = {
    outerDerivations(fp,c) ::: innerDerivations(fp,c)
  }
  
  private def derivsWithOneRule(cf: CompFun, c: Constraints): Seq[Fun] = {

    val optionsList = cf.funs.map(f => derivsWithOneRule(f,c))
    Utils.listPossiblities(cf.funs, optionsList).map(funs => new CompFun(funs: _*))
    
    /*val pairs = cf.getFuns().zip(cf.getFuns.tail)
      val pairDerivs = pairs.map({case (x, y) => pairDeriv(x,y)})      
      val numPairDerivs = pairDerivs.foldLeft(0)((num,l) => num+l.length)*/
  }
   
    
    
  /*
   * Return a list of all possible derivations using only one rule
   */
  def derivsWithOneRule(f: Fun, c: Constraints): Seq[Fun] =  {
    f match {
    	case cf: CompFun => derivsWithOneRule(cf,c)
    	case fp: FPattern => derivsWithOneRule(fp,c)
    	case p: Pattern => outerDerivations(p,c)
    }       
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
  def innerDerivations(fpat: FPattern, c: Constraints): List[Fun] = {
    fpat.fun match {
      case _ : Pattern => outerDerivations(fpat.fun, c).map((f) =>
        fpat.getClass().getConstructor(classOf[Fun]).newInstance(f).setContext(fpat.context))
      case _ => List()
    }    
  }

  def outerDerivations(f: Fun, c: Constraints): List[Fun] = {
    
    //assert(f.context != null)
    
    f match {

      case Map(inF) => {
        var result = List[Fun]()
        if (f.context.inMapGlb || f.context.inMapLcl)
        	result = result :+ MapSeq(inF).setContext(f.context)
        if (f.context.mapDepth < c.maxMapDepth && !c.onlyTerminal)
          result = result :+ new CompFun(oJoin(), Map(Map(inF)), oSplit()).updateContext(f.context)
        if (!f.context.inMapGlb && !f.context.inMapWrg) {
          result = result :+ MapGlb(inF).updateContext(f.context)
          result = result :+ MapWrg(inF).updateContext(f.context)
        }
        if (f.context.inMapWrg && !f.context.inMapGlb && !f.context.inMapLcl) {
          result = result :+ MapLcl(inF).updateContext(f.context)    
        }
        result
      }     
      
      case Reduce(inF) => {
        var result = List[Fun]()
        if (f.context.mapDepth < c.maxMapDepth && !c.onlyTerminal)
        	result = result :+ new CompFun(Reduce(inF), oJoin(), Map(PartRed(inF)), oSplit()).updateContext(f.context)
        result = result :+ ReduceSeq(inF).setContext(f.context)
        result
      }
      
      case PartRed(_) => List() // TODO
      
      case _ => List() // all the terminals end up here
    }
  }

  /*
   * Simplification and fusion rules.
   */
  /*def pairDeriv(f1: Fun, f2: Fun): List[Fun] = {
    f1 match {

      case Map(f,ctx1) => {
        f2 match {
          case Map(g,ctx2) => List(Map(new CompFun(f, g),ctx1)) // TODO: merge context ctx1 and ctx2
          case _ => List()
        }
      }

      case oSplit(_) => {
        f2 match {
          case oJoin(_) => List(new NullFun())
          case _ => List()
        }
      }

      case iSplit(_) => {
        f2 match {
          case iJoin(_) => List(new NullFun())
          case _ => List()
        }
      }

      case _ => List()      
    }
  }*/

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

  
