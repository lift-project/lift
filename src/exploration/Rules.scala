package exploration

import scala.collection.Seq
import ir._
import opencl.ir._

/*class Derivations(val f: Fun, val c: Constraints) {
  
  private def vfn(f: Fun, s: Seq[Seq[Fun]]): Seq[Seq[Fun]] = {
      val outDerivs = Rules.outerDerivations(f, c)
      s :+ outDerivs
    }
  private val derivs = Fun.visit(Seq(Seq[Fun]()))(f, vfn)
    
  def getAll() : Seq[Fun] {
    listPossiblities[T](oriList : Seq[T], optionsList : Seq[Seq[T]]) : Seq[Seq[T]]
  }
  
  def getRandom() : Fun {
    
  }
  
}*/

object Rules {
   
  
  /*private def derivsWithOneRule(fp: FPattern, c: Constraints): Seq[Fun] = {
    outerDerivations(fp,c) ++ Rules.innerDerivations(fp,c)
  }*/
  
  private def composedDerivations(cf: CompFun, c: Constraints, level: Int): Seq[Fun] = {

    val optionsList = cf.funs.map(f => derivsWithOneRule(f,c, level))
    Utils.listPossiblities(cf.funs, optionsList).map(funs => new CompFun(funs: _*))
    
    /*val pairs = cf.getFuns().zip(cf.getFuns.tail)
      val pairDerivs = pairs.map({case (x, y) => pairDeriv(x,y)})      
      val numPairDerivs = pairDerivs.foldLeft(0)((num,l) => num+l.length)*/
  }
   
   private def innerDerivations(fpat: FPattern, c: Constraints, level: Int): Seq[Fun] = {
     derivsWithOneRule(fpat.f, c,level).map((f) => fpat.getClass().getConstructor(classOf[Fun]).newInstance(f))        
  }
    
    
  /*
   * Return a list of all possible derivations using only one rule at a given level
   */
  def derivsWithOneRule(f: Fun, c: Constraints, level: Int): Seq[Fun] =  {
      f match {
        case cf: CompFun => composedDerivations(cf, c, level)
        case fp: FPattern if (level==0) => outerDerivations(fp,c)
        case fp: FPattern if (level>0) => innerDerivations(fp,c,level-1)
        case p: Pattern if (level==0)  => outerDerivations(p, c)
        case _ => List()
      }     
  } 


  private def validOSplitRange(t: Type) = {
    t match {
      case ArrayType(_, len) => RangeMul(Cst(1), len, Cst(2))
      case _ => RangeUnkown // Error
    }
  }
 

  def outerDerivations(f: Fun, c: Constraints): Seq[Fun] = {   
    
    f match {

      case Map(inF) => {
        var result = List[Fun]()
        
        // sequential
        if (f.context.inMapGlb || f.context.inMapLcl)
        	result = result :+ MapSeq(inF)
        	
        // global, workgroup
        if (f.context.mapDepth == 0 && !f.context.inMapGlb && !f.context.inMapWrg) {
          result = result :+ MapGlb(inF)  
          result = result :+ MapWrg(inF)
        }
        
        // local
        if (f.context.mapDepth == 1 && f.context.inMapWrg && !f.context.inMapGlb && !f.context.inMapLcl) {
          result = result :+ MapLcl(inF) 
        }
        
        // split-join
        if (f.context.mapDepth+1 < c.maxMapDepth && !c.converge)          
          result = result :+ new CompFun(Join(), Map(Map(inF)), Split(Var(validOSplitRange(f.inT))))
        
        result
      }     
      
      case Reduce(inF) => {
        var result = List[Fun]()
        if (!c.converge)
        	result = result :+ (Reduce(inF) o PartRed(inF))
        	
        if (f.context.inMapGlb || f.context.inMapLcl)        	
        	result = result :+ ReduceSeq(inF)
        else if (!f.context.inMapGlb && !f.context.inMapLcl && !f.context.inMapWrg)  
          result = result :+ ReduceHost(inF)
        	
        result
      }
      
      case PartRed(inF) => {
        var result = List[Fun]()
        result = result :+ Reduce(inF)
        if (f.context.mapDepth < c.maxMapDepth && !c.converge)
          result = result :+ (Join() o Map(PartRed(inF)) o Split(Var(validOSplitRange(f.inT))))
        result
      }
      
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
  
}