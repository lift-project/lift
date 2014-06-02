package test

object Rules {
	
  private def derivsWithOneRule(fp: FPattern, c: Constraints): Seq[Fun] = {
    outerDerivations(fp,c) ++ Rules.innerDerivations(fp,c)
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
    	case _ => List()
    }       
  }
  
   def innerDerivations(fpat: FPattern, c: Constraints): Seq[Fun] = {
     derivsWithOneRule(fpat.fun, c).map((f) =>
        fpat.getClass().getConstructor(classOf[Fun]).newInstance(f))//.setContext(fpat.context))
        
    /*fpat.fun match {
      case _ : Fun => derivsWithOneRule(fpat.fun, c).map((f) =>
        fpat.getClass().getConstructor(classOf[Fun]).newInstance(f))//.setContext(fpat.context))
      case _ => List()
    }   */ 
  }

  /*
   * The context of the functions within the returned list might be invalid
   */
  def outerDerivations(f: Fun, c: Constraints): Seq[Fun] = {
            
    f match {

      case Map(inF) => {
        var result = List[Fun]()
        
        // sequential
        if (f.context.inMapGlb || f.context.inMapLcl)
        	result = result :+ MapSeq(inF)//.setContext(f.context)
        	
        // global, workgroup
        if (f.context.mapDepth == 0 && !f.context.inMapGlb && !f.context.inMapWrg) {
          result = result :+ MapGlb(inF)//.updateContext(f.context)
          result = result :+ MapWrg(inF)//.updateContext(f.context)
        }
        
        // local
        if (f.context.mapDepth == 1 && f.context.inMapWrg && !f.context.inMapGlb && !f.context.inMapLcl) {
          result = result :+ MapLcl(inF)//.updateContext(f.context)    
        }
        
        // split-join
        if (f.context.mapDepth < c.maxMapDepth && !c.onlyTerminal)
          result = result :+ new CompFun(oJoin(), Map(Map(inF)), oSplit())//.updateContext(f.context)
        
        result
      }     
      
      case Reduce(inF) => {
        var result = List[Fun]()
        if (f.context.mapDepth < c.maxMapDepth && !c.onlyTerminal)
        	result = result :+ new CompFun(Reduce(inF), oJoin(), Map(PartRed(inF)), oSplit())//.updateContext(f.context)
        result = result :+ ReduceSeq(inF)//.setContext(f.context)
        result
      }
      
      case PartRed(inF) => List(Reduce(inF)) // TODO
      
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