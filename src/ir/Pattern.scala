package ir

//import scala.collection.mutable.HashSet
//import scala.collection.mutable.LinkedList







  
  
  
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

  
