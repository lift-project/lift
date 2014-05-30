package test

import scala.util.Random

object Main extends App {
	   
	   //val root = new CompFun(new Reduce(null), Map(Map(null),new Context()))
	   //val root = new CompFun(new Reduce(NullFun), Map(Map(NullFun))).updateContext(new Context())
	   val root = new Reduce(NullFun)
	   //val root =  MapWrg(new CompFun(Map(NullFun))).updateContext(new Context)
 
	   
	   //val root = new Map(Map(NullFun)).updateContext(new Context())

	   //val root = new CompFun(ReduceSeq(NullFun), oJoin(), MapWrg(MapLcl(Map(NullFun))), oSplit()).updateContext(new Context())
	   
	   //val c = new Constraints()
	   //c.onlyTerminal = true;

	   for (i <- 1 to 100)
		   println(Pattern.randomDescent(root, 10))
	   
	   //println(Pattern.derivsWithOneRule(root))
	   //Pattern.explore(root, 20, 5)
	   
	   /*val root1 = new CompFun(new Map(null), new Reduce(null))
	   val root2 = new CompFun(new Map(null), new Reduce(null))
	   
	   //val root1 = new CompFun(Map(null))
	   //val root2 = new CompFun(Map(null))
	   
	   val s = List(root1, root2)
	   //println(root1.equals(root2))
	   //println(root1 == root2)
	   println(s.distinct)*/

}