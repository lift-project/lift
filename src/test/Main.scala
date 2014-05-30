package test

object Main extends App {
	   
	   //val root = new CompFun(new Reduce(null), Map(Map(null),new Context()))
	   val root = new CompFun(new Reduce(null), Map(Map(null)).updateContext(new Context()))
	       

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