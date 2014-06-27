package ir

import scala.util.Random
import exploration.Exploration

object Main extends App {
	   
	   //val root = new CompFun(new Reduce(null), Map(Map(null)))
	   //val root = new CompFun(new Reduce(NullFun), Map(Map(NullFun))).updateContext(new Context())
	   
	   //val root = new Map(Map(NullFun)).updateContext(new Context())

	   //val root = new CompFun(ReduceSeq(NullFun), Join(), MapWrg(MapLcl(Map(NullFun))), Split()).updateContext(new Context())
	   
	   //val c = new Constraints()
	   //c.onlyTerminal = true;

	   //val userFun = UserFun("func", "float func(float x, float y) { return x+y; })")
	     
  	   val root = Reduce(NullFun)
  	   //val varN = Var("N")
	   val inputType: Type = ArrayType(ScalarType("int", Cst(4)),Cst(0))
  
  	   //val root =  MapWrg(new CompFun(Map(NullFun),Map(NullFun)))
	   //val inputType: Type = new ArrayType(new ArrayType(new ScalarType, Cst(20)),Cst(10))

	   	   	   
	   //println(Fun.maxDepth(root))	   
	   
	   val r = Reduce(NullFun)
	   val rr = r o PartRed(NullFun)
  	   println(rr)
  	   println(Fun.replaceRef(rr, r, Reduce(NullFun) o PartRed(NullFun)))
  	   	
	   
	   println(root)
	   println("--------------------")
	   
	   //val root2 = new CompFun(oJoin(), oJoin())
	   //println(Fun.replaceRef(root2, root2.funs(1), asScalar()))


	   
	   //println("root == Reduce(NullFun)  : "+(root == Reduce(NullFun)))
	   //println("root.eq(Reduce(NullFun)) : "+(root.eq(Reduce(NullFun))))
	   //println("root.eq(root) : " + (root.eq(root)))
	   ///println("Reduce(NullFun).eq(Reduce(NullFun)) : " + (Reduce(NullFun).eq(Reduce(NullFun))))
	   
//	   val nu = NullFun
//	   val cf1 = Reduce(nu)
//	   val cf2 = Reduce(nu) 
//	   val cf3 = cf2
//	   var s = Set[Fun]()
//	   s = s + cf1
//	   s = s + cf2
//	   println(cf1.eq(cf2))
//  	   println(cf1 == cf2)
//  	   
//  	   println(cf3.eq(cf2))
//  	   println(cf3 == cf2)
  	   
	   
	   
	   Type.check(root, inputType)
	   Context.updateContext(root, new Context())
	   println(Exploration.search(root))

	    //val results = Exploration.bfs(root, inputType, 0)
	    //results.map(r => println(r))
	   
	   for (i <- 101 to 100) {
	     println("----------------------------")
	     val f = Utils.randomDescent(root, inputType, 10)
	     //println(f)

	     //Type.check(e, inputType)
	     //println(Printer.toStringWithType(f))
	     val vars = Var.getVars(f)
         val substs = Var.setVarsAtRandom(vars)
         println(substs)

         //println(f)
         val newF = Fun.visitExpr(f, e => Expr.substitute(e, substs.toMap))
         println(newF)	           
         
         Type.check(newF, inputType)
         //println(Printer.toStringWithType(newF))
                  
         //println(Var.substitute(e, substs))

	   }
	   
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
