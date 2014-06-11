package generators

import ir._

object Main extends App {
  //val id = UserFunc("int id(int x) { return xy; }")
  val id = NullFun
  val sumUp = UserFun("sumUp", "int sumUp(int x, int y) { return x+y; }")
  
  val highLevel = Reduce(sumUp, Cst(0))
  
  val lowLevel = CompFun(oJoin(), oJoin(),
		  					MapWrg(MapLcl(ReduceSeq(sumUp, Cst(0)))),
		  				 oSplit(Cst(128)), oSplit(Cst(2048)))
  
  //val varN = Var("N")
  val varN = Cst(1048576)
  val inputType: Type = ArrayType(ScalarType(/*"int"*/), varN)
	   
  println("HighLevel expr: " + highLevel)
  println("--------------------")
  println("LowLevel expr: " + lowLevel)
  println("--------------------")

  Type.check(lowLevel, inputType)
  val code = OpenCLGenerator.generateKernel(lowLevel)
  println("Code:")
  println("--------------------")
  println(code)
  println("--------------------")
  
  /*
  val x = Variable("x", ArrayType(ScalarType("int"), Var("N")))
  val y = Variable("y", ArrayType(ScalarType("int"), Var("N")))
  val alpha = Variable("alpha", ScalarType("int"))
  val beta = Variable("alpha", ScalarType("int"))
  
  val timesAlpha = UserFunc("int timesAlpha(int alpha, int x) { return alpha * x; }")
  val add = UserFunc("int add(int x, int y) { return x+y; }")
  val abs = UserFunc("int abs(int x) { x > 0 ? x : -x; }")
  val mult = UserFunc("int mult(int x, int y) { return x* y; }")
  
  val scal1 = (alpha, x) => { Map(timesAlpha(alpha), x) }
  val scal2 = (alpha, x) => { Map(timesAlpha(alpha)) o x }
  
  val asum1 = (x) => { Reduce(add, 0) o Map(abs, x) }
  val asum2 = (x) => { Reduce(add, 0) o Map(abs) o x }
  
  val dot1 = (x, y) => { Reduce(add, 0) o Map(mult) o Zip(x, y) }
  val dot2 = (x, y) => { Reduce(add, 0) o Map(mult) o Zip ??? }
  
  val gemv1 = (A, x, y, alpha, beta) => { Map(add) o Zip(Map(scal(alpha) o dot(x), A),
		  				       				 		    scal(beta, y)) }
  
  val gemv2 = (A, x, y, alpha, beta) => { Map(add) o Zip(Map(scal(alpha) o dot(x)) o A,
		  				       				 		    scal(beta, y)) }
  */
  
  println("end")
}
