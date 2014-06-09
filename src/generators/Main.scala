package generators

import test._
import test.CompFun

object Main extends App {
  //val id = UserFunc("int id(int x) { return xy; }")
  val id = NullFun
  //val sumUp = UserFunc("int sumUp(int x, int y) { return x+y; }")
  val sumUp = NullFun
  
  val highLevel = Reduce(sumUp)
  
  val lowLevel = CompFun(oJoin(), oJoin(),
		  					MapWrg(MapLcl(ReduceSeq(sumUp))),
		  				 oSplit(Cst(128)), oSplit(Cst(2048)))
  
  //val varN = Var("N")
  val varN = Cst(1048576)
  val inputType: Type = ArrayType(ScalarType(/*"int"*/), varN)
	   
  println("HighLevel expr: " + highLevel)
  println("--------------------")
  println("LowLevel expr: " + lowLevel)
  println("--------------------")

  Type.check(lowLevel, inputType)
  val code = OpenCLGenerator.generate(lowLevel)
  println("Code:")
  println("--------------------")
  println(code)
  println("--------------------")
  
  println("end")
}