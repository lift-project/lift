package ir

abstract class Memory {  
  def variable : Var
  def size : Expr
  def accessFunctions : Array[(Expr) => Expr]
}

object NullMemory extends Memory {
  val variable = Var("NULL")
  val size = Cst(0)
  val accessFunctions = Array.empty[(Expr) => Expr]
}
