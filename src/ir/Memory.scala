package ir

abstract class Memory {
  def variable : Var
  def size : Cst
}

object NullMemory extends Memory {
  val variable = Var("NULL")
  val size = Cst(0)
}
