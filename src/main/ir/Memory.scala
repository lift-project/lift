package ir

import arithmetic.{ArithExpr, Cst, Var}

abstract class Memory {  
  def variable : Var
  def size : ArithExpr

  var readOnly = false
}

object UnallocatedMemory extends Memory {
  val variable = Var("NULL")
  val size = Cst(0)
}