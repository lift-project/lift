package ir

import lift.arithmetic.{ArithExpr, Cst, Var}

abstract class Memory {  
  def variable : Var
  def size : ArithExpr

  var readOnly = false

  def equalsInShape(that: Memory): Boolean = {
    (this, that) match {
      case (thisColl: MemoryCollection, thatColl: MemoryCollection) =>
        thisColl.subMemories.zip(thatColl.subMemories).foldLeft(true) {
          case (equalSoFar, (thisSubMem, thatSubMem)) =>
            equalSoFar && thisSubMem.equalsInShape(thatSubMem)
        }
      case (_: Memory, _: Memory) => true
      case _ => false
    }
  }
}

object UnallocatedMemory extends Memory {
  val variable = Var("NULL")
  val size = Cst(0)
}

trait MemoryCollection extends Memory {
  val subMemories: Vector[Memory]
}