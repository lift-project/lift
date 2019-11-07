package backends.spatial.accel

import _root_.ir.ast._
import _root_.ir._
import backends.spatial.accel.ir.pattern.{AbstractSpFold, SpForeach}
import backends.spatial.common.ir.{ContextualMemoryCollection, RegMemory, ScalarMemory, SpatialMemory, SpatialMemoryCollection}

object ShouldUnroll {
  def apply(lambda: Lambda, allTypedMemories: ContextualMemoryCollection): Unit =
    new ShouldUnroll(lambda, allTypedMemories)
}

class ShouldUnroll(lambda: Lambda, allTypedMemories: ContextualMemoryCollection) {

  Expr.visit(lambda.body, _ => Unit, {
    case call: FunCall =>

      call.f match {
        case sf: SpForeach => if (shouldUnrollLoop(call)) sf.shouldUnroll = true
        case asf: AbstractSpFold =>
          // Does the fold read from register or literal memory?
          if (SpatialMemory.containsScalarMemory(call.args(1).mem))
            asf.shouldUnroll = true
        // TODO: confirm whether any other loops need checking
        case _ =>
      }

    case _ =>
  })

  private def existsInScalarMemories(mem: Memory): Boolean =
    allTypedMemories.intermediates(RegMemory).exists(_.mem.isInstanceOf[ScalarMemory])

  private def shouldUnrollLoop(call: FunCall): Boolean = {
    // Does the loop read from register or literal memory?
    ((SpatialMemory.containsScalarMemory(call.args.head.mem))
      && (call.args.head.mem match {
      case coll: SpatialMemoryCollection =>
        // TODO: this might be simplifiable
        coll.subMemories.exists(mem => existsInScalarMemories(mem))
      case _ => existsInScalarMemories(call.args.head.mem)
    })) ||
      // Does the loop write to register or literal memory?
      SpatialMemory.containsScalarMemory(call.mem)
  }

}
