package backends.spatial.accel

import _root_.ir.ast._
import _root_.ir._
import backends.spatial.accel.ir.pattern.{AbstractSpFold, SpForeach}
import backends.spatial.common.ir.{LiteralMemory, RegMemory, SpatialMemory, SpatialMemoryCollection, ContextualMemoryCollection}

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
          if (SpatialMemory.containsPrivateMemory(call.args(1).mem))
            asf.shouldUnroll = true
        // TODO: confirm whether any other loops need checking
        case _ =>
      }

    case _ =>
  })

  private def existsInPrivateMemories(mem: Memory): Boolean =
    allTypedMemories.intermediates(RegMemory).exists(_.mem == mem) ||
      allTypedMemories.intermediates(LiteralMemory).exists(_.mem == mem)

  private def shouldUnrollLoop(call: FunCall): Boolean = {
    // Does the loop read from register or literal memory?
    (SpatialMemory.containsPrivateMemory(call.args.head.mem)
      && (call.args.head.mem match {
      case coll: SpatialMemoryCollection =>
        // TODO: this might be simplifiable
        coll.subMemories.exists(mem => existsInPrivateMemories(mem))
      case _ => existsInPrivateMemories(call.args.head.mem)
    })) ||
      // Does the loop write to register or literal memory?
      SpatialMemory.containsPrivateMemory(call.mem)
  }

}
