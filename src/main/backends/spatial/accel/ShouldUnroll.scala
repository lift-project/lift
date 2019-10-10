package backends.spatial.accel

import _root_.ir.ast._
import _root_.ir._
import backends.spatial.accel.ir.pattern.{AbstractSpFold, SpForeach}
import backends.spatial.common.ir.{RegMemory, SpatialMemory, SpatialMemoryCollection, TypedMemoryCollection}

object ShouldUnroll {
  def apply(lambda: Lambda, allTypedMemories: TypedMemoryCollection): Unit =
    new ShouldUnroll(lambda, allTypedMemories)

}

class ShouldUnroll(lambda: Lambda, allTypedMemories: TypedMemoryCollection) {

  Expr.visit(lambda.body, _ => Unit, {
    case call: FunCall =>

      call.f match {
        case sf: SpForeach => if (shouldUnrollLoop(call)) sf.shouldUnroll = true
        case asf: AbstractSpFold =>
          // Does the fold read from register memory?
          if (SpatialMemory.containsRegMemory(call.args(1).mem))
            asf.shouldUnroll = true
        // TODO: confirm whether any other loops need checking
        case _ =>
      }

    case _ =>
  })

  private def existsInRegMemories(mem: Memory): Boolean =
    allTypedMemories.intermediates(RegMemory).exists(_.mem == mem)

  private def shouldUnrollLoop(call: FunCall): Boolean = {
    // Does the loop read from register memory?
    (SpatialMemory.containsRegMemory(call.args.head.mem)
      && (call.args.head.mem match {
      case coll: SpatialMemoryCollection =>
        // TODO: this might be simplifiable
        coll.subMemories.exists(mem => existsInRegMemories(mem))
      case _ => existsInRegMemories(call.args.head.mem)
    })) ||
      // Does the loop write to register memory?
      SpatialMemory.asSpatialMemory(call.mem).addressSpace == RegMemory
  }

}
