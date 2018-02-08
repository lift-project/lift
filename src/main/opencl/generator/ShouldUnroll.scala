package opencl.generator

import ir._
import ir.ast.{Expr, FunCall, Lambda}
import opencl.ir.pattern._
import opencl.ir.{OpenCLMemory, OpenCLMemoryCollection, PrivateMemory}

object ShouldUnroll {

  def apply(lambda: Lambda): Unit =
    new ShouldUnroll(lambda)

}

class ShouldUnroll(val lambda: Lambda) {

  private val (_, privateMems, varDecls) = OpenCLGeneratorOld.getDifferentMemories(lambda)

  private def getOriginalType(mem: OpenCLMemory) =
    OpenCLGeneratorOld.getOriginalType(mem, varDecls)

  private def existsInPrivateMemories(mem: Memory): Boolean =
    privateMems.exists(_.mem == mem)

  private def shouldUnrollLoop(call: FunCall): Boolean = {
    var originalType: Type = UndefType
    try {
      originalType = getOriginalType(call.args.head.mem.asInstanceOf[OpenCLMemory])
    } catch {
      case _: VariableNotDeclaredError =>
    }
    val currentType = call.args.head.t

    val loopingOverVectorComponents = (originalType, currentType) match {
      case (_: VectorType, ArrayType(_: ScalarType)) => true
      case _ => false
    }

    loopingOverVectorComponents ||
      (OpenCLMemory.containsPrivateMemory(call.args.head.mem)
        && (call.args.head.mem match {
        case coll: OpenCLMemoryCollection =>
          coll.subMemories.exists(mem => existsInPrivateMemories(mem))
        case _ => existsInPrivateMemories(call.args.head.mem)
      })) ||
      // Don't unroll just for value
      OpenCLMemory.asOpenCLMemory(call.mem).addressSpace == PrivateMemory
  }

   Expr.visit(lambda.body, _ => Unit, {
     case call: FunCall =>

       call.f match {
         case m: MapLcl => if (shouldUnrollLoop(call)) m.shouldUnroll = true
         case m: MapSeq => if (shouldUnrollLoop(call)) m.shouldUnroll = true
         case r: ReduceSeq =>
           if (OpenCLMemory.containsPrivateMemory(call.args(1).mem))
             r.shouldUnroll = true
         case sp: MapSeqSlide=> if (shouldUnrollLoop(call)) sp.shouldUnroll = true
         case scan:ScanSeq => scan.shouldUnroll=  OpenCLMemory.containsPrivateMemory(call.args(1).mem)
         case _ =>
       }

     case _ =>
   })
}
