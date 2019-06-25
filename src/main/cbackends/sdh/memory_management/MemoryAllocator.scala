package cbackends.sdh.memory_management

import ir.ast.{FunCall, IRNode, Lambda}
import cbackends.common.memory_management.MemoryAllocator.{init_params, post_check, pre_check}
import cbackends.common.utils.pattern_matching.IsDefinedAt
import cbackends.sdh.sdh_ir.{ToGPE, ToLCP}

object MemoryAllocator {

  def alloc(node: IRNode, cont: IRNode => IRNode) : IRNode = {

    node match {
      case fc@FunCall(_:ToLCP|_:ToGPE, arg) =>
        cont(arg)
        fc.mem = arg.mem;fc
    }

  }

  def composed_generateInputView(in: IRNode) : IRNode = {

    val partial_binded_common = new PartialFunction[IRNode, IRNode] with IsDefinedAt[IRNode]
    { def apply(x: IRNode) = cbackends.common.memory_management.MemoryAllocator.alloc(x, composed_generateInputView) }
    val partial_binded_sdh = new PartialFunction[IRNode,IRNode] with IsDefinedAt[IRNode]
    { def apply(x: IRNode) = cbackends.sdh.memory_management.MemoryAllocator.alloc(x, composed_generateInputView) }
    val composed = partial_binded_common orElse partial_binded_sdh
    composed(in)

  }


  def apply(lambda: Lambda): Unit = {


    pre_check(lambda)

    init_params(lambda)

    composed_generateInputView(lambda.body)

    post_check(lambda)

  }

}
