package cbackends.global.transformation.cast_transformation.cpu_outline_transformation

import cbackends.global.global_ir.CPUFunc
import ir.ast.{FunCall, Lambda}

object OutlineTargetAnalysis {

  def apply (lambda: Lambda) : List[FunCall] = {

    val online_targests = scala.collection.mutable.ListBuffer.empty[FunCall]

    lambda visitBy {
      case cf@FunCall(_:CPUFunc, _*) => online_targests += cf
      case _ =>
    }

    online_targests.toList

  }

}
