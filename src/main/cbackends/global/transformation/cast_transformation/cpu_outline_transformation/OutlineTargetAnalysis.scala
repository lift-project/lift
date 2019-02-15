package cbackends.global.transformation.cast_transformation.cpu_outline_transformation

import cbackends.host.host_ir.{CPUFunc, CPUFunc2, OclFunc}
import ir.ast.{FunCall, Lambda}

object CPUOutlineTargetAnalysis {

  def apply (lambda: Lambda) : List[Lambda] = {

    val online_targests = scala.collection.mutable.ListBuffer.empty[Lambda]

    lambda visitBy {
      case cf@FunCall(c:CPUFunc, _*) => c.f.funcName = c.funcName; online_targests += c.f
      case cf@FunCall(c:CPUFunc2, _*) => c.f.funcName = c.funcName; online_targests += c.f
      case _ =>
    }

    online_targests.toList

  }

}


object OclOutlineTargetAnalysis {

  def apply (lambda: Lambda) : List[Lambda] = {

    val online_targests = scala.collection.mutable.ListBuffer.empty[Lambda]

    lambda visitBy {
      case cf@FunCall(c:OclFunc, _*) => c.f.funcName = c.funcName; online_targests += c.f
      case _ =>
    }

    online_targests.toList

  }

}
