package cbackends.global.transformation.cast_transformation.cpu_outline_transformation

import cbackends.host.host_ir.{CPUFunc, OclFunc}
import ir.ast.{FunCall, Lambda}

import scala.collection.mutable

object CPUOutlineTargetAnalysis {

  def apply (lambda: Lambda) : List[Lambda] = {

    val online_targets = scala.collection.mutable.ListBuffer.empty[Lambda]

    lambda visitBy {
      case cf@FunCall(c:CPUFunc, _*) => c.f.funcName = c.funcName; online_targets += c.f
      //case cf@FunCall(c:CPUFunc2, _*) => c.f.funcName = c.funcName; online_targets += c.f
      case _ =>
    }

    online_targets.toList

  }

}


object OclOutlineTargetAnalysis {

  def apply (lambda: Lambda) : Map[String, Lambda] = {

    //val online_targets = scala.collection.mutable.ListBuffer.empty[Lambda]
    val online_targets = mutable.Map.empty[String,Lambda]

    lambda visitBy {
      case cf@FunCall(c:OclFunc, _*) => c.f.funcName = c.funcName; online_targets += ("kernel_"+cf.gid+".cl") -> c.f
      case _ =>
    }

    online_targets.toMap

  }

}
