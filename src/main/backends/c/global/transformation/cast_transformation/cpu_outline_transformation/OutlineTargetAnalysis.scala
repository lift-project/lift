package backends.c.global.transformation.cast_transformation.cpu_outline_transformation

import backends.c.host.host_ir.{CPUFunc, OclFunc}
import ir.ast.{FunCall, Lambda}
import opencl.generator.NDRange

import scala.collection.mutable

object CPUOutlineTargetAnalysis {

  def apply (lambda: Lambda) : List[Lambda] = {

    val online_targests = scala.collection.mutable.ListBuffer.empty[Lambda]

    lambda visitBy {
      case cf@FunCall(c:CPUFunc, _*) => c.f.funcName = c.funcName; online_targests += c.f
      //case cf@FunCall(c:CPUFunc2, _*) => c.f.funcName = c.funcName; online_targests += c.f
      case _ =>
    }

    online_targests.toList

  }

}


object OclOutlineTargetAnalysis {

  def apply (lambda: Lambda) : List[Tuple4[String,NDRange, NDRange, Lambda]] = {

    //val online_targets = scala.collection.mutable.ListBuffer.empty[Lambda]
    val online_targets = mutable.ListBuffer.empty[Tuple4[String,NDRange, NDRange, Lambda]]

    lambda visitBy {
      case cf@FunCall(c:OclFunc, _*) =>
        c.f.funcName = c.funcName
        online_targets += Tuple4("kernel_"+cf.gid+".cl", c.ndranges._1, c.ndranges._2, c.f )
      case _ =>
    }

    online_targets.toList

  }

}
