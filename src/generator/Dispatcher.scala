package generator

import opencl.generator.OpenCLGenerator
import opencl.ir.ReduceHost

import scala.collection.mutable
import scala.collection.immutable


import ir._

object Dispatcher {

  // TODO: maybe based this method on a trait that determines if a function is generable with OpenCL or not
  private def isOpenCLGenerable(f: Fun) : Boolean = {
    f match {
      case cf : CompFun => cf.funs.foldLeft(true)((result, f) => result && isOpenCLGenerable(f))
      case rh : ReduceHost => false
      case NullFun => false
      case _ => true
    }
  }

  //
  def execute(f:Fun) {
    // TODO: should generate the kernel if not yet generated (maintain a cache) and execute the function
  }

  def generateOpenCLKernels(f: Fun) : immutable.Map[Fun, String] = {

    // TODO: WIP finish this

    val openCLFuns = mutable.Set[Fun]()

    if (isOpenCLGenerable(f))
      openCLFuns += f
    else
      f match {
        case cf : CompFun => cf.funs.map(generateOpenCLKernels(_)).reduce((x,y) => x ++ y)
        case _ => immutable.Map()
      }

    println("Generating code for " + f)
    val kernelCode = OpenCLGenerator.compile(f)
    println("Kernel code:")
    println(kernelCode)
    //results += (f -> kernelCode)

    val results = mutable.Map[Fun, String]()


    results.toMap
  }

}
