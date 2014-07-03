package generator

import opencl.executor.{Executor, value, global}
import opencl.executor.KernelArg
import opencl.generator.OpenCLGenerator
import opencl.ir.{MapWrg, MapGlb, ReduceHost}

import scala.collection.mutable
import scala.collection.immutable


import ir._

object Dispatcher {

  // TODO: maybe based this method on a trait that determines if a function is generable with OpenCL or not
  private def isOpenCLGenerable(f: Fun) : Boolean = {
    f match {
      case MapGlb(_) | MapWrg(_) | ReduceHost(_)=> true
      case _ => false
    }
  }

  //
  def execute(f:Fun) {
    // TODO: should generate the kernel if not yet generated (maintain a cache) and execute the function

    val inputSize = 1024*1024

    val inputArray = Array.fill(inputSize)(1.0f)
    val inputData = global.input(inputArray)
    val outputData = global.output[Float](inputSize / 2048)

    val args = Array(inputData, outputData, value(inputSize))

    val kernels = generateOpenCLKernels(f)

    val outputArray = outputData.asFloatArray()

   /* def execute(f:Fun, inputSize: Int, args :Array[KernelArg]) {

      val kernelCode = kernels.get(f)
      if (!kernelCode.isEmpty)
        f match {
          case ReduceHost => Executor.execute(kernels.get(f).get, 1, inputSize, args)
          case _ => Executor.execute(kernels.get(f).get, 128, inputSize, args)
        }
      else
        f match
          case cf: CompFun => {
            cf.funs.foldRight
         }
          //case _ => // error

    }*/


  }

  def generateOpenCLKernels(f: Fun) : immutable.Map[Fun, String] = {

    val result = mutable.Map[Fun, String]()

    if (isOpenCLGenerable(f)) {
      println("Generating code for " + f)
      val kernelCode = OpenCLGenerator.compile(f)
      println("Kernel code:")
      println(kernelCode)
      result += (f -> kernelCode)
    }
    else
      f match {
        case cf : CompFun => result ++= cf.funs.map(generateOpenCLKernels(_)).reduce((x,y) => x ++ y)
      }

    result.toMap
  }

}
