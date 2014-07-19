package generator

import opencl.executor.{local, Executor, value, global}
import opencl.generator.OpenCLGenerator
import opencl.ir._

import ir._

object Dispatcher {

  Executor.loadLibrary()

  def execute(f:Fun, inputArray: Array[Float]) : (Double,Array[Float]) = {

    Executor.init()
    val result = execute1(f, inputArray)
    Executor.shutdown()

    result
  }

  private def executeOpenCL(f: Fun, inputArray: Array[Float]) : (Double,Array[Float]) = {

    val inputLen = inputArray.length
    val outputLen = f.inT match {
      case at: ArrayType => {
        val newInputType = new ArrayType(at.elemT, inputLen)
        Type.check(f, newInputType, false) match {
          case at: ArrayType => at.len match {
            case Cst(c) => c
          }
        }
      }
    }

    println("Generating code for " + f)
    val kernelCode = OpenCLGenerator.generate(f)
    println("Kernel code:")
    println(kernelCode)

    val inputData = global.input(inputArray)
    val outputData = global.output[Float](outputLen)

    val mems = OpenCLGenerator.Kernel.memory

    val memArgs = mems.map(m => {
      if (m == f.inM) inputData
      else if (m == f.outM) outputData
      else m.addressSpace match {
        case LocalMemory => local(m.size.eval())
        case GlobalMemory =>  throw new NotImplementedError()
      }
    })

    val allVars = OpenCLGenerator.Kernel.memory.map( m => Var.getVars(m.size) ).filter(_.nonEmpty).flatten.distinct
    val args =
      if (allVars.length == 0)
        memArgs
      else if (allVars.length == 1)
        memArgs :+ value(inputLen)
      else
        throw new NotImplementedError()



    val time = f match {
      case _: ReduceHost => Executor.execute(kernelCode, 1, inputLen, args)  // single-threaded execution
      case _ => Executor.execute(kernelCode, 128, inputLen, args)
    }

    val result = outputData.asFloatArray()
    (time,result)
  }

  private def isolateReduceHost(cf: CompFun) : List[CompFun] = {

    // TODO: must also isolate mapGlb and map Wrkg !
    // problem if we have reduceHost o join o mapGlobal o split o join o mapGlobal o split   for instance!!!

    // flatten all the composed functions
    def flatten(cf: CompFun) : List[Fun] = {
      cf.funs.foldLeft(List[Fun]())((l, f) => {
        f match {
          case cf: CompFun => l ++ flatten(cf)
          case _ => l :+ f
        }
      })
    }

    flatten(cf).foldLeft(List(List[Fun]()))((ll,f) => {
      f match {
        case ReduceHost(_) => ll :+ List(f) :+ List()
        case _ => ll.init :+ (ll.last :+ f)
      }
    }).filter(_.nonEmpty).map(l => CompFun(l : _*))

  }

  private def execute1(f:Fun, inputArray: Array[Float]) : (Double,Array[Float]) = {

    f match {

      case cf : CompFun => {

        // partitions the function by isolating ReduceHost
        val newCompFun = isolateReduceHost(cf)

        newCompFun.foldRight((0.0d, inputArray))((cf,result) => {
          val newRes = executeOpenCL(cf, result._2)
          (newRes._1+result._1, newRes._2)
        })
      }

      case in : Input => {
        // TODO: should consume one of the inputs (change inputArray to variadic argument)
        (0,inputArray)
      }

      case _ => executeOpenCL(f, inputArray)

    }
  }

}
