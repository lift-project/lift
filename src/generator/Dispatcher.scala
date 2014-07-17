package generator

import opencl.executor.{local, Executor, value, global}
import opencl.generator.OpenCLGenerator
import opencl.ir._
import org.junit.{AfterClass, BeforeClass}

import ir._

object Dispatcher {

  Executor.loadLibrary()


  /*@BeforeClass def before() {
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }*/


  def execute(f:Fun, inputArray: Array[Float]) : Array[Float] = {

    Executor.init()
    val result = execute1(f, inputArray)
    Executor.shutdown()

    result
  }

  private def execute1(f:Fun, inputArray: Array[Float]) : Array[Float] = {

    f match {

      case in : Input => {
        // TODO: should consume one of the inputs (change inputArray to variadic argument)
        inputArray
      }

      case MapGlb(_) | MapWrg(_) | ReduceHost(_) => {

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



        f match {
          case _: ReduceHost => Executor.execute(kernelCode, 1, inputLen, args)  // single-threaded execution
          case _ => Executor.execute(kernelCode, 128, inputLen, args)
        }

        val result = outputData.asFloatArray()
        result
      }

      case cf : CompFun => {
        cf.funs.foldRight(inputArray)((f,inputArray) => execute1(f, inputArray))
      }
    }
  }

}
