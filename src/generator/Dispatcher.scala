package generator

import opencl.executor.{Executor, value, global}
import opencl.generator.OpenCLGenerator
import opencl.ir.{MapWrg, MapGlb, ReduceHost}
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

        val inputData = global.input(inputArray)
        val outputData = global.output[Float](outputLen)

        val args = Array(inputData, outputData, value(inputLen))

        println("Generating code for " + f)
        val kernelCode = OpenCLGenerator.generate(f)
        println("Kernel code:")

        Executor.execute(kernelCode, 128, inputLen, args)

        outputData.asFloatArray()
      }

      case cf : CompFun => {
        cf.funs.foldRight(inputArray)((f,inputArray) => execute1(f, inputArray))
      }
    }
  }

}
