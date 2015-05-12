package generator

import opencl.executor.Executor
import opencl.generator.OpenCLGenerator
import opencl.ir._

import ir._

object Dispatcher {

  Executor.loadLibrary()

  class ProfilingInfo


  def execute(f:Lambda, inputs: Seq[Any]) : (Double,Any) = {

    Executor.init()
    val result = execute1(f, inputs)
    Executor.shutdown()

    result
  }

  /*private def executeOpenCL(f: Lambda, inputs: Seq[Any]) : (Double,Any) = {

    val inputLen = inputArray.length
    val outputLen = f.body.inT match {
      case at: ArrayType =>
        val newInputType = new ArrayType(at.elemT, inputLen)
        val t = Type.check(f.body, newInputType, setType=false)
        t match {
          case at: ArrayType => at.len match {
            case Cst(c) => c
          }
          case _ => throw new TypeException(t,"ArrayType")
        }

      case _ => throw new TypeException(f.body.inT,"ArrayType")
    }

    println("Generating code for " + f)
    val kernelCode = OpenCLGenerator.generate(f.body)
    println("Kernel code:")
    println(kernelCode)

    val inputData = global.input(inputArray)
    val outputData = global.output[Float](outputLen)

    val params = OpenCLGenerator.Kernel.memory

    val memArgs = params.map( mem => {
      val m = mem.mem

      if      (m == f.body.inM)  inputData
      else if (m == f.body.outM) outputData
      else m.addressSpace match {
        case LocalMemory => local(m.size.eval())
        case GlobalMemory =>  throw new NotImplementedError()  //TODO: bug when reaching this
      }
    })

    val allVars = OpenCLGenerator.Kernel.memory.map(
      mem => Var.getVars(mem.mem.size) ).filter(_.nonEmpty).flatten.distinct

    val args =
      if (allVars.length == 0)
        memArgs
      else if (allVars.length == 1)
        memArgs :+ value(inputLen)
      else
        throw new NotImplementedError()



    val time = f match {
      case _: ReduceHost => Executor.execute(kernelCode, 1, inputLen, args)  // single-threaded execution
      case _ => Executor.execute(kernelCode, scala.math.min(128,inputLen), inputLen, args)
    }

    val result = outputData.asFloatArray()
    (time,result)
  }
*/


 /*private def isolateForExecution(cf: CompFunDef) : List[Lambda] = {

    cf.flatten.foldRight(List(List[Lambda]()))((f,ll) => {
      f.callee match {
        case ReduceHost(_,_) | MapGlb(_) | MapWrg(_) => List() :: (f::ll.head) :: ll.tail //List(List(), List(f)) ++ ll
        case _ => (f::ll.head) :: ll.tail //List(List(f) ++ ll.head) ++ ll.tail
      }
    }).filter(_.nonEmpty).map(lf => {
      // create new composed function
      val cf = new Lambda(lf.CompFunDef(lf: _*)

      // patch the type
      val inT = lf.last.body.inT
      val ouT = lf.head.body.outT
      cf.inT = inT
      cf.ouT = ouT

      // patch the memory
      val inM = lf.last.body.inM
      val outM = lf.head.body.outM
      cf.inM = inM
      cf.outM = outM

      cf
    })

  }*/

    private def execute1(f:Lambda, inputs: Seq[Any]) : (Double,Any) = {

      (0.0f, 0.0f)

    /*f.callee match {

      case cf : CompFunDef => {

        val newCompFun = isolateForExecution(cf)

        newCompFun.foldRight((0.0d, inputs))((f,result) => {

          val newRes = executeOpenCL(f, result._2)
          (newRes._1 + result._1, Seq(newRes._2))
        }
        )
      }

      case _ => executeOpenCL(f, inputs)

    }*/
  }



}
