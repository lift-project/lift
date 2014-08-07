package generator

import Function.tupled
import opencl.executor.{local, Executor, value, global}
import opencl.generator.OpenCLGenerator
import opencl.ir._

import ir._

object Dispatcher {

  Executor.loadLibrary()

  class ProfilingInfo

  /*def execute(f: Fun, inputs: Any*) : (ProfilingInfo, Any) = {
    Executor.init()
    val result = execute1(f, inputs)
    Executor.shutdown()

    result
  }*/

  def execute(f:FunExpr, inputArray: Array[Float]) : (Double,Array[Float]) = {

    Executor.init()
    val result = execute1(f, inputArray)
    Executor.shutdown()

    result
  }

  private def executeOpenCL(f: FunExpr, inputArray: Array[Float]) : (Double,Array[Float]) = {

    val inputLen = inputArray.length
    val outputLen = f.inT match {
      case at: ArrayType => {
        val newInputType = new ArrayType(at.elemT, inputLen)
        val t = Type.check(f, newInputType, false)
        t match {
          case at: ArrayType => at.len match {
            case Cst(c) => c
          }
          case _ => throw new TypeException(t,"ArrayType")
        }
      }
      case _ => throw new TypeException(f.inT,"ArrayType")
    }

    println("Generating code for " + f)
    val kernelCode = OpenCLGenerator.generate(f)
    println("Kernel code:")
    println(kernelCode)

    val inputData = global.input(inputArray)
    val outputData = global.output[Float](outputLen)

    val params = OpenCLGenerator.Kernel.memory

    val memArgs = params.map( mem => {
      val m = mem.mem

      if      (m == f.inM)  inputData
      else if (m == f.outM) outputData
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



 private def isolateForExecution(cf: CompFunDef) : List[FunExpr] = {

    cf.flatten.foldRight(List(List[FunExpr]()))((f,ll) => {
      f match {
        case ReduceHost(_,_) | MapGlb(_) | MapWrg(_) => List() :: (f::ll.head) :: ll.tail //List(List(), List(f)) ++ ll
        case _ => (f::ll.head) :: ll.tail //List(List(f) ++ ll.head) ++ ll.tail
      }
    }).filter(_.nonEmpty).map(lf => {
      // create new composed function
      val cf = CompFunDef(lf: _*)

      // patch the type
      val inT = lf.last.inT
      val ouT = lf.head.ouT
      cf.inT = inT
      cf.ouT = ouT

      // patch the memory
      val inM = lf.last.inM
      val outM = lf.head.outM
      cf.inM = inM
      cf.outM = outM

      cf
    })

  }

  private def execute1(f:FunExpr, inputArray: Array[Float]) : (Double,Array[Float]) = {

    f match {

      case cf : CompFunDef => {

        val newCompFun = isolateForExecution(cf)

        newCompFun.foldRight((0.0d, inputArray))((f,result) => {
          val newRes = f match {
            case CompFunDef(Input(_,_)) => (0.0d,inputArray)
            case _ => executeOpenCL(f, result._2)
          }
          (newRes._1 + result._1, newRes._2)
        }
        )
      }

      case in : Input => {
        // TODO: should consume one of the inputs (change inputArray to variadic argument)
        (0,inputArray)
      }

      case _ => executeOpenCL(f, inputArray)

    }
  }

  /* private def execute1(f: Fun, inputs: Seq[Any]) : (ProfilingInfo, Any) = {

    f match {

      case cf : CompFun => {

        val newCompFun = isolateForExecution(cf)

        newCompFun.foldRight((0.0d, inputArray))((f,result) => {
          val newRes = f match {
            case CompFun(Input(_,_)) => (0.0d,inputArray)
            case _ => executeOpenCL(f, result._2)
          }
          (newRes._1 + result._1, newRes._2)
        }
        )
      }

      case _ => executeOpenCL(f, inputArray)

    }
  }*/

}
