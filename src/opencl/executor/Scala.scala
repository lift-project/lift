package opencl.executor

import ir._
import opencl.generator.OpenCLGenerator
import opencl.ir.{GlobalMemory, LocalMemory, Float}

import scala.reflect.ClassTag

object Execute {
  var wgSize = 128

  def apply(first: Array[Float], f: (Input) => CompFun) = {
    val inputSize = first.size
    val N = Var("N")
    val valueMap = scala.collection.immutable.Map[Expr, Expr](N -> inputSize)

    val inputs = Array(Input(Var("x"), ArrayType(Float, N)))

    val kernel = f(inputs(0))

    Type.check(kernel, NoType)

    val kernelCode = OpenCLGenerator.generate(kernel)
    println("Kernel code:")
    println(kernelCode)

    val outputSize = Expr.substitute(Type.getLength(kernel.ouT), valueMap).eval()

    val inputData = global.input(first)
    val outputData = global.output[Float](outputSize)

    val memArgs = OpenCLGenerator.Kernel.memory.map( mem => {
      val m = mem.mem
      if (m == inputs(0).outM) inputData
      else if (m == kernel.outM) outputData
      else m.addressSpace match {
        case LocalMemory => local(Expr.substitute(m.size, valueMap).eval() * 4) // TODO: check on this ...
        case GlobalMemory => global(Expr.substitute(m.size, valueMap).eval() * 4)
      }
    })

    val args = memArgs :+ value(inputSize)

    val runtime = Executor.execute(kernelCode, wgSize, inputSize, args)

    val outputArray = outputData.asFloatArray()

    args.foreach(_.dispose)

    (outputArray, runtime)
  }

  def apply(first: Array[Float], second: Array[Float], f: (Input, Input) => Fun, wgSize: Int = 128) = {
    val N = Var("N")
    val M = Var("M")
    val valueMap = scala.collection.immutable.Map[Expr, Expr](N -> first.size, M -> second.size)

    val inputs = Array(Input(Var("x"), ArrayType(Float, N)), Input(Var("y"), ArrayType(Float, M)))

    val kernel = f(inputs(0), inputs(1))

    Type.check(kernel, NoType)

    val kernelCode = OpenCLGenerator.generate(kernel)
    println("Kernel code:")
    println(kernelCode)

    val outputSize = Expr.substitute(Type.getLength(kernel.ouT), valueMap).eval()

    val data = Array(global.input(first), global.input(second))
    val outputData = global.output[Float](outputSize)

    val memArgs = OpenCLGenerator.Kernel.memory.map( mem => {
      val m = mem.mem
      if (m == inputs(0).outM) data(0)
      else if (m == inputs(1).outM) data(1)
      else if (m == kernel.outM) outputData
      else m.addressSpace match {
        case LocalMemory => local(Expr.substitute(m.size, valueMap).eval() * 4) // TODO: check on this ...
        case GlobalMemory => global(Expr.substitute(m.size, valueMap).eval() * 4)
      }
    })

    val args = memArgs :+ value(first.size) :+ value(second.size)

    val runtime = Executor.execute(kernelCode, wgSize, first.size, args)

    val outputArray = outputData.asFloatArray()

    args.foreach(_.dispose)

    (outputArray, runtime)
  }
}


object global {
  object input {
    def apply(array: Array[Float]) = GlobalArg.createInput(array)
    def apply(array: Array[Int]) = GlobalArg.createInput(array)
  }

  object output {
    def apply[T : ClassTag](length: Int) = {
      implicitly[ClassTag[T]] match {
        case ClassTag.Float => GlobalArg.createOutput(length * 4) // in bytes
        case ClassTag.Int => GlobalArg.createOutput(length * 4) // in bytes
        case _ => throw new IllegalArgumentException
      }
    }
  }

  def apply(size: Int) = GlobalArg.createOutput(size)
}

object local {
  def apply(size: Int) = LocalArg.create(size)
}

object value {
  def apply(value: Float) = ValueArg.create(value)
  def apply(value: Int) = ValueArg.create(value)
}