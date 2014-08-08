package opencl.executor

import ir._
import opencl.generator.OpenCLGenerator
import opencl.ir.{OpenCLMemory, GlobalMemory, LocalMemory, Float}

import scala.reflect.ClassTag

object Compile {
  def apply(f: Lambda) = {
    Type.check(f.body, NoType)

    // allocate the params and set the corresponding type
    f.params.map( (p) => {
      p.inM = OpenCLMemory.allocGlobalMemory(OpenCLMemory.getMaxSizeInBytes(p.outT))
      p.inT = p.outT
    })

    val kernelCode = OpenCLGenerator.generate(f.body)
    println("Kernel code:")
    println(kernelCode)

    kernelCode
  }
}

object Execute {
  var wgSize = 128

  def apply(f: Lambda, values: Array[Float]*) : (Array[Float], Double) = {
    assert( f.params.forall( _.outT != UndefType ), "Types of the params have to be set!" )
    val code = Compile(f)
    Execute(code, f, values:_*)
  }

  def apply(code: String, f: Lambda, values: Array[Float]*) : (Array[Float], Double) = {
    val valueMap = (    f.params.map( (p) => Type.getLength(p.outT))
      zip values.map( (a) => Cst(a.size)) ).toMap[Expr, Expr]

    val outputSize = Expr.substitute(Type.getLength(f.body.outT), valueMap).eval()

    val inputs = values.map( global.input(_) )
    val outputData = global.output[Float](outputSize)

    val memArgs = OpenCLGenerator.Kernel.memory.map( mem => {
      val m = mem.mem
      val i = f.params.indexWhere( m == _.outM )
      if (i != -1) inputs(i)
      else if (m == f.body.outM) outputData
      else m.addressSpace match {
        case LocalMemory => local(Expr.substitute(m.size, valueMap).eval() * 4) // TODO: check on this ...
       case GlobalMemory => global(Expr.substitute(m.size, valueMap).eval() * 4)
      }
    })

    val args = memArgs ++ values.map( (a) =>  value(a.size) )

    // TODO: think about global size
    val runtime = Executor.execute(code, wgSize, values(0).size, args)

    val output = outputData.asFloatArray()

    args.foreach(_.dispose)

    (output, runtime)
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