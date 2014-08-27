package opencl.executor

import ir._
import opencl.generator.OpenCLGenerator
import opencl.ir._

import scala.reflect.ClassTag

object Compile {
  def apply(f: Lambda) = {
    Type.check(f.body)

    val kernelCode = OpenCLGenerator.generate(f)
    println("Kernel code:")
    println(kernelCode)

    kernelCode
  }
}

object Execute {
  def apply(globalSize: Int): Execute = {
    apply(128, globalSize)
  }

  def apply(localSize: Int, globalSize: Int): Execute = {
    new Execute(localSize, globalSize)
  }
}

class Execute(val localSize: Int, val globalSize: Int) {
  def apply(f: Lambda, values: Any*) : (Array[Float], Double) = {
    assert( f.params.forall( _.t != UndefType ), "Types of the params have to be set!" )
    val code = Compile(f)
    apply(code, f, values:_*)
  }

  def apply(code: String, f: Lambda, values: Any*) : (Array[Float], Double) = {

    val vars = f.params.map((p) => Type.getLengths(p.t).filter(_.isInstanceOf[Var])).flatten// just take the variable
    val sizes = values.map({
        case aaa: Array[Array[Array[_]]] => Seq(Cst(aaa.size), Cst(aaa(0).size), Cst(aaa(0)(0).size))
        case aa: Array[Array[_]] => Seq(Cst(aa.size), Cst(aa(0).size))
        case a: Array[_] => Seq(Cst(a.size))
        case any: Any => Seq(Cst(1))
      }).flatten[ArithExpr]
    val valueMap = (vars zip sizes).toMap[ArithExpr, ArithExpr]

    val outputSize = ArithExpr.substitute(Type.getLengths(f.body.t).reduce(_*_), valueMap).eval()

    val inputs = values.map({
      case f: Float => value(f)
      case af: Array[Float] => global.input(af)
      case aaf: Array[Array[Float]] => global.input(aaf.flatten)
      case aaaf: Array[Array[Array[Float]]] => global.input(aaaf.flatten.flatten)

      case i: Int => value(i)
      case ai: Array[Int] => global.input(ai)
      case aai: Array[Array[Int]] => global.input(aai.flatten)
    })
    val outputData = global.output[Float](outputSize)

    val memArgs = OpenCLGenerator.Kernel.memory.map( mem => {
      val m = mem.mem
      val i = f.params.indexWhere( m == _.outM )
      if (i != -1) inputs(i)
      else if (m == f.body.outM) outputData
      else m.addressSpace match {
        case LocalMemory => local(ArithExpr.substitute(m.size, valueMap).eval())
        case GlobalMemory => global(ArithExpr.substitute(m.size, valueMap).eval())
      }
    })

    val args: Array[KernelArg] = (memArgs ++ inputs).distinct.toArray

    println("args.length " + args.length)

    val runtime = Executor.execute(code, localSize, globalSize, args)

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