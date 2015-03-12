package opencl.executor

import ir._
import opencl.generator.{Verbose, OpenCLGenerator}
import opencl.ir._

import scala.collection.immutable
import scala.reflect.ClassTag

object Compile {
  def apply(f: Lambda): String = apply(f, -1)

  def apply(f: Lambda, localSize0: Int): String = {
    Type.check(f.body)

    val kernelCode = OpenCLGenerator.generate(f, localSize0)
    if (Verbose()) {
      println("Kernel code:")
      println(kernelCode)
    }

    kernelCode
  }
}

object Execute {
  def apply(globalSize: Int): Execute = {
    apply(128, globalSize)
  }

  def apply(localSize: Int, globalSize: Int, injectLocalSize: Boolean = false): Execute = {
    new Execute(localSize, globalSize, injectLocalSize)
  }

  def createValueMap(f: Lambda, values: Any*): immutable.Map[ArithExpr, ArithExpr] = {
    val vars = f.params.map((p) => Type.getLengths(p.t).filter(_.isInstanceOf[Var])).flatten // just take the variable

    val tupleSizes = f.params.map(_.t match {
      case ArrayType(ArrayType(ArrayType(tt: TupleType, _), _), _) => tt.elemsT.length
      case ArrayType(ArrayType(tt: TupleType, _), _) => tt.elemsT.length
      case ArrayType(tt: TupleType, _) => tt.elemsT.length
      case tt: TupleType => tt.elemsT.length
      case ArrayType(ArrayType(ArrayType(vt: VectorType, _), _), _) => vt.len.eval()
      case ArrayType(ArrayType(vt: VectorType, _), _) => vt.len.eval()
      case ArrayType(vt: VectorType, _) => vt.len.eval()
      case vt: VectorType => vt.len.eval()
      case _ => 1
    })

    val sizes = (values, tupleSizes).zipped.map((value, tupleSize) => value match {
      case aaaa: Array[Array[Array[Array[_]]]] => Seq(Cst(aaaa.size), Cst(aaaa(0).size), Cst(aaaa(0)(0).size), Cst(aaaa(0)(0)(0).size / tupleSize))
      case aaa: Array[Array[Array[_]]] => Seq(Cst(aaa.size), Cst(aaa(0).size), Cst(aaa(0)(0).size / tupleSize))
      case aa: Array[Array[_]] => Seq(Cst(aa.size), Cst(aa(0).size / tupleSize))
      case a: Array[_] => Seq(Cst(a.size / tupleSize))
      case any: Any => Seq(Cst(1))
    }).flatten[ArithExpr]

    (vars zip sizes).toMap[ArithExpr, ArithExpr]
  }
}

class Execute(val localSize: Int, val globalSize: Int, injectLocalSize: Boolean) {
  def apply(f: Lambda, values: Any*) : (Array[Float], Double) = {
    assert( f.params.forall( _.t != UndefType ), "Types of the params have to be set!" )
    val code = if (injectLocalSize) Compile(f, localSize) else Compile(f)
    apply(code, f, values:_*)
  }

  def apply(code: String, f: Lambda, values: Any*) : (Array[Float], Double) = {

    val valueMap: immutable.Map[ArithExpr, ArithExpr] = Execute.createValueMap(f, values:_*)

    println(valueMap)

    val outputSize = ArithExpr.substitute(Type.getSize(f.body.t), valueMap).eval()

    val inputs = values.map({
      case f: Float => value(f)
      case af: Array[Float] => global.input(af)
      case aaf: Array[Array[Float]] => global.input(aaf.flatten)
      case aaaf: Array[Array[Array[Float]]] => global.input(aaaf.flatten.flatten)
      case aaaf: Array[Array[Array[Array[Float]]]] => global.input(aaaf.flatten.flatten.flatten)


      case i: Int => value(i)
      case ai: Array[Int] => global.input(ai)
      case aai: Array[Array[Int]] => global.input(aai.flatten)
    })
    val outputData = global(outputSize)

    val memArgs = OpenCLGenerator.Kernel.memory.map( mem => {
      val m = mem.mem
      val i = f.params.indexWhere( m == _.mem )
      if (i != -1) inputs(i)
      else if (m == f.body.mem) outputData
      else m.addressSpace match {
        case LocalMemory => local(ArithExpr.substitute(m.size, valueMap).eval())
        case GlobalMemory => global(ArithExpr.substitute(m.size, valueMap).eval())
      }
    })

    val args: Array[KernelArg] = (memArgs ++ inputs).distinct.toArray

    if (Verbose())
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