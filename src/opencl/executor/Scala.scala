package opencl.executor

import arithmetic.{Var, Cst, ?, ArithExpr}
import ir._
import opencl.generator.{Verbose, OpenCLGenerator}
import opencl.ir._

import scala.collection.immutable
import scala.reflect.ClassTag

object Eval {
  def apply(code: String): Lambda = {
    val imports = """
                    |import arithmetic._
                    |import ir._
                    |import opencl.ir._
                    |
                  """.stripMargin
    com.twitter.util.Eval[Lambda](imports ++ code)
  }
}

object Compile {
  def apply(code: String): (String, Lambda) = {
    val f = Eval(code)
    (apply(f), f)
  }

  def apply(f: Lambda): String = apply(f, ?, ?, ?)

  def apply(f: Lambda,
            localSize1: ArithExpr, localSize2: ArithExpr, localSize3: ArithExpr): String =
    apply(f, localSize1, localSize2, localSize3, ?, ?, ?, immutable.Map())

  def apply(f: Lambda,
            localSize0: ArithExpr, localSize1: ArithExpr, localSize2: ArithExpr,
            globalSize1: ArithExpr, globalSize2: ArithExpr, globalSize3: ArithExpr,
            valueMap: immutable.Map[ArithExpr, ArithExpr]) = {

    Type.check(f.body)

    val kernelCode = OpenCLGenerator.generate(f,
      Array(localSize0, localSize1, localSize2),
      Array(globalSize1, globalSize2, globalSize3), valueMap)
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

  def apply(localSize: Int, globalSize: Int, injectSizes: (Boolean, Boolean) = (false, false)): Execute = {
    new Execute(localSize, 1, 1, globalSize, 1, 1, injectSizes._1, injectSizes._2)
  }

  def apply(localSize1: Int, localSize2: Int, globalSize1: Int,  globalSize2: Int,
            injectSizes: (Boolean, Boolean)): Execute = {
    new Execute(localSize1, localSize2, 1, globalSize1, globalSize2, 1, injectSizes._1, injectSizes._2)
  }

  def apply(localSize1: Int, localSize2: Int, localSize3: Int,
            globalSize1: Int,  globalSize2: Int, globalSize3: Int,
            injectSizes: (Boolean, Boolean)): Execute = {
    new Execute(localSize1, localSize2, localSize3, globalSize1, globalSize2, globalSize3, injectSizes._1, injectSizes._2)
  }

  def createValueMap(f: Lambda, values: Any*): immutable.Map[ArithExpr, ArithExpr] = {
    // just take the variables
    val vars = f.params.map((p) => Type.getLengths(p.t).filter(_.isInstanceOf[Var])).flatten

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
      case aaaa: Array[Array[Array[Array[_]]]] => Seq(Cst(aaaa.length), Cst(aaaa(0).length), Cst(aaaa(0)(0).length), Cst(aaaa(0)(0)(0).length / tupleSize))
      case aaa: Array[Array[Array[_]]] => Seq(Cst(aaa.length), Cst(aaa(0).length), Cst(aaa(0)(0).length / tupleSize))
      case aa: Array[Array[_]] => Seq(Cst(aa.length), Cst(aa(0).length / tupleSize))
      case a: Array[_] => Seq(Cst(a.length / tupleSize))
      case any: Any => Seq(Cst(1))
    }).flatten[ArithExpr]

    (vars zip sizes).toMap[ArithExpr, ArithExpr]
  }
}

class Execute(val localSize1: Int, val localSize2: Int, val localSize3: Int,
              val globalSize1: Int, val globalSize2: Int, val globalSize3: Int,
              val injectLocalSize: Boolean, val injectGroupSize: Boolean = false) {

  def apply(input: String, values: Any*): (Array[Float], Double) = {
    val (code, f) = Compile(input)
    apply(code, f, values:_*)
  }

  def apply(f: Lambda, values: Any*): (Array[Float], Double) = {
    val valueMap = Execute.createValueMap(f, values:_*)

    val code = if (injectLocalSize)
      if (injectGroupSize)
        Compile(f, localSize1, localSize2, localSize3,
          globalSize1, globalSize2, globalSize3, valueMap)
      else
        Compile(f, localSize1, localSize2, localSize3)
    else Compile(f)

    apply(code, f, values:_*)
  }

  def apply(code: String, f: Lambda, values: Any*) : (Array[Float], Double) = {

    val valueMap: immutable.Map[ArithExpr, ArithExpr] = Execute.createValueMap(f, values:_*)

    val outputSize = ArithExpr.substitute(Type.getSize(f.body.t), valueMap).eval()

    // Check all Group functions valid arguments for the given input sizes
    staticGroupCheck(f, valueMap)

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

    val args: Array[KernelArg] = (memArgs ++ inputs).distinct

    if (Verbose())
      println("args.length " + args.length)

    val runtime = Executor.execute(code, localSize1, localSize2, localSize3,
      globalSize1, globalSize2, globalSize3, args)

    val output = outputData.asFloatArray()

    args.foreach(_.dispose)

    (output, runtime)
  }

  /** Check that all possible indices returned by Group calls are in-bounds */
  def staticGroupCheck(f: Lambda, valueMap: immutable.Map[ArithExpr, ArithExpr]): Unit = {
    val groupFuns = Expr.visit(Set[Group]())(f.body, (expr, set) =>
      expr match {
        case call: FunCall => call.f match {
          case group: Group => set + group
          case _ => set
        }
        case _ => set
      })

    for (g <- groupFuns) {
      val allIndices = g.relIndices.min to g.relIndices.max

      g.params(0).t match  {
        case ArrayType(_, lenExpr) =>
          val length = ArithExpr.substitute(lenExpr, valueMap).eval()

          for (relIdx <- allIndices) {
            var newIdx = 0
            if (relIdx < 0) {
              newIdx = g.negOutOfBoundsF(relIdx, length).eval()
            } else if (relIdx > 0) {
              newIdx = g.posOutOfBoundsF(relIdx, length).eval()
            }

            if (newIdx < 0 || newIdx >= length) {
              throw new IllegalArgumentException("Group function would map relative out-of-bounds index " + relIdx +
                " to new illegal index " + newIdx + ".")
            }
          }
      }
    }
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