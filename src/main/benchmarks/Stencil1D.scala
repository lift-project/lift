package benchmarks

import lift.arithmetic.Var
import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._
import opencl.executor.Utils

@deprecated("Uses an old benchmark infrastructure", "")
class Stencil1D(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("Stencil1D", Seq(1024 * 1024), f, 0.01f) {


  override def runScala(inputs: Any*): Array[Float] = {
    Stencil1D.runScala(inputs(0).asInstanceOf[Array[Float]])
  }

  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    //val inputData = Array.tabulate(inputSizeM, inputSizeN)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val inputData = Array.tabulate(inputSizeN)(x => util.Random.nextFloat())

    Seq(inputData, Stencil1D.weightsBig)
  }

  override def globalSize: Array[Int] = {
    Array(inputSizes()(0),1,1)
  }
}

object Stencil1D{

  val scalaClamp = (idx: Int, length: Int) => {
    if(idx<0) 0 else if(idx>length-1) length-1 else idx
  }

  val scalaWrap = (idx: Int, length: Int) => {
    (idx % length + length) % length
  }

  val scalaMirror = (idx: Int, length: Int) => {
    val id = (if(idx < 0) -1-idx else idx) % (2*length)
    if(id >= length) length+length-id-1 else id
  }

  val size = 9
  val step = 1
  val left = 4
  val right = 4
  val scalaBoundary = scalaWrap
  val makePositive = UserFun("makePositive", "i", "{ return (i < 0) ? 0 : i;  }", Float, Float)
  val weights = Array(0.25f, 0.5f, 0.25f)
  val weightsBig = Array(1,1,1,1,1,1,1,1,1).map(_.toFloat)


  def runScala(input: Array[Float]): Array[Float] = {
    Utils.scalaCompute1DStencil(input, size, step, left, right, weightsBig, scalaBoundary)
  }

  def create1DStencilLambda(boundary: BoundaryFun,
                            weights: Array[Float],
                            left: Int, right: Int,
                            size: Int, step: Int): Lambda2 = {
    fun(
      ArrayTypeWSWC(Float, Var("N")),
      ArrayTypeWSWC(Float, weights.length),
      (input, weights) => {
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(makePositive)) o
              ReduceSeqUnroll(fun((acc, y) => {
                multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
              }), 0.0f) $
              Zip(weights, neighbourhood)
          })
        ) o Slide(size, step) o Pad(left, right, boundary) $ input
      }
    )
  }

  def createTiled1DStencilLambda(boundary: BoundaryFun,
                                 weights: Array[Float],
                                 left: Int, right: Int,
                                 size: Int, step: Int,
                                 tileSize: Int, tileStep: Int): Lambda2 = {
    fun(
      ArrayTypeWSWC(Float, Var("N")),
      ArrayTypeWSWC(Float, weights.length),
      (input, weights) => {
        MapWrg(fun(tile =>
          MapLcl(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(makePositive)) o
              ReduceSeqUnroll(fun((acc, y) => {
                multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
              }), 0.0f) $
              Zip(weights, neighbourhood)
          })
        ) o Slide(size, step) o MapLcl(toLocal(id)) $ tile

        )) o Slide(tileSize, tileStep) o Pad(left, right, boundary) $ input
      }
    )
  }

  def createNaiveLocalMemory1DStencilLambda(boundary: BoundaryFun): Lambda2 = {
    fun(
      ArrayTypeWSWC(Float, Var("N")),
      ArrayTypeWSWC(Float, weights.length),
      (input, weights) => {
        MapWrg(MapLcl(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(makePositive)) o
              ReduceSeqUnroll(fun((acc, y) => {
                multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
              }), 0.0f) $
              Zip(weights, toLocal(MapSeqUnroll(id)) $ neighbourhood)
          }))
        ) o Split(2) o Slide(size, step) o Pad(left, right, boundary) $ input
      }
    )
  }

  def apply() = new Stencil1D(
    Seq(
      ("3_POINT_1D_STENCIL_CLAMP", Array[Lambda](create1DStencilLambda(Pad.Boundary.Clamp, weights, 1,1, 3,1))),
      ("3_POINT_1D_STENCIL_MIRROR_UNSAFE", Array[Lambda](create1DStencilLambda(Pad.Boundary.MirrorUnsafe, weights, 1,1, 3,1))),
      ("3_POINT_1D_STENCIL_WRAP", Array[Lambda](create1DStencilLambda(Pad.Boundary.Wrap, weights, 1,1, 3,1))),
      ("3_POINT_1D_STENCIL_MIRROR", Array[Lambda](create1DStencilLambda(Pad.Boundary.Mirror, weights, 1,1, 3,1))),
      ("TILED_3_POINT_1D_WRAP", Array[Lambda](createTiled1DStencilLambda(Pad.Boundary.Wrap, weights, 1,1, 3,1 ,18,16))),
      ("COPY_GROUPS_LOCAL_MEM", Array[Lambda](createNaiveLocalMemory1DStencilLambda(Pad.Boundary.Wrap))),
      ("TILED_9_POINT_1D_WRAP", Array[Lambda](createTiled1DStencilLambda(Pad.Boundary.Wrap, weightsBig, 4,4, 9,1, 4104,4096))),
      ("9_POINT_1D_WRAP", Array[Lambda](create1DStencilLambda(Pad.Boundary.Wrap, weightsBig, 4,4, 9,1)))
    )
  )

  def main(args: Array[String]): Unit = {
    Stencil1D().run(args)
  }
}
