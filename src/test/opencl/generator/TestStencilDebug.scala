package opencl.generator

import apart.arithmetic.Var
import ir.ast._
import opencl.executor._
import opencl.ir.pattern._
import org.junit.{AfterClass, BeforeClass, Test, Ignore}
import opencl.ir._
import ir._
import ir.ast.Pad.BoundaryFun
import org.junit.Assert._

import scala.util.Random

object TestStencilDebug {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

/**
  * Contains tests which include the combined usage of the group
  * and pad pattern.
  * Inherits from TestGroup to avoid code duplication
  */
class TestStencilDebug {
  // Boundary conditions implemented as scala functions for gold versions
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

  /* **********************************************************
      STENCIL TEST SETTINGS
   ***********************************************************/
  val UNROLL = true
  val randomData = Seq.fill(1024)(Random.nextFloat()).toArray
  val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }
  // currently used for 2D stencils / refactor to run with every boundary condition
  val BOUNDARY = Pad.Boundary.Clamp
  val SCALABOUNDARY: (Int, Int) => Int = scalaClamp

  val MapSeqOrMapSeqUnroll = (g: FunDecl) =>
    if (UNROLL)
      MapSeqUnroll(g)
    else
      MapSeq(g)

  def create2DPadGroupLambda(boundary: BoundaryFun, leftHalo: Int, center: Int, rightHalo: Int): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqOrMapSeqUnroll(MapSeqOrMapSeqUnroll(id)) $ neighbours
          ))
        ) o Group2D(leftHalo, center, rightHalo) o Pad2D(Math.max(leftHalo, rightHalo), boundary) $ domain
      }
    )
  }

  def scala2DNeighbours(data: Array[Array[Float]],
                        l1: Int, c1: Int, r1: Int,
                        l2: Int, c2: Int, r2: Int,
                        r: Int,
                        c: Int,
                        boundary: (Int, Int) => Int = SCALABOUNDARY) = {
    /*
    val nrRows = data.length
    val nrColumns = data(0).length

    relRows.flatMap(x => {
      var newR = boundary(r + x, nrRows)

      relColumns.map(y => {
        var newC = boundary(c + y, nrRows)
        data(newR)(newC)
      })
    })
    */
    data.flatten
  }

  def compareGoldWithOutput(gold: Array[Float], output: Array[Float], runtime: Double): Unit = {
    println("runtime = " + runtime)
    //println(output.mkString(", "))
    assertArrayEquals(gold, output, 0.00001f)
  }

  def runCombinedPadGroupTest(leftHalo: Int,
                              center: Int,
                              rightHalo: Int,
                              boundary: BoundaryFun,
                              scalaBoundary: (Int, Int) => Int,
                              data: Array[Array[Float]] = data2D): Unit = {
    val nrRows = data.length
    val nrColumns = data(0).length
    val gold = (0 until nrRows).flatMap(r =>
      (0 until nrColumns).map(c =>
        scala2DNeighbours(data, leftHalo, center, rightHalo, leftHalo, center, rightHalo, r, c, scalaBoundary))
    )

    val lambda = create2DPadGroupLambda(boundary, leftHalo, center, rightHalo)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)
    compareGoldWithOutput(gold.flatten.toArray, output, runtime)
  }

	@Ignore
  @Test def groupClampPaddedData2D() = {
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = scalaClamp

    runCombinedPadGroupTest(1,1,1, boundary, scalaBoundary)
  }

  @Ignore
  @Test def groupMirrorPaddedData2D() = {
    val boundary = Pad.Boundary.Mirror
    val scalaBoundary = scalaMirror

    runCombinedPadGroupTest(1,1,1, boundary, scalaBoundary)
  }

  @Test def groupMirrorUnsafePaddedData2D() = {
    val boundary = Pad.Boundary.MirrorUnsafe
    val scalaBoundary = scalaMirror

    runCombinedPadGroupTest(1,1,1, boundary, scalaBoundary)
  }

	@Ignore
  @Test def groupWrapPaddedData2D() = {
    val boundary = Pad.Boundary.Wrap
    val scalaBoundary = scalaWrap

    runCombinedPadGroupTest(1,1,1, boundary, scalaBoundary)
  }

}
