package opencl.generator

import java.io.FileInputStream
import java.util.Scanner

import ir.ArrayType
import ir.ast._
import lift.arithmetic.{ArithExpr, Cst, Lookup, SizeVar}
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.{Ignore, Test}

import scala.util.Random

object TestSlide extends TestWithExecutor

class TestSlide {

  val UNROLL = true
  val data = Array.tabulate(5)(_ * 1.0f)
  val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }

  val ReduceSeqOrReduceSeqUnroll = (g: FunDecl, init: Expr) =>
    if (UNROLL)
      ReduceSeqUnroll(g, init)
    else
      ReduceSeq(g, init)

  val MapSeqOrMapSeqUnroll = (g: FunDecl) =>
    if (UNROLL)
      MapSeqUnroll(g)
    else
      MapSeq(g)

  val gaussWeights = Array(
      0.08f, 0.12f, 0.08f,
      0.12f, 0.20f, 0.12f,
      0.08f, 0.12f, 0.08f)
  val sobelWeights = Array(
      -1, -2, -1,
       0,  0,  0,
       1,  2,  1).map(_.toFloat)

  // todo eventually move to select primitive
  @Test def lookupSimplfication(): Unit = {
    val table: Seq[ArithExpr] = Array(0,1,2).map(Cst(_))
    val lookup = Lookup(table, Cst(1), 0)
    assertEquals(table.apply(1), lookup)
  }

  def scalaSlide2D(input: Array[Array[Float]],
                   size1: Int, step1: Int,
                   size2: Int, step2: Int) = {
    val firstSlide = input.sliding(size1, step1).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(size2, step2).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    neighbours
  }

  def createSimple1DGroupLambda(size: Int, step: Int): Lambda1 = {
    fun(
      ArrayType(Float, SizeVar("N")),
      (domain) => MapGlb(MapSeqOrMapSeqUnroll(id)) o Slide(size, step) $ domain
    )
  }

  def createSimple2DGroupLambda(size: Int, step: Int): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqOrMapSeqUnroll(MapSeqOrMapSeqUnroll(id)) $ neighbours
          ))
        ) o Slide2D(size, step) $ domain
      }
    )
  }

  def createAsymmetric2DGroupLambda(size1: Int, step1: Int,
                                    size2: Int, step2: Int): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeq(MapSeqOrMapSeqUnroll(id)) $ neighbours
          ))
        ) o Slide2D(size1, step1, size2, step2) $ domain
      }
    )
  }

  /* // Different Syntax
  def createSimple2DGroupLambda2(indices: Array[Int]): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      (domain) => {
        domain :>> Group2D(indices) :>> MapGlb(1)(
          MapGlb(0)(
             MapSeq(
               MapSeq(id)
             )
          )
        )
      }
    )
  }
  */

  def createGroups1D(lambda: Lambda1, data: Array[Float]): (Array[Float], Double) = {
    val (output, runtime) = Execute(data.length, data.length)[Array[Float]](lambda, data)
    (output, runtime)
  }

  def createGroup2D(lambda: Lambda1, data2D: Array[Array[Float]]): (Array[Float], Double) = {
    val (output, runtime) = Execute(data2D.length, data2D.length)[Array[Float]](lambda, data2D)
    (output, runtime)
  }

  def compareGoldWithOutput(gold: Array[Float], output: Array[Float], runtime: Double): Unit = {
    println("runtime = " + runtime)
    //println(output.mkString(", "))
    assertArrayEquals(gold, output, 0.2f)
  }

  /* **********************************************************
      SLIDE 1D
   ***********************************************************/
  @Test def groupLeftCurrentRight(): Unit = {
    val gold = Array(0,1,2, 1,2,3, 2,3,4).map(_.toFloat)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(3,1), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def slideWithGap(): Unit = {
    val data = Array(0,1,2,3,4,5).map(_.toFloat)
    val gold = data.sliding(2,4).toArray.flatten
    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(2,4), data)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def slideWithBiggerGap(): Unit = {
    val data = Array(0,1,2,3,4,5,6,7,8,9,10,11).map(_.toFloat)
    val gold = data.sliding(2,4).toArray.flatten
    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(2,4), data)
    println(output.mkString(","))
    compareGoldWithOutput(gold, output, runtime)
  }

   @Test def splitEquivalent(): Unit = {
     val data = Array(0,1,2,3,4,5).map(_.toFloat)
      val gold = data.sliding(3,3).toArray.flatten
      val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(3,3), data)
     compareGoldWithOutput(gold, output, runtime)
  }

  @Test def slideWidth4Step2(): Unit = {
    val gold = Array(0,1,2,3, 2,3,4,5).map(_.toFloat)
    val data = Array(0,1,2,3,4,5).map(_.toFloat)

    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(4,2), data)
    println(output.mkString(","))
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def slideIdentity(): Unit = {
    val (output: Array[Float], runtime: Double) = createGroups1D(createSimple1DGroupLambda(1,1), data)
    compareGoldWithOutput(data, output, runtime)
  }

  /* **********************************************************
      GROUP 2D
   ***********************************************************/
  @Test def slide2DIdentity(): Unit = {
    val (output: Array[Float], runtime: Double) = createGroup2D(createSimple2DGroupLambda(1,1), data2D)
    compareGoldWithOutput(data2D.flatten, output, runtime)
  }

  @Test def slide2DIdentityTile(): Unit = {
    val (output: Array[Float], runtime: Double) = createGroup2D(createSimple2DGroupLambda(4,2), data2D)
    compareGoldWithOutput(data2D.flatten, output, runtime)
  }

  @Test def group9PointNeighbourhoodIdentity(): Unit = {
    val data2D = Array.tabulate(3, 3) { (i, j) => i * 3.0f + j }
    val (output: Array[Float], runtime: Double) = createGroup2D(createSimple2DGroupLambda(3,1), data2D)
    compareGoldWithOutput(data2D.flatten, output, runtime)
  }

  @Ignore // output too big
  @Test def validTileSize(): Unit = {
    val tileSize = 16
    val tileStep = 14
    val data2D = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val gold = scalaSlide2D(data2D, tileSize,tileStep,tileSize,tileStep)
    val (output: Array[Float], runtime: Double) = createGroup2D(createSimple2DGroupLambda(tileSize,tileStep), data2D)

    compareGoldWithOutput(gold.flatten.flatten.flatten, output, runtime)
  }

  /**
    * Sliding a 2D data structure results in a 4D data structure.
    * Creating a 9 Point Neighbourhood of a 4x4 Array results in a
    * 2x2x3x3 Array. Each gold version below corresponds to a
    * Neighbourhood of one element of the input array
    */
  @Test def group9PointNeighbourhood(): Unit = {
    /* */
    val goldTopLeft = Array.tabulate(3, 3) { (i, j) => i * 3.0f + j + i }.flatten
    val goldTopRight = Array.tabulate(3, 3) { (i, j) => i * 3.0f + 1 + j + i }.flatten
    val goldBottomLeft = goldTopLeft.map(_ + 4) // Top arrays are already flat
    val goldBottomRight = goldTopRight.map(_ + 4)
    val gold = goldTopLeft ++ goldTopRight ++ goldBottomLeft ++ goldBottomRight

    val (output: Array[Float], runtime: Double) = createGroup2D(createSimple2DGroupLambda(3,1), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }

  /**
    * Applies a 1D group to a 2D data structure.
    * Grouping left and right neighbours
    */
  @Test def group2DRelColsOnly(): Unit = {
    val gold = Array(0, 1, 2,
      1, 2, 3,
      4, 5, 6,
      5, 6, 7,
      8, 9, 10,
      9, 10, 11,
      12, 13, 14,
      13, 14, 15).map(_.toFloat)

    val (output: Array[Float], runtime: Double) = createGroup2D(createAsymmetric2DGroupLambda(1,1, 3,1), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }

  /**
    * Applyies a 1D group to a 2D data structure.
    * Group top and bottom neighbours
    */
  @Test def group2DRelRowsOnly(): Unit = {
    val gold = Array(0, 4, 8,
      1, 5, 9,
      2, 6, 10,
      3, 7, 11,
      4, 8, 12,
      5, 9, 13,
      6, 10, 14,
      7, 11, 15).map(_.toFloat)

    val (output: Array[Float], runtime: Double) = createGroup2D(createAsymmetric2DGroupLambda(3,1, 1,1), data2D)
    compareGoldWithOutput(gold, output, runtime)
  }

  @Test def group16PointTiles(): Unit = {
    val data2D = Array.tabulate(6, 6) { (i, j) => i * 6.0f + j }
    val gold = Array(0,1,2,3,
      6,7,8,9,
      12,13,14,15,
      18,19,20,21,
      2,3,4,5,
      8,9,10,11,
      14,15,16,17,
      20,21,22,23,
      12,13,14,15,
      18,19,20,21,
      24,25,26,27,
      30,31,32,33,
      14,15,16,17,
      20,21,22,23,
      26,27,28,29,
      32,33,34,35).map(_.toFloat)
    val (output: Array[Float], runtime: Double) = createGroup2D(createSimple2DGroupLambda(4,2), data2D)
    println(data2D.flatten.mkString(","))
    println(output.mkString(","))
    compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
   2D STENCILS WITHOUT PADDING
  ***********************************************************/
  val outputLocation = "../../../../../../Downloads/pgm"
  val lenaPGM = "../../../../../../Downloads/pgm/lena.ascii.pgm"

  /** ensures that resulting pixel value will be greater than 0 */
  val clamp = UserFun("my_clamp", "i", "{ return (i < 0) ? 0 : i;  }",
    Float, Float)

  /**
    * saves an array of floats as grayscale ascii .pgm image
    *
    * @param name     name of result file
    * @param location specifies where image should be stored
    * @param img      array of pixels
    */
  def savePGM(name: String, location: String, img: Array[Array[Float]]): Unit = {
    val out = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(location, name)))
    out.write(
      s"""|P2
          |${img.length} ${img.head.length}
          |255
          |${img.map(_.map(x => x /* * 255.0f*/.toInt).mkString("\n")).mkString("\n")}
      """.stripMargin)
    out.close()
  }

  /**
    * Stores the pixel values of the input image in an array of floats
    *
    * @param name name and path of the input picture
    * @return array of floats containing pixel values
    */
  def readInputImage(name: String): (Int, Int, Array[Array[Float]]) = {
    val in = new FileInputStream(lenaPGM)
    val scanner = new Scanner(in, "ASCII")
    scanner.useDelimiter("""\s+#.+\s+|\s+""".r.pattern)
    scanner.nextLine()
    val width = scanner.nextInt()
    val height = scanner.nextInt()
//    val max = scanner.nextInt()

    val input = Array.tabulate(width, height)((r, c) => scanner.nextInt()).map(_.map(_/*/ max*/.toFloat))
    scanner.close()
    (width, height, input)
  }

  def createSimpleStencilWithoutPad(size: Int, step: Int, weights: Array[Float]): Lambda2 = {
    fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      ArrayType(Float, weights.length),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeq(clamp)) o
              ReduceSeqOrReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(size, step) $ matrix
      })
  }

  def runSimple2DStencilWithoutPadding(size: Int, step: Int, weights: Array[Float], name: String): Unit = {
    try {
      //val (width, height, input) = readInputImage(lenaPGM)
      val randomData2D = Array.tabulate(1024, 1024) { (i, j) => Random.nextFloat() }
      val width = randomData2D(0).length
      val height = randomData2D.length

      val f = createSimpleStencilWithoutPad(size, step, weights)

      val (_, runtime) = Execute(1, 1, width, height, (false, false))[Array[Float]](f, randomData2D, weights)
      println("Runtime: " + runtime)

      // val outOfBoundElementsX = size + (size - step) / 2 //todo only true if symmetric padding! check this
      // savePGM(name, outputLocation, output.grouped(width - outOfBoundElementsX).toArray)

    } catch {
      case x: Exception => x.printStackTrace()
    }
  }

  /**
    * Computation of gaussian blur using a .pgm file
    * Uses no padding which causes the output picture to be smaller than the input
    *
    * Used to actually produce images
    */
  @Test def gaussianBlurNoPad(): Unit = {
    runSimple2DStencilWithoutPadding(3,1, gaussWeights, "gaussNoPad.pgm")
  }

  /* **********************************************************
   ITERATIVE SLIDE
  ***********************************************************/
  @Test def iterativeSlide(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val data = Array(0,1,2,3,4,5).map(_.toFloat)
    val gold = Array(18,27).map(_.toFloat)
    val lambda = fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        Iterate(2) (Join() o MapGlb(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Slide(3,1)) $ input
      })

    val (output, runtime) = Execute(data.length, data.length)[Array[Float]](lambda, data)
    compareGoldWithOutput(gold, output, runtime)
  }

  /* **********************************************************
   SLIDE 3D
  ***********************************************************/
  @Test def slide3D(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val size = 2
    val step = 1
    val N = SizeVar("N")
    val b = Pad.Boundary.Clamp
    val fct = fun(
      //ArrayType(ArrayType(ArrayType(Float, N), N), N),
      ArrayType(ArrayType(ArrayType(Float, 3), 3), 3),
      (input) => MapGlb(0)(MapGlb(1)(MapGlb(2)(
        //MapSeq(MapSeq(MapSeq(id)))))) o
        toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join() o Join()))) o
        // another slide3d
      Slide3D(size, step) $ input
        /*
        Map(Map(Transpose()) o Transpose()) o
        Slide(size, step) o
        Map(Map(Transpose()) o Slide(size, step) o Map(Slide(size, step))) $ input
        */
        // toomas slide3d
        /*
        Map(Map(Transpose()) o Transpose() o
          Map(
            Map(Transpose() o Map(Slide(size, step))
            ) o Slide(size, step))
        ) o Slide(size, step) $ input
        */
        /*
        Transpose() o Map(Transpose()) o Transpose() o
        Map(Map(Slide(2,1))) o Map(Transpose()) o Transpose() o
        Map(Slide2D(2,1)) $ input
        */
    )

    val input3D = Array.tabulate(3, 3, 3) { (i, j, k) => i * 9.0f + j * 3.0f + k}
    val test: Array[Array[Array[Array[Array[Array[Float]]]]]] = input3D.map(_.sliding(2,1).toArray.map(_.transpose.sliding(2,1).toArray).map(_.transpose)).transpose.map(_.transpose).map(_.map(_.sliding(2,1).toArray)).transpose
    val nbh= test.map(_.map(_.map(_.flatten.flatten.foldLeft(0.0f)((acc, p) => acc + p))))
    val gold = Array(52,60,76,84,124,132,148,156).map(_.toFloat)
    //val first = nbh.head.head.head
    val (output,runtime) = Execute(2,2,2,2,2,2,(true,true))[Array[Float]](fct, input3D)
    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.0f)
    //println("lift:  " + output.map(_.toInt).map(_+97).map(_.asInstanceOf[Char]).mkString(","))
    println("scala: " + test.flatten.flatten.flatten.flatten.flatten.map(_.toInt).map(_+97).map(_.asInstanceOf[Char]).mkString(","))
    println(output.map(_.toInt).mkString(","))
  }

  @Test def slideND(): Unit = {
    LongTestsEnabled()

    val n = 3
    val s = 1
    val input3D = Array.tabulate(34, 34, 34) { (_, _, _) => scala.util.Random.nextFloat() }
    val input2D = Array.tabulate(34, 34) { (_, _) => scala.util.Random.nextFloat() }

    val applyId3D = MapGlb(2)(MapGlb(1)(MapGlb(0)(MapSeq(MapSeq(MapSeq(id))))))
    val applyId2D = MapGlb(1)(MapGlb(0)(MapSeq(MapSeq(id))))
    def lambda2D(f: Lambda) = {
      fun(
        ArrayType(ArrayType(Float,SizeVar("M")), SizeVar("N")),
        input => applyId2D o f $ input
      )
    }
    def lambda3D(f: Lambda) = {
      fun(
        ArrayType(ArrayType(ArrayType(Float,SizeVar("M")), SizeVar("N")), SizeVar("O")),
        input => applyId3D o f $ input
      )
    }

    ////////// 2D
    val generated2D = SlideND(2)(n,s)
    val handwritten2D = Map(Transpose()) o Slide(n,s) o Map(Slide(n,s))

    val (outGold2D, _) = Execute(1,1,32,32,(false,false))[Array[Float]](lambda2D(handwritten2D), input2D)
    val (outGenerated2D, _) = Execute(1,1,32,32,(false,false))[Array[Float]](lambda2D(generated2D), input2D)

    assertArrayEquals(outGold2D, outGenerated2D, 0.1f)

    ////////// 3D
    val generated3D = SlideND(3)(n,s)
    val handwritten3D = Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Map(Transpose()))) o Slide(n,s) o Map(Slide(n,s)) o Map(Map(Slide(n,s)))

    val (outGold, _) = Execute(1,1,1,32,32,32,(false,false))[Array[Float]](lambda3D(handwritten3D), input3D)
    val (outGenerated, _) = Execute(1,1,1,32,32,32,(false,false))[Array[Float]](lambda3D(generated3D), input3D)

    assertArrayEquals(outGold, outGenerated, 0.1f)
  }

  @Test def tiledSlideND1(): Unit = {
    val n = 3
    val s = 1
    val tileStep = 4 // how many neighborhoods do we want to have in a tile

    ////////// 1D
    val input1D = Array.tabulate(34) { _ => scala.util.Random.nextFloat() }

    val slide1D = SlideND(1)(n, s)
    val tiledSlide1D = TiledSlidedND(1)(n, s, tileStep)

    val applyId1D = MapGlb(0)(MapSeq(id))

    def lambda1D(f: Lambda) = {
      fun(
        ArrayType(Float, SizeVar("N")),
        input => applyId1D o f $ input
      )
    }

    val (outGold1D, _) = Execute(1, 32, (false, false))[Array[Float]](lambda1D(slide1D), input1D)
    val (outTiled1D, _) = Execute(1, 32, (false, false))[Array[Float]](lambda1D(tiledSlide1D), input1D)

    assertArrayEquals(outGold1D, outTiled1D, 0.1f)
  }

  @Test def tiledSlideND2(): Unit = {
    val n = 3
    val s = 1
    val tileStep = 4 // how many neighborhoods do we want to have in a tile

    ////////// 2D
    val input2D = Array.tabulate(34, 34) { (_, _) => scala.util.Random.nextFloat() }

    val slide2D = SlideND(2)(n, s)
    val tiledSlide2D = TiledSlidedND(2)(n, s, tileStep)

    val applyId2D = MapGlb(1)(MapGlb(0)(MapSeq(MapSeq(id))))

    def lambda2D(f: Lambda) = {
      fun(
        ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
        input => applyId2D o f $ input
      )
    }

    val (outGold2D, _) = Execute(1, 1, 32, 32, (false, false))[Array[Float]](lambda2D(slide2D), input2D)
    val (outTiled2D, _) = Execute(1, 1, 32, 32, (false, false))[Array[Float]](lambda2D(tiledSlide2D), input2D)

    assertArrayEquals(outGold2D, outTiled2D, 0.1f)
  }

  @Test def tiledSlideND3(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)
    LongTestsEnabled()

    val n = 3
    val s = 1
    val tileStep = 4 // how many neighborhoods do we want to have in a tile

    ////////// 3D
    val input3D = Array.tabulate(34, 34, 34) { (_, _, _) => scala.util.Random.nextFloat() }

    val slide3D = SlideND(3)(n, s)
    val tiledSlide3D = TiledSlidedND(3)(n, s, tileStep)

    val applyId3D = MapWrg(2)(MapWrg(1)(MapWrg(0)(MapLcl(2)(MapLcl(1)(MapLcl(0)(id))))))

    def lambda3D(f: Lambda) = {
      fun(
        ArrayType(ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")), SizeVar("O")),
        input => applyId3D o f $ input
      )
    }

    val (outGold3D, _) = Execute(4, 4, 4, 32, 32, 32, (true, true))[Array[Float]](lambda3D(slide3D), input3D)
    val (outTiled3D, _) = Execute(4, 4, 4, 32, 32, 32, (true, true))[Array[Float]](lambda3D(tiledSlide3D), input3D)

    assertArrayEquals(outGold3D, outTiled3D, 0.1f)
  }

  @Ignore //see Issue #97
  @Test def tiledSlideND4(): Unit = {
    LongTestsEnabled()

    val n = 3
    val s = 1
    val tileStep = 4 // how many neighborhoods do we want to have in a tile

    ////////// 4D
    val input4D = Array.tabulate(10, 10, 10, 10) { (_, _, _, _) => scala.util.Random.nextFloat() }

    val slide4D = SlideND(4)(n,s)
    val tiledSlide4D = TiledSlidedND(4)(n,s,tileStep)

    //val applyId4D = MapGlb(2)(MapGlb(1)(MapGlb(0)(MapSeq(MapSeq(MapSeq(MapSeq(MapSeq(id))))))))
    val applyId4D = MapSeq(MapWrg(2)(MapWrg(1)(MapWrg(0)(MapSeq(MapLcl(2)(MapLcl(1)(MapLcl(0)(id))))))))
    def lambda4D(f: Lambda) = {
      fun(
        ArrayType(ArrayType(ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")), SizeVar("O")), SizeVar("P")),
        input => applyId4D o f $ input
      )
    }

    val (outGold4D, _) = Execute(4,4,4,8,8,8,(true,true))[Array[Float]](lambda4D(slide4D), input4D)
    val (outTiled4D, _) = Execute(4,4,4,8,8,8,(true,true))[Array[Float]](lambda4D(tiledSlide4D), input4D)

    assertArrayEquals(outGold4D, outTiled4D, 0.1f)
  }
}
