package opencl.generator.stencil

import ir.{ArrayType}
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.{BeforeClass, _}

object TestStencil3D {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestStencil3D{

  @Test def slide3DInTermsOfSlide2D(): Unit = {
    LongTestsEnabled()
    val n = 3
    val s = 1
    val input = Array.tabulate(34, 34, 34) { (_, _, _) => scala.util.Random.nextFloat() }

    val applyId = MapGlb(2)(MapGlb(1)(MapGlb(0)(MapSeq(MapSeq(MapSeq(id))))))
    def lambda(f: Lambda) = {
      fun(
        ArrayType(ArrayType(ArrayType(Float,SizeVar("M")), SizeVar("N")), SizeVar("O")),
        input => applyId o f $ input
      )
    }

    val slide3DOriginal = Slide3D(n,s)
    //                  = Map(Map(Transpose())) o Map(Transpose()) o Map(Map(Map(Transpose()))) o
    //                      Slide(n,s) o Map(Slide(n,s)) o Map(Map(Slide(n,s)))

    // use slide2d for XY
    val slide3DAlternative1 = Map(Map(Transpose())) o Map(Map(Map(Transpose()))) o
      Slide2D(n, s) o Map(Map(Slide(n,s)))

    // use slide2d for YZ
    val slide3DAlternative2 = Map(Map(Transpose())) o Map(Transpose()) o Slide(n,s) o Map(Slide2D(n,s))

    // use slide2d for YZ written differently
    val slide3DAlternative3 = Map(Map(Transpose())) o Map(Transpose()) o
      Map(Map(Slide2D(n,s))) o Slide(n,s)

    val (outGold: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(slide3DOriginal), input)
    val (outAlternative1: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(slide3DAlternative1), input)
    val (outAlternative2: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(slide3DAlternative2), input)
    val (outAlternative3: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(slide3DAlternative3), input)

    assertArrayEquals(outGold, outAlternative1, 0.1f)
    assertArrayEquals(outGold, outAlternative2, 0.1f)
    assertArrayEquals(outGold, outAlternative3, 0.1f)
  }

  @Test def tile2DInA3DStencil(): Unit = {
    LongTestsEnabled()
    // REMARK: Final dimensions need to be corrected -> stencil function cannot be applied to rewritten expressions
    val n = 3 // stencil parameters
    val s = 1
    val u = 4 // tile parameters
    val v = 2
    val input = Array.tabulate(34, 34, 34) { (_, _, _) => scala.util.Random.nextFloat() }

    ////////// XY SLICING
    // Slide2D for x and y dimension
    val goldXY = Map(Map(Transpose())) o Map(Map(Map(Transpose()))) o
        Slide2D(n, s) o  // <- replace this one with 2D tiling
      Map(Map(Slide(n,s)))

    val tileXY = Map(Map(Transpose())) o Map(Map(Map(Transpose()))) o
    //  |   This removes the tiling directly  |
    //  v    => move this to the end...       v
        Map(Join()) o Join() o Map(Transpose()) o Map(Map(Slide2D(n,s))) o Slide2D(u,v) o // <- tiled slide2D
      Map(Map(Slide(n,s)))

    val usefulTileXY = Map(Map(TransposeW())) o Map(Map(Map(TransposeW()))) o Map(Join()) o Join() o Map(TransposeW()) o
      MapWrg(2)(MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(MapSeq(MapSeq(MapSeq(id)))))))) o
        Map(Map(Slide2D(n,s))) o Slide2D(u,v) o Map(Map(Slide(n,s)))

    ////////// YZ SLICING
    val goldYZ = Map(Map(Transpose())) o Map(Transpose()) o Slide(n,s) o Map(Slide2D(n,s))

    val tileYZ = Map(Map(Transpose())) o Map(Transpose()) o Slide(n,s) o Map(
      Map(Join()) o Join() o Map(Transpose()) o Map(Map(Slide2D(n,s))) o Slide2D(u,v)
    )

    val tileYZFissioned = Map(Map(Transpose())) o Map(Transpose()) o Slide(n,s) o
      Map(Map(Join())) o Map(Join()) o Map(Map(Transpose())) o Map(Map(Map(Slide2D(n,s)))) o Map(Slide2D(u,v))

    val usfulTileYZFissioned = Map(Map(Transpose())) o Map(Transpose()) o Slide(n,s) o
      Map(Map(Join())) o Map(Join()) o Map(Map(Transpose())) o Map(Map(Map(Slide2D(n,s)))) o Map(Slide2D(u,v))

    ///////// YZ SLICING USING OTHER SLIDE3D
    val goldYZ2 = Map(Map(Transpose())) o Map(Transpose()) o
      Map(Map(Slide2D(n,s))) o Slide(n,s)

    val tileYZ2 = Map(Map(Transpose())) o Map(Transpose()) o
        Map(Map(Map(Join()))) o Map(Map(Join())) o Map(Map(Map(Transpose()))) o
          Map(Map(Map(Map(Slide2D(n,s))))) o Map(Map(Slide2D(u,v))) o Slide(n,s)

    val usefulTileYZ2 = Map(Map(TransposeW())) o Map(TransposeW()) o
        Map(Map(Map(Join()))) o Map(Map(Join())) o Map(Map(Map(TransposeW()))) o
      //                  sequentially compute z-dimension
      MapWrg(1)(MapWrg(0)(MapSeq(MapLcl(1)(MapLcl(0)(MapSeq(MapSeq(MapSeq(id)))))))) o
      Map(Map(Map(Map(Slide2D(n,s))))) o Map(Map(Slide2D(u,v))) o Slide(n,s)

    val applyId = MapGlb(2)(MapGlb(1)(MapGlb(0)(MapSeq(MapSeq(MapSeq(id))))))
    def lambda(f: Lambda) = {
      fun(
        //ArrayType(ArrayType(ArrayType(Float,SizeVar("M")), SizeVar("N")), SizeVar("O")),
        ArrayType(ArrayType(ArrayType(Float,34), 34), 34),
        input => f $ input
      )
    }

    val (outGold: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(applyId o goldXY), input)
    val (tiledXY: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(applyId o tileXY), input)
    val (tiledXY1: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(usefulTileXY), input)

    val (outGoldYZ: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(applyId o goldYZ), input)
    val (tiledYZ: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(applyId o tileYZ), input)
    val (tiledYZFissioned: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(applyId o tileYZFissioned), input)

    val (outGoldYZ2: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(applyId o goldYZ2), input)
    val (tiledYZ2: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(applyId o tileYZ2), input)
    val (tiledYZ21: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(usefulTileYZ2), input)

    assertArrayEquals(outGold, tiledXY, 0.1f)
    assertArrayEquals(outGold, tiledXY1, 0.1f)

    assertArrayEquals(outGold, outGoldYZ, 0.1f)
    assertArrayEquals(outGold, tiledYZ, 0.1f)
    assertArrayEquals(outGold, tiledYZFissioned, 0.1f)

    assertArrayEquals(outGold, outGoldYZ2, 0.1f)
    assertArrayEquals(outGold, tiledYZ2, 0.1f)
    assertArrayEquals(outGold, tiledYZ21, 0.1f)
  }

  @Test def sliceXYPlane(): Unit = {
    val n = 3 // stencil parameters
    val s = 1
    val u = 6 // tile parameters
    val v = 4
    val input = Array.tabulate(34, 34, 34) { (_, _, _) => scala.util.Random.nextFloat() }

    //val f = MapSeq(MapSeq(MapSeq(id))) // stencil function
    val f = toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join() o Join() // stencil: sum up all elements
    def applyStencil(f: Lambda) = MapGlb(2)(MapGlb(1)(MapGlb(0)(f)))

    def gold = applyStencil(f) o Slide3D(n,s)

    // use def to differentiate auxiliary functions from full expressions
    val fixDims = Map(Map(Transpose())) o Map(Map(Map(Transpose())))
    val slideZ = Map(Map(Slide(n,s)))
    def useSlide2D = applyStencil(f) o fixDims o Slide2D(n, s) o slideZ

    val undoTiling = Map(Join()) o Join() o Map(Transpose())
    val createTiles = Slide2D(u,v)
    val createNbhsInTiles = Map(Map(Slide2D(n,s)))
    def tileXY = applyStencil(f) o fixDims o undoTiling o createNbhsInTiles o createTiles o slideZ

    // swap fixDims and undoTiling
    val rest = createNbhsInTiles o createTiles o slideZ
    def afterSwapping = applyStencil(f) o undoTiling o Map(Map(fixDims)) o rest

    val undoTilingW = Map(Join()) o Join() o Map(TransposeW())
    val newFixDims = Map(Map(Map(Map(Transpose())))) o Map(Map(Map(Map(Map(Transpose())))))
    def promoteMaps = undoTilingW o MapSeq(MapSeq(applyStencil(f))) o newFixDims o rest

    val compute = MapSeq(MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)))))
    val pushZ = Transpose() o Map(Transpose()) o Map(Map(Transpose())) o Map(Map(Map(Transpose())))
    val pullZ = Map(Map(Map(TransposeW()))) o Map(Map(TransposeW())) o Map(TransposeW()) o TransposeW()
    def promoteZ = undoTilingW o pullZ o compute o pushZ o newFixDims o rest

    def lambda(f: Lambda) = {
      fun(
        //ArrayType(ArrayType(ArrayType(Float,SizeVar("M")), SizeVar("N")), SizeVar("O")),
        ArrayType(ArrayType(ArrayType(Float,34), 34), 34),
        input => f $ input
      )
    }

    val (outGold: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(gold), input)
    val (outUseSlide2D: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(useSlide2D), input)
    val (outTileXY: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(tileXY), input)
    val (outAfterSwapping: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(afterSwapping), input)
    val (outPromoteMaps: Array[Float], _) = Execute(1,1,1,32,32,32,(false,false))(lambda(promoteMaps), input)
    val (outPromoteZ: Array[Float], _) = Execute(4,4,32,32,(true,true))(lambda(promoteZ), input)

    assertArrayEquals(outGold, outUseSlide2D, 0.1f)
    assertArrayEquals(outGold, outTileXY, 0.1f)
    assertArrayEquals(outGold, outAfterSwapping, 0.1f)
    assertArrayEquals(outGold, outPromoteMaps, 0.1f)
    assertArrayEquals(outGold, outPromoteZ, 0.1f)
  }
}
