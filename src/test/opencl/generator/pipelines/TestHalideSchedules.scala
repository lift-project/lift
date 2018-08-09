package opencl.generator.pipelines

import ir._
import ir.ast._
import ir.ast.debug.PrintType
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit._

object TestHalideSchedules extends TestWithExecutor

class TestHalideSchedules {

  @Test
  def singleStagePipelineHalideSchedules(): Unit = {
    val bright = UserFun("bright", "x", "{ return min(x * 1.5f, 255.0f); }", Float, Float)
    val bright4 = UserFun("bright4", "x", "{ return min(x * 1.5f, 255.0f); }", Float4, Float4)

    val M = 4
    val N = 4

    val input = Array.tabulate(M, N) { (i, j) => i * M + j * 1.0f}

    def lambda(l: Lambda): Lambda = {
      λ(ArrayType(ArrayType(Float, N), M),
        image => l $ image
      )
    }

    // initial version
    val brighten = MapSeq(MapSeq(bright))

    // schedule: brighten.reorder(y,x)
    val brighten_col_major = TransposeW() o MapSeq(MapSeq(bright)) o Transpose()

    // schedule: split(x, x_outer, x_inner, 2)
    val brighten_split = MapSeq(Join() o MapSeq(MapSeq(bright)) o Split(2))

    // schedule: fuse(x, y, fused)
    val brighten_fused = MapSeq(bright) o Join()

    //tile(x, y, x_outer, y_outer, x_inner, y_inner, 4, 4)
    // -- or --
    //split(x, x_outer, x_inner, 4);
    //split(y, y_outer, y_inner, 4);
    //reorder(x_inner, y_inner, x_outer, y_outer);
    val brighten_tiled =
      Join() o Map(Map(Join()) o TransposeW()) o      // untile
      MapSeq(MapSeq(MapSeq(MapSeq(bright)))) o        // compute
      Map(Transpose()) o Split(2) o Map(Split(2))     // tile

    // vectorize(x, 4);
    // -- or --
    // split(x, x, x_inner, 4);
    // vectorize(x_inner);
    val brighten_in_vectors = MapSeq(asScalar() o MapSeq(bright4) o asVector(4))

    // split(x, x_outer, x_inner, 2);
    // unroll(x_inner);
    val brighten_unroll = MapSeq(Join() o MapSeq(MapSeqUnroll(bright)) o Split(2))

    // tile(x, y, x_outer, y_outer, x_inner, y_inner, 2, 2)
    // fuse(x_outer, y_outer, tile_index)
    // parallel(tile_index);
    val brighten_fused_tiles =
      Join() o Map(Map(Join()) o TransposeW()) o      // untile
      Split(2) o                                      // INVESTIGATE
      MapGlb(MapSeq(MapSeq(bright))) o                // compute
      Join() o                                        // fuse
      Map(Transpose()) o Split(2) o Map(Split(2))     // tile
    println(Compile(lambda(brighten_fused_tiles)))


    val (outBrighten, _) = Execute(1,1,1,1,(false,false))[Array[Float]](lambda(brighten), input)
    val (outBrightenColMajor, _) = Execute(1,1, 1, 1,(false,false))[Array[Float]](lambda(brighten_col_major), input)
    val (outBrightenSplit, _) = Execute(1,1, 1, 1,(false,false))[Array[Float]](lambda(brighten_split), input)
    val (outBrightenFused, _) = Execute(1,1, 1, 1,(false,false))[Array[Float]](lambda(brighten_fused), input)
    val (outBrightenTiled, _) = Execute(1,1, 1, 1,(false,false))[Array[Float]](lambda(brighten_tiled), input)
    val (outBrightenInVectors, _) = Execute(1,1, 1, 1,(false,false))[Array[Float]](lambda(brighten_in_vectors), input)
    val (outBrightenUnroll, _) = Execute(1,1, 1, 1,(false,false))[Array[Float]](lambda(brighten_unroll), input)
    val (outBrightenFusedTiles, _) = Execute(1,1, 4, 1,(false,false))[Array[Float]](lambda(brighten_fused_tiles), input)

    assertArrayEquals(outBrightenColMajor, outBrighten, 0.0f)
    assertArrayEquals(outBrightenSplit, outBrighten, 0.0f)
    assertArrayEquals(outBrightenFused, outBrighten, 0.0f)
    assertArrayEquals(outBrightenTiled, outBrighten, 0.0f)
    assertArrayEquals(outBrightenInVectors, outBrighten, 0.0f)
    assertArrayEquals(outBrightenUnroll, outBrighten, 0.0f)
    assertArrayEquals(outBrightenFusedTiles, outBrighten, 0.0f)
  }

  @Test
  def halideLesson05Fast(): Unit = {
    val bright = UserFun("bright", "x", "{ return min(x * 1.5f, 255.0f); }", Float, Float)
    val bright4 = UserFun("bright4", "x", "{ return min(x * 1.5f, 255.0f); }", Float4, Float4)
    val M = 128
    val N = 128
    val input = Array.tabulate(M, N) { (i, j) => i * M + j * 1.0f}

    def lambda(l: Lambda): Lambda = {
      λ(ArrayType(ArrayType(Float, N), M),
        image => l $ image
      )
    }

    // [x][y]
    val brighten0 = MapSeq(MapSeq(bright))

    // construct
    // brighten
    //    .tile(x, y, x_outer, y_outer, x_inner, y_inner, 64, 64)                           -> brighten1
    //    .fuse(x_outer, y_outer, tile_index)                                               -> brighten2
    //    .parallel(tile_index);                                                            -> brighten3
    //    .tile(x_inner, y_inner, x_inner_outer, y_inner_outer, x_vectors, y_pairs, 4, 2)   -> brighten4
    //    .vectorize(x_vectors)                                                             -> brighten5
    //    .unroll(y_pairs);                                                                 -> brighten6

    // [x_inner][y_inner][x_outer][y_outer]
    val brighten1 =
      Join() o Map(Map(Join()) o TransposeW()) o     // REVERSE(tile)
      MapSeq(MapSeq(MapSeq(MapSeq(bright)))) o       // add two maps
      Map(Transpose()) o Split(64) o Map(Split(64))  // tile(x, y, x_outer, y_outer, x_inner, y_inner, 64, 64)

    // [x_inner][y_inner][tile_index]
    val brighten2 =
      Join() o Map(Map(Join()) o TransposeW()) o
      Split(2) o                                     // REVERSE(fuse) -- find out how many?!
      MapSeq(MapSeq(MapSeq(bright))) o               // remove one map
      Join() o                                       // fuse(x_outer, y_outer, tile_index)
      Map(Transpose()) o Split(64) o Map(Split(64))

    // [x_inner][y_inner][tile_index]
    val brighten3 =
      Join() o Map(Map(Join()) o TransposeW()) o
      Split(2) o
      MapGlb(MapSeq(MapSeq(bright))) o               // parallel(tile_index)
      Join() o
      Map(Transpose()) o Split(64) o Map(Split(64))

    // [x_vectors][y_pairs][x_inner_outer][y_inner_outer][tile_index]
    val brighten4 =
      Join() o Map(Map(Join()) o TransposeW()) o
      Split(2) o
      MapGlb(
        Join() o Map(Map(Join()) o TransposeW()) o PrintType() o    // REVERSE(tile)
        MapSeq(MapSeq(MapSeq(MapSeq(bright)))) o                    // add two maps
        Map(Transpose()) o Split(2) o MapSeq(Split(4))              // tile(x_inner, y_inner, x_inner_outer, y_inner_outer, x_vectors, y_pairs, 4, 2)
      ) o
      Join() o
      Map(Transpose()) o Split(64) o Map(Split(64))

    // [x_vectors][y_pairs][x_inner_outer][y_inner_outer][tile_index]
    val brighten5 =
      Join() o Map(Map(Join()) o TransposeW()) o
      Split(2) o
      MapGlb(
        Join() o Map(Map(Join()) o TransposeW()) o
        MapSeq(MapSeq(Split(4) o asScalar() o MapSeq(bright4))) o   // REVERSE(vectorize) = add asScalar + reconstruct array from vector using split
        Map(Transpose()) o Split(2) o MapSeq(asVector(4))           // vectorize(x_vectors) = use vectorized UserFun + remove one map
      ) o
      Join() o
      Map(Transpose()) o Split(64) o Map(Split(64))

    // [x_vectors][y_pairs][x_inner_outer][y_inner_outer][tile_index]
    val brighten6 =
      Join() o Map(Map(Join()) o TransposeW()) o
      Split(2) o
      MapGlb(
        Join() o Map(Map(Join()) o TransposeW()) o
        MapSeq(MapSeq(Split(4) o asScalar() o MapSeqUnroll(bright4))) o   // unroll(y_pairs)
        Map(Transpose()) o Split(2) o MapSeq(asVector(4))
      ) o
      Join() o
      Map(Transpose()) o Split(64) o Map(Split(64))


    val (outBrighten0, _) = Execute(1,1,1,1,(false,false))[Array[Float]](lambda(brighten0), input)
    val (outBrighten1, _) = Execute(1,1,1,1,(false,false))[Array[Float]](lambda(brighten1), input)
    val (outBrighten2, _) = Execute(1,1,1,1,(false,false))[Array[Float]](lambda(brighten2), input)
    val (outBrighten3, _) = Execute(1,1,32,1,(false,false))[Array[Float]](lambda(brighten3), input)
    val (outBrighten4, _) = Execute(1,1,32,1,(false,false))[Array[Float]](lambda(brighten4), input)
    val (outBrighten5, _) = Execute(1,1,32,1,(false,false))[Array[Float]](lambda(brighten5), input)
    val (outBrighten6, _) = Execute(1,1,32,1,(false,false))[Array[Float]](lambda(brighten6), input)

    assertArrayEquals(outBrighten1, outBrighten0, 0.0f)
    assertArrayEquals(outBrighten2, outBrighten0, 0.0f)
    assertArrayEquals(outBrighten3, outBrighten0, 0.0f)
    assertArrayEquals(outBrighten4, outBrighten0, 0.0f)
    assertArrayEquals(outBrighten5, outBrighten0, 0.0f)
    assertArrayEquals(outBrighten6, outBrighten0, 0.0f)
  }
}
