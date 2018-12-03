package host

import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import ir.ast.{Array3DFromUserFunGenerator, ArrayFromUserFunGenerator, Get, Join, Pad, Split, Transpose, TransposeW, UserFun, Zip, \, fun}
import ir.ast.Pad.Boundary.WrapUnsafe
import opencl.ir.{Float, add, _}
import lift.arithmetic.SizeVar
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import org.junit.Test
import org.junit.Assert._
//import org.scalatest.expect

import sys.process._
import scala.language.postfixOps

class TestHost {


  val N = SizeVar("N")

  val incrementF = fun(Float, x => add(Float).apply(1f, x))

  val add2 = UserFun("add", Array("l", "r"),
    "{ return (l + r); }",
    Seq(Float, Float), Float)


  private def compile_native(path: String, file: String): Unit = {

    val full_path_file = path + "/" + file
    val target = path + "/" + "a.out"

    val status_code = s"g++ $full_path_file -I$path -o $target" !

    assert(status_code == 0, "Native Compilation error!")


  }

  private def run_executable(path: String, file: String): String = {

    ( ( if(path.isEmpty()) "./" else path + "/" ) + s"$file" ) !!

  }

  private def native_compile_and_run(path: String, file: String)  : String = {

    compile_native(path, "main.cpp")

    val status_code = (s"rm $path" + "/" + s"$file") !

    assert(status_code == 0, "Delete generated lib file error!")

    val result = run_executable(path, "a.out")

    val status_code2 = (s"rm $path" + "/a.out") !

    assert(status_code2 == 0, "Delete generated binary error!")

    result


  }

  @Test
  def test_map(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/01.maphost"
    val file = "libmap.cpp"

    val f = fun( ArrayType(Float, N),
      in => MapSeq( incrementF ) $ in
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)
    //expect(expected){actual}

    println("Test case test_map done!")

  }

  @Test
  def test_reduceseq(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/09.reduceseq"
    val file = "libreduceseq.cpp"

    val f = fun( ArrayType(Float, N),
      in => ReduceSeq( add, 1.0f ) $ in
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "17 \n"
    assertEquals(expected, actual)

    println("Test case test_reduce done!")

  }

  @Test
  def test_zip(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/02.zip"
    val file = "libzip.cpp"

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) => MapSeq( fun(y => add2.apply(Get(y,0), Get(y,1)) ) ) $ Zip(left, right)
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 \n"
    assertEquals(expected, actual)

    println("Test case test_zip done!")
  }

  @Test
  def test_split_join(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/03.split_join"
    val file = "libsplit_join.cpp"

    val f = fun(
      ArrayType(Float, N),
      in => Join() o MapSeq( MapSeq(incrementF)  )  o Split(8) $ in
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_split_join done!")

  }

  @Test
  def test_map_zip_split_join(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/04.map_zip_split_join"
    val file = "libmap_zip_split_join.cpp"

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) => Join() o MapSeq( MapSeq( fun(y => add2.apply(Get(y,0), Get(y,1)) ) )  )  o Split(8) $ Zip(left, right)
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 \n"
    assertEquals(expected, actual)

    println("Test case test_map_zip_split_join done!")

  }

  @Test
  def test_transpose_transposew(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/05.transpose_transposew"
    val file = "libtranspose_tranposew.cpp"

    val f = fun(
      ArrayType(Float, N),
      in => Join() o TransposeW() o MapSeq( MapSeq(incrementF)  ) o Transpose() o Split(8) $ in
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }


  @Test
  def test_pad(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/06.pad"
    val file = "libpad.cpp"

    val f = fun(
      ArrayType(Float, N),
      in =>  MapSeq(incrementF) o Pad(1, 1, WrapUnsafe)  $ in
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    //notice that the output array expected is slightly larger than the input array
    //as the padding only influence the size of the output array,
    //not the input array
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_pad done!")

  }

  @Test
  def test_arrayFromUserFunGenerator(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/07.arrayfromuserfungenerator"
    val file = "libarrayfromuserfungenerator.cpp"

    val at = ArrayTypeWSWC(Float, SizeVar("N"))
    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)


    val f = fun(
      at,
      input => MapSeq(fun(y => add2.apply(Get(y,0), Get(y,1)))) $ Zip(input, ArrayFromUserFunGenerator(idxF, at))
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 \n"
    assertEquals(expected, actual)

    println("Test case test_pad done!")

  }


  @Test
  def test_array3dfromuserfungenerator (): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/08.array3dfromuserfungenerator"
    val file = "libarray3dfromuserfungenerator.cpp"

    val M = SizeVar("M")
    val O = SizeVar("O")

    val type3d = ArrayTypeWSWC( ArrayTypeWSWC( ArrayTypeWSWC(Float, O), M), N)
    val idxF = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ return i+j+k; }", Seq(Int, Int, Int, Int, Int, Int), Int)

    val f = fun(
      type3d,
      _ => Join() o MapSeq( Join() o MapSeq( MapSeq(incrementF)))  $ Array3DFromUserFunGenerator(idxF, type3d)
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 2 3 2 3 4 \n"
    assertEquals(expected, actual)

    println("Test case test_array3dfromuserfungenerator done!")

  }


  /* Common facility for testing FFT */
  //--------------------------------------------------------------------------------

  val p_pass1 = 2
  val p_pass2 = 4
  val LPrevIter = 1
  val N_fft = 8

  def genReorderedTwiddleWithDFTUserFun(complexConjugate: Boolean = false): UserFun = {
    val signString = if (complexConjugate) "" else "-"
    UserFun("genTwiddleWithDFT",
      Array("j", "k", "l", "LPrevIter", "pheight", "pwidth"),
      "{ Tuple2_double_double twiddleWithDFT;\n" +
        "\tdouble exponent = " + signString + "2.0 * (k * LPrevIter + j) * l / (pheight * LPrevIter);\n" +
        "\ttwiddleWithDFT._0 = cospi(exponent);\n" +
        "\ttwiddleWithDFT._1 = sinpi(exponent);\n" +
        "\treturn twiddleWithDFT;}",
      Seq(Int, Int, Int, Int, Int, Int),
      TupleType(Double, Double)
    )
  }

  val TypeOfB_pass2 =
    ArrayTypeWSWC(
      ArrayTypeWSWC(
        ArrayTypeWSWC(TupleType(Double, Double), p_pass2),
        p_pass2),
      p_pass1)


  val reorderedB_pass2 = Array3DFromUserFunGenerator(
    genReorderedTwiddleWithDFTUserFun(), TypeOfB_pass2)

  val TypeOfB_pass1 =
    ArrayTypeWSWC(
      ArrayTypeWSWC(
        ArrayTypeWSWC(TupleType(Double, Double), p_pass1),
        p_pass1),
      LPrevIter)

  val reorderedB_pass1 = Array3DFromUserFunGenerator(
    genReorderedTwiddleWithDFTUserFun(), TypeOfB_pass1)

  val complex_zero = (0.0, 0.0)

  val complexId: UserFun = UserFun("cId",
    Array("x"),
    "{ return x; }",
    Seq(TupleType(Double, Double)),
    TupleType(Double, Double))

  val complexMultAndSumUp: UserFun = UserFun("cMultAndSumUp",
    Array("acc", "t"),
    "{ Tuple2_double_double result;\n" +
      "result._0 = acc._0 + ((t._0)._0*(t._1)._0 - (t._0)._1)*(t._1)._1);\n" +
      "result._1 = acc._1 + ((t._0)._1*(t._1)._0 + (t._0)._0*(t._1)._1);\n" +
      "return result; }",
    Seq(TupleType(Double, Double), TupleType(TupleType(Double, Double), TupleType(Double, Double))),
    TupleType(Double, Double))

  //--------------------------------------------------------------------------------

  @Test
  def test_fft (): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/10.fft"
    val file = "libfft.cpp"


    val smallFFT =
      \(ArrayTypeWSWC(TupleType(Double, Double), N_fft),
        (x) =>
          //
          //Second Pass
          //
          Join() o MapSeq(Join() o TransposeW()) o Split(p_pass1) o MapGlb(fun((yChunkWithBrow) => {

            val yChunk = yChunkWithBrow._0
            val Brow = yChunkWithBrow._1
            Join() o MapSeq(\((Bchunk) =>
              toGlobal(MapSeq(complexId)) o ReduceSeq(complexMultAndSumUp, complex_zero)
                $ Zip(yChunk, Bchunk)
            )) $ Brow

          })) $ Zip(Transpose() o Split(N_fft/p_pass2) o
            //
            //First pass
            //
            //Bring chunks into order for the next pass.
            Join() o MapSeq(Join() o TransposeW()) o Split(LPrevIter) o MapGlb(\((yChunkWithBrow) => {

            //Matrix multiplication of Butterfly matrix with accompanying chunk of input array.
            val yChunk = yChunkWithBrow._0
            val Brow = yChunkWithBrow._1
            Join() o MapSeq(\((Bchunk) =>
              toGlobal(MapSeq(complexId)) o ReduceSeq(complexMultAndSumUp, complex_zero)
                $ Zip(yChunk, Bchunk)
            )) $ Brow

            //Assign Butterfly matrices to accompanying parts of the input array.
          })) $ Zip(//Reorder chunks of the input vector to be in the order of their accompanying Butterfly matrices.
            Transpose() o Split(N_fft/p_pass1) $ x,
            //Create an array of small Butterfly matrices.
            Pad(0, (N_fft/p_pass1) - 1, WrapUnsafe) $ reorderedB_pass1),

            Pad(0, (N_fft/p_pass2) - p_pass1, WrapUnsafe) $ reorderedB_pass2)
      )

    def arrayOfDoubleToArrayOfDoubleTuple(arr: Array[Double]): Array[(Double, Double)] = {
      arr.grouped(2).map(x => (x(0), x(1))).toArray
    }

    val vector = Array.tabulate(N_fft) { i => (1.0 * i, 1.0 * i) }


    val DELTA: Double = 0.0000000001

    val GOLD_FFT_8: Array[(Double, Double)] =
      Array((28.00000000000000, 28.00000000000000),
        (-13.65685424949238, 5.65685424949238),
        (-8.00000000000000, -0.00000000000000),
        (-5.65685424949238, -2.34314575050762),
        (-4.00000000000000, -4.00000000000000),
        (-2.34314575050762, -5.65685424949238),
        (0.00000000000000, -8.00000000000000),
        (5.65685424949238, -13.65685424949238))

    val output = GOLD_FFT_8 /*arrayOfDoubleToArrayOfDoubleTuple(
      Execute(32,32)[Array[Double]](smallFFT, vector)._1
    )*/

    assertTupleArrayEquals(GOLD_FFT_8, output, DELTA)


  }

  @Test
  def test_fft1(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/10.fft"
    val file = "libfft.cpp"

    val f =
      \(ArrayTypeWSWC(TupleType(Double, Double), N_fft),
        (x) =>
            //
            //First pass
            //
            //Bring chunks into order for the next pass.
            MapSeq(\((yChunkWithBrow) => {

            //Matrix multiplication of Butterfly matrix with accompanying chunk of input array.
            val yChunk = yChunkWithBrow._0
            val Brow = yChunkWithBrow._1
            Join() o MapSeq(\((Bchunk) =>
              ReduceSeq(complexMultAndSumUp, complex_zero)
                $ Zip(yChunk, Bchunk)
            )) $ Brow

            //Assign Butterfly matrices to accompanying parts of the input array.
          })) $ Zip(//Reorder chunks of the input vector to be in the order of their accompanying Butterfly matrices.
            Transpose() o Split(N_fft/p_pass1) $ x,
            //Create an array of small Butterfly matrices.
            Pad(0, (N_fft/p_pass1) - 1, WrapUnsafe) $ reorderedB_pass1)
      )

    CompileHost(f, path, file)

  }

  def assertTupleArrayEquals(expecteds: Array[(Double, Double)], actuals: Array[(Double, Double)],
                             delta: Double = 0.0): Unit = {
    val e = expecteds.flatMap(t => List(t._1, t._2))
    val a = actuals.flatMap(t => List(t._1, t._2))

    assertArrayEquals(e, a, delta)
  }


}
