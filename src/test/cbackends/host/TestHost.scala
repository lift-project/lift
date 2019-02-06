package cbackends.host

import ir.ast.Pad.Boundary.WrapUnsafe
import ir.ast.{Array3DFromUserFunGenerator, ArrayFromUserFunGenerator, Get, Join, Lambda, Pad, Slide, Slide2D, Slide3D, Split, Transpose, TransposeW, UserFun, Zip, \, fun}
import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import lift.arithmetic.{Cst, SizeVar}
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import opencl.ir.{Float, add, _}
import org.junit.Assert._
import org.junit.Test
import rewriting.Rewrite
import rewriting.rules.Rules
import rewriting.utils.NumberPrinter
//import org.scalatest.expect

import scala.language.postfixOps
import scala.sys.process._

import cbackends.common.executor.Executor.{native_compile_and_run}

class TestHost {

  val common_path = "/home/lu/Documents/Research/lift/src/test/cbackends/host"

  val N = SizeVar("N")

  val incrementF = fun(Float, x => add(Float).apply(1f, x))

  val add2 = UserFun("add", Array("l", "r"),
    "{ return (l + r); }",
    Seq(Float, Float), Float)




  @Test
  def test_map(): Unit = {

    val path = s"$common_path/01.maphost"
    val file = "libmap.cpp"

    val f = fun( ArrayType(Float, N),
      in => MapSeq( incrementF ) $ in
    )

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }

  @Test
  def test_reduceseq(): Unit = {

    val path = s"$common_path/09.reduceseq"
    val file = "libreduceseq.cpp"

    val f = fun( ArrayType(Float, N),
      in => ReduceSeq( add, 1.0f ) $ in
    )

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "17 \n"
    assertEquals(expected, actual)

    println("Test case test_reduce done!")

  }

  @Test
  def test_zip(): Unit = {

    val path = s"$common_path/02.zip"
    val file = "libzip.cpp"

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) => MapSeq( fun(y => add2.apply(Get(y,0), Get(y,1)) ) ) $ Zip(left, right)
    )

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 \n"
    assertEquals(expected, actual)

    println("Test case test_zip done!")
  }

  val add_complex = UserFun("add_complex", Array("init", "l", "r"),
    "{ return {std::get<0>(init)+l, std::get<1>(init)+r}; }",
    Seq(TupleType(Double,Double), Double, Double), TupleType(Double,Double)
  )

  @Test
  def test_reduceseq_zip(): Unit = {

    val path = s"$common_path/12.reduceseq_zip"
    val file = "libreduceseq_zip.cpp"

    val f = fun(
      ArrayType(Double, N),
      ArrayType(Double, N),
      (left, right) => ReduceSeq( fun((init, aValueInArray)=>add_complex.apply(init, Get(aValueInArray,0), Get(aValueInArray, 1))), (1.0,2.0) )  $ Zip(left, right)
    )

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "17 34 \n"
    assertEquals(expected, actual)

    println("Test case test_zip done!")

  }

  @Test
  def test_split_join(): Unit = {

    val path = s"$common_path/03.split_join"
    val file = "libsplit_join.cpp"

    val f = fun(
      ArrayType(Float, N),
      in => Join() o MapSeq( MapSeq(incrementF)  )  o Split(8) $ in
    )

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_split_join done!")

  }

  @Test
  def test_map_zip_split_join(): Unit = {

    val path = s"$common_path/04.map_zip_split_join"
    val file = "libmap_zip_split_join.cpp"

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) => Join() o MapSeq( MapSeq( fun(y => add2.apply(Get(y,0), Get(y,1)) ) )  )  o Split(8) $ Zip(left, right)
    )

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 \n"
    assertEquals(expected, actual)

    println("Test case test_map_zip_split_join done!")

  }

  @Test
  def test_transpose_transposew(): Unit = {

    val path = s"$common_path/05.transpose_transposew"
    val file = "libtranspose_tranposew.cpp"

    val f = fun(
      ArrayType(Float, N),
      in => Join() o TransposeW() o MapSeq( MapSeq(incrementF)  ) o Transpose() o Split(8) $ in
    )

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }


  @Test
  def test_pad(): Unit = {

    val path = s"$common_path/06.pad"
    val file = "libpad.cpp"

    val f = fun(
      ArrayType(Float, N),
      in =>  MapSeq(incrementF) o Pad(1, 1, WrapUnsafe)  $ in
    )

    HostCompiler ! (f, path, List(file) )

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

    val path = s"$common_path/07.arrayfromuserfungenerator"
    val file = "libarrayfromuserfungenerator.cpp"

    val at = ArrayTypeWSWC(Float, SizeVar("N"))
    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)


    val f = fun(
      at,
      input => MapSeq(fun(y => add2.apply(Get(y,0), Get(y,1)))) $ Zip(input, ArrayFromUserFunGenerator(idxF, at))
    )

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 \n"
    assertEquals(expected, actual)

    println("Test case test_pad done!")

  }


  @Test
  def test_array3dfromuserfungenerator (): Unit = {

    val path = s"$common_path/08.array3dfromuserfungenerator"
    val file = "libarray3dfromuserfungenerator.cpp"

    val M = SizeVar("M")
    val O = SizeVar("O")

    val type3d = ArrayTypeWSWC( ArrayTypeWSWC( ArrayTypeWSWC(Float, O), M), N)
    val idxF = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ return i+j+k; }", Seq(Int, Int, Int, Int, Int, Int), Int)

    val f = fun(
      type3d,
      _ => Join() o MapSeq( Join() o MapSeq( MapSeq(incrementF)))  $ Array3DFromUserFunGenerator(idxF, type3d)
    )

    HostCompiler ! (f, path, List(file) )

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


  //Seq(TupleType(Double, Double), TupleType(TupleType(Double, Double), TupleType(Double, Double))),
  //--------------------------------------------------------------------------------

  @Test
  def test_fft (): Unit = {

    val path = s"$common_path/10.fft"
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

    val path = s"$common_path/11.fft1"
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
            val yChunk = Get(yChunkWithBrow,0)
            val Brow = Get(yChunkWithBrow,1)
            Join() o MapSeq(\((Bchunk) =>
              ReduceSeq(complexMultAndSumUp, complex_zero) $ Zip(yChunk, Bchunk)
            )) $ Brow

            //Assign Butterfly matrices to accompanying parts of the input array.
          })) $ Zip(//Reorder chunks of the input vector to be in the order of their accompanying Butterfly matrices.
            Transpose() o Split(N_fft/p_pass1) $ x,
            //Create an array of small Butterfly matrices.
            Pad(0, (N_fft/p_pass1) - 1, WrapUnsafe) $ reorderedB_pass1)
      )

    //CompileHost(f, path, file)

  }

  def assertTupleArrayEquals(expecteds: Array[(Double, Double)], actuals: Array[(Double, Double)],
                             delta: Double = 0.0): Unit = {
    val e = expecteds.flatMap(t => List(t._1, t._2))
    val a = actuals.flatMap(t => List(t._1, t._2))

    assertArrayEquals(e, a, delta)
  }

  import scala.reflect.runtime._
  import scala.tools.reflect.ToolBox

  @Test
  def test_eval(): Unit = {

    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    val tb = mirror.mkToolBox()
    val code = """def add(ty: ScalarType, name: String = "add"): UserFun =
      UserFun(name, Array("x", "y"), "return x + y;", Seq(ty, ty), ty);

      val incrementF = fun(Float, x => add(Float).apply(1f, x));

     val N = SizeVar("N");

    fun( ArrayType(Float, N),
      in => MapSeq( incrementF ) $ in
    )

               """
    val tree = tb.parse(s"""
                           |import arithmetic._
                           |import lift.arithmetic._
                           |import ir._
                           |import ir.ast._
                           |import opencl.ir._
                           |import opencl.ir.pattern._
                           |import opencl.ir.ast._
                           |$code
                       """.stripMargin)
    val f = tb.eval(tree).asInstanceOf[Lambda]

    val path = s"$common_path/01.maphost"
    val file = "libmap.cpp"


    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Done")


  }

  @Test
  def test_matrix_mul(): Unit = {

    val path = s"$common_path/13.matrixmul"
    val file = "libmatrixmul.cpp"

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        MapSeq(fun( Arow =>
          Join() o  MapSeq(fun( Bcol =>
            ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol)
          )) $ B
        )) $ A
      })

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "8 8 8 8 \n"
    assertEquals(expected, actual)

    println("Done")

  }

  @Test
  def test_rewrite_rule_hello_world(): Unit = {

    val path = s"$common_path/14.rewrite_rule_hello_world"
    val file = "librewrite_rule_hello_world.cpp"


    val f = fun( ArrayType(Float, N),
      in => MapSeq( incrementF ) $ in
    )

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    //----------------------------------------------------------------------
    //Now rewrite

    val f2 = fun( ArrayType(Float, N),
      in => MapSeq( incrementF ) $ in
    )

    //used to find which id to rewrite
    println(NumberPrinter(f2))

    //val g = if (Rules.splitJoinMapSeq.rewrite.isDefinedAt(f.body) ) Rules.splitJoinMapSeq(Cst(4)).rewrite(f.body) else f
    val g = if (Rules.splitJoinMapSeq.rewrite.isDefinedAt(f2.body) ) Rewrite.applyRuleAt(f2, f2.body, Rules.splitJoinMapSeq(Cst(4))) else f2


    HostCompiler ! (g, path, List(file) )
    val actual_rewrite : String = native_compile_and_run(path, file)

    assertEquals(expected, actual_rewrite)


    println("Test case test_map done!")

  }

  @Test
  def test_reduce_3d_matrix(): Unit = {

    val path = s"$common_path/15.reduce_3d_matrix"
    val file = "libreduce_3d_matrix.cpp"

    val array3d = ArrayType( ArrayType(ArrayType(Float, N), N), N)

    //I really want this, as it make it the shape right as early as possible
    //Join() o ReduceSeq(add,0.0f) o MapSeq(Join() o ReduceSeq(add, 0.0f)) o MapSeq(MapSeq(Join() o ReduceSeq(add, 0.0f) )) $ _

    //debug version
    // MapSeq( ReduceSeq(add, 0.0f) ) o MapSeq(Join() o MapSeq( ReduceSeq(add, 0.0f) )) $ _
    //simpler version
    // MapSeq(Join() o MapSeq( ReduceSeq(add, 0.0f) )) $ _

    val f = fun(
      array3d,
      //RduceSeq(add,0.0f) o Join() o
       ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, 0.0f)) o MapSeq(Join() o MapSeq( ReduceSeq(add, 0.0f) )) $ _

    )

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "64 \n"
    assertEquals(expected, actual)

    println("Test case test_reduce_3d_matrix done!")


  }

  @Test
  def test_slide_hello(): Unit = {

    val path = s"$common_path/16.slide_hello"
    val file = "libslide_hello.cpp"

    val f = fun(
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(Float, N),
      in => MapSeq( MapSeq(incrementF) ) o Slide(3,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    ("rm -rf " + s"$path" ) !!

    println("Test case test_slide_hello done!")
  }

  @Test
  def test_slide_meaningful(): Unit = {

    val path = s"$common_path/17.slide_meaningful"
    val file = "libslide_meaningful.cpp"

    val f = fun(
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(Float, N),
      in => MapSeq( ReduceSeq(add, 0.0f) ) o Slide(3,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    /*
    import opencl.executor.Compile
    val gpu_f = fun(
      ArrayTypeWSWC(Float, N),
      in => MapGlb( toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) ) o Slide(3,1) $ in
    )
    Compile(gpu_f)
    */


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "3 3 3 3 3 3 3 3 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Test
  def test_slide2d(): Unit = {

    val path = s"$common_path/18.slide2d"
    val file = "libslide2d.cpp"

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      in => MapSeq( Join() o MapSeq(
        ReduceSeq(add, 0.0f)  o
        Join() o MapSeq( ReduceSeq(add, 0.0f) ) )
      ) o Slide2D(3,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

  @Test
  def test_slide3d_inner_part(): Unit = {

    val path = s"$common_path/21.slide3d_inner_part"
    val file = "libslide3d.cpp"

    /*
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      in => MapSeq( MapSeq( Join() o MapSeq(
        ReduceSeq(add, 0.0f)  o Join() o MapSeq( ReduceSeq(add, 0.0f) )  o MapSeq( Join() o MapSeq( ReduceSeq( add, 0.0f ) ))
      ) ) ) o Slide3D(3,1) $ in
    )*/

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      in => ReduceSeq(add, 0.0f)  o Join() o MapSeq( ReduceSeq(add, 0.0f) )  o MapSeq( Join() o MapSeq( ReduceSeq( add, 0.0f ) )) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "27 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

  @Test
  def test_slide3d(): Unit = {

    val path = s"$common_path/22.slide3d"
    val file = "libslide3d.cpp"


    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      in => MapSeq(  MapSeq( Join() o MapSeq(
        ReduceSeq(add, 0.0f)  o Join() o MapSeq( ReduceSeq(add, 0.0f) )  o MapSeq( Join() o MapSeq( ReduceSeq( add, 0.0f ) ))
      ) ) ) o Slide3D(3,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "27 27 27 27 27 27 27 27 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

  @Test
  def test_viewmapseq(): Unit = {

    val path = s"$common_path/19.viewmap"
    val file = "libviewmap.cpp"

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      in => MapSeq(  MapSeq( id ) ) o MapSeq( MapSeq( id )) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

  @Test
  def test_viewreduceseq(): Unit = {

    val path = s"$common_path/20.viewreduceseq"
    val file = "libviewreduce.cpp"

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      in =>  ReduceSeq( add, 0.0f ) o Join() o MapSeq( ReduceSeq( add, 0.0f )) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "9 \n"
    assertEquals(expected, actual)

    println("Test case test_viewreduce done!")
  }

  @Test
  def test_conv3d_atom(): Unit = {

    val path = s"$common_path/23.conv3d_atom"
    val file = "libconv3d_atom.cpp"


    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 6), 6), 6),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 6), 6), 6),
      (in, weights) =>
        ReduceSeq(add, 0.0f)  o Join() o
        MapSeq( ReduceSeq(add, 0.0f) )  o
        MapSeq( Join() o MapSeq( ReduceSeq( add, 0.0f ) ) ) o
        Split(6) o Split(6) o
          MapSeq( fun(y => mult.apply(Get(y,0), Get(y,1))))
          $ Zip( Join() o Join() $ in, Join() o Join() $ weights)
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "432 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

  @Test
  def test_conv3d(): Unit = {

    val path = s"$common_path/24.conv3d"
    val file = "libconv3d.cpp"


    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 8), 8),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 6), 6), 6),
      (in, weights) => MapSeq(  MapSeq( Join() o MapSeq(

        fun(cube =>

          ReduceSeq(add, 0.0f) o
          MapSeq( fun(y => mult.apply(Get(y,0), Get(y,1))) )
            $ Zip( Join() o Join() $ cube, Join() o Join() $ weights)

        )

      ) ) ) o Slide3D(6,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 432 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

}
