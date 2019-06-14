package cbackends.host

import cbackends.common.common_ir.{Concat, Slice}
import ir.ast.Pad.Boundary.WrapUnsafe
import ir.ast.{Array3DFromUserFunGenerator, ArrayAccess, ArrayFromUserFunGenerator, Get, Iterate, Join, Lambda, Pad, PadConstant, Slide, Slide2D, Slide3D, Slide3D_R, Split, Transpose, TransposeW, Unzip, UserFun, Zip, \, fun}
import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import lift.arithmetic.{Cst, SizeVar}
import opencl.ir.pattern._
import opencl.ir.{Float, add, dividedBy, _}
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

  val common_path = System.getProperty("user.dir") + "/src/test/cbackends/host"

  val N = SizeVar("N")
  val M = SizeVar("M")
  val O = SizeVar("O")
  val K = SizeVar("K")

  val incrementF = fun(Float, x => add(Float).apply(1f, x))

  val add2 = UserFun("add", Array("l", "r"),
    "{ return (l + r); }",
    Seq(Float, Float), Float)


  val diff2 = UserFun("diff2", Array("l", "r"),
    "{ return (r - l); }",
    Seq(Float, Float), Float)

  val cross_calc1 = UserFun("cross_calc", Array("a1","a2","a3","b1", "b2", "b3"),
    "{ return a2 * b3 - a3 * b2;}",
    Seq(Float, Float, Float, Float, Float, Float), Float )
  val cross_calc2 = UserFun("cross_calc", Array("a1","a2","a3","b1", "b2", "b3"),
    "{ return a1 * b3 - a3 * b1;}",
    Seq(Float, Float, Float, Float, Float, Float), Float )
  val cross_calc3 = UserFun("cross_calc", Array("a1","a2","a3","b1", "b2", "b3"),
    "{ return a2 * b3 - a3 * b2;}",
    Seq(Float, Float, Float, Float, Float, Float), Float )
  val cross_calc = UserFun("cross_calc", Array("a1","a2","a3","b1", "b2", "b3"),
    "{ return {a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1 };}",
    Seq(Float, Float, Float, Float, Float, Float), TupleType(Float,Float,Float) )

  val tuple_in_tuple_out = UserFun("tuple_in_tuple_out", Array("l", "r"),
    "{ return {l+1, r+1}; }",
    Seq(Float, Float), TupleType(Float,Float)
  )

  val trapz = UserFun("trapz", Array("x1", "x2", "y1", "y2"),
    "{ return (x2-x1)*(y2+y1)/2.0f; }",
    Seq(Float, Float, Float, Float), Float
  )


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
    "{ return {init._0+l, init._1+r}; }",
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
    val expected : String = "8 8 8 8 8 8 \n"
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

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "2 3 4 3 4 5 4 5 6 5 6 7 6 7 8 7 8 9 8 9 10 9 10 11 \n"
    assertEquals(expected, actual)

    //("rm -rf " + s"$path" ) !!

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

  @Test
  def test_conv3d_slide3D_diff(): Unit = {

    val path = s"$common_path/25.conv3d_slide3D_diff"
    val file = "libconv3d_slide3D_diff.cpp"


    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 8), 8),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 6), 6), 8),
      (in, weights) => MapSeq(  MapSeq( Join() o MapSeq(

        fun(cube =>

          ReduceSeq(add, 0.0f) o
            MapSeq( fun(y => mult.apply(Get(y,0), Get(y,1))) )
            $ Zip( Join() o Join() $ cube, Join() o Join() $ weights)

        )

      ) ) ) o Slide3D(6,1,6,1,8,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    //6*6*8*2 = 576
    val expected : String = "576 576 576 576 576 576 576 576 576 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

  @Test
  def test_conv3d_slide3D_R_diff(): Unit = {

    val path = s"$common_path/27.conv3d_slide3D_R_diff"
    val file = "libconv3d_slide3D_R_diff.cpp"


    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 8), 8),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 6), 6), 8),
      (in, weights) => MapSeq(  MapSeq( Join() o MapSeq(

        fun(cube =>

          ReduceSeq(add, 0.0f) o
            MapSeq( fun(y => mult.apply(Get(y,0), Get(y,1))) )
            $ Zip( Join() o Join() $ cube, Join() o Join() $ weights)

        )

      ) ) ) o Slide3D_R(8,1,6,1,6,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    //6*6*8*2 = 576
    val expected : String = "576 576 576 576 576 576 576 576 576 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

  @Test
  def test_pool(): Unit = {

    val path = s"$common_path/26.pool"
    val file = "libpool.cpp"

    val counts = 6 * 6 * 8

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 8), 8),
      in => MapSeq( MapSeq ( dividedBy(counts) )) o
        Join() o MapSeq( MapSeq( Join() o MapSeq(
        fun(y => ReduceSeq(add, 0.0f) o Join() o Join() $ y )
      ) ) ) o Slide3D(6,1,6,1,8,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    //6*6*8*2 = 576
    val expected : String = "2 2 2 2 2 2 2 2 2 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

  @Test
  def test_pool_r(): Unit = {

    val path = s"$common_path/28.pool"
    val file = "libpool_r.cpp"

    val counts = 8 * 6 * 6

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 8), 8),
      in => MapSeq( MapSeq ( dividedBy(counts) )) o
        Join() o MapSeq( MapSeq( Join() o MapSeq(
        fun(y => ReduceSeq(add, 0.0f) o Join() o Join() $ y )
      ) ) ) o Slide3D_R(8,1,6,1,6,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    //6*6*8*2 = 576
    val expected : String = "2 2 2 2 2 2 2 2 2 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

  //will not work, as it contains "concrete o nonconcrete o concrete"
  //use global compiler to slice them fix the issue
  /*
  @Test
  def test_conv_pool() : Unit = {


    val path = s"$common_path/29.conv_pool"
    val file = "libconv_pool.cpp"

    val counts = 8 * 6 * 6

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 10), 10), 10),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 3), 3), 3),
      (in, weights) =>
        //pool
        MapSeq( MapSeq ( dividedBy(counts) )) o
        Join() o MapSeq( MapSeq( Join() o MapSeq(
        fun(y => ReduceSeq(add, 0.0f) o Join() o Join() $ y )
      ) ) ) o Slide3D_R(8,1,6,1,6,1) o
        //conv
        MapSeq(  MapSeq( Join() o MapSeq(

        fun(cube =>

          ReduceSeq(add, 0.0f) o
            MapSeq( fun(y => mult.apply(Get(y,0), Get(y,1))) )
            $ Zip( Join() o Join() $ cube, Join() o Join() $ weights)

        )

      ) ) ) o Slide3D_R(3,1,3,1,3,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    //6*6*8*2 = 576
    val expected : String = "2 2 2 2 2 2 2 2 2 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")

  }
  */

  @Test
  def test_concrete_non_concrete(): Unit = {

    val path = s"$common_path/30.concrete_non_concrete"
    val file = "libconcrete_non_concrete.cpp"

    val f = fun(
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(Float, N),
      in => Join() o MapSeq( ReduceSeq(add, 0.0f) ) o Slide(3,1) o MapSeq(incrementF) $ in
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
    val expected : String = "6 6 6 6 6 6 6 6 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  /* //stash for now, as this problem is bypassed by emitting function directly
  @Test
  def test_concrete_non2d_concrete(): Unit = {

    val path = s"$common_path/31.concrete_non2d_concrete"
    val file = "libconcrete_non2d_concrete.cpp"

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      in => MapSeq( MapSeq(
        Join() o MapSeq( ReduceSeq(add, 0.0f) )
      ) ) o Slide2D(3,1) o MapSeq(MapSeq(incrementF)) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "18 18 18 18 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Test
  def test_concrete_nonTranspose_concrete(): Unit = {

    val path = s"$common_path/32.concrete_nonTranspose_concrete"
    val file = "libconcrete_nonTranspose_concrete.cpp"

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      in => MapSeq(MapSeq(incrementF)) o Transpose() o MapSeq(MapSeq(incrementF)) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "2 2 2 2 2 2 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  */

  /*
  @Test
  def test_iterate(): Unit = {

    val path = s"$common_path/31.iterate"
    val file = "libiterate.cpp"

    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => Iterate(6)(  MapSeq(incrementF) ) $ in
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
    val expected : String = "6 6 6 6 6 6 6 6 \n"
    assertEquals(expected, actual)

    println("Test case test_iterate done!")
  }
  */


  /*
  @Test
  def test_zip_unzip(): Unit = {

    val path = s"$common_path/31.iterate_zip"
    val file = "libiterate_zip.cpp"

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) =>  Unzip() o MapSeq( fun(y => tuple_in_tuple_out.apply(Get(y,0), Get(y,1)) ) ) $ Zip(left, right)
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "6 6 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }*/

  @Test
  def test_array_tuple(): Unit = {

    val path = s"$common_path/32.array_tuple"
    val file = "libarray_tuple.cpp"

    /*
    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      //(left, right) => Iterate(6)( CPUFunc( Unzip() o MapSeq( fun(y => tuple_in_tuple_out.apply(Get(y,0), Get(y,1)) ) ) ) ) $ Zip(left, right)
      (left, right) =>  MapGlb( fun(y => tuple_in_tuple_out.apply(Get(y,0), Get(y,1)) ) )  $ Zip(left, right)
    )
    */

    val f = fun(
      ArrayType(TupleType(Float, Float), N),
      MapSeq( fun(y => tuple_in_tuple_out.apply(Get(y,0), Get(y,1)) ) ) $ _
    )

    val f_gpu = fun(
      ArrayType(TupleType(Float, Float), N),
      MapGlb( fun(y => tuple_in_tuple_out.apply(Get(y,0), Get(y,1)) ) ) $ _
    )

    ("mkdir -p " + s"$path" ) !!

    //import opencl.executor.Compile
    //val res = Compile(f_gpu)

    HostCompiler ! (f, path, List(file))



    val actual : String = native_compile_and_run(path, file)
    val expected : String = "2, 3, 4, 5\n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }


  @Test
  def test_conv2d_for_cases_paper(): Unit = {

    val path = s"$common_path/33.conv2d_for_cases_paper"
    val file = "libconv2d.cpp"


    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 8),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 6), 6),
      (in, weights) =>   MapSeq( Join() o MapSeq(

        fun(square =>

          ReduceSeq(add, 0.0f) o
            MapSeq( fun(y => mult.apply(Get(y,0), Get(y,1))) )
            $ Zip( Join() $ square, Join() $ weights)

        )

      ) )  o Slide2D(6,1) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    //6*6*8*2 = 576
    val expected : String = "72 72 72 72 72 72 72 72 72 \n"
    assertEquals(expected, actual)

    println("Test case test_slide2d done!")
  }

  @Test
  def test_parallel_partial_reduce_then_sequential_reduce(): Unit = {

    val path = s"$common_path/34.parallel_partial_reduce_then_sequential_reduce"
    val file = "libpartial_reduce.cpp"

    val array = ArrayType(Float, N)


    val f = fun(
      array,
      ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, 0.0f) ) o Split(4) $ _
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "16 \n"
    assertEquals(expected, actual)

    println("Test case test_reduce_3d_matrix done!")


  }



  @Test
  def test_scanseq(): Unit = {

    val path = s"$common_path/35.scanseq"
    val file = "libscan.cpp"

    val array = ArrayType(Float, N)


    val f = fun(
      array,
      ScanSeq(add, 1.1f) $ _
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "2.1 3.1 4.1 5.1 6.1 7.1 8.1 9.1 10.1 11.1 12.1 13.1 14.1 15.1 16.1 17.1 \n"
    assertEquals(expected, actual)

    println("Test case test_reduce_3d_matrix done!")


  }

  @Test
  def test_numpy_diff(): Unit = {

    val path = s"$common_path/36.numpy_diff"
    val file = "libnumpy_diff.cpp"

    val array = ArrayType(Float, N)


    val f = fun(
      array,
      MapSeq( ReduceSeq(diff2, 0.0f) ) o Slide(2,1) $ _
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 2 3 -7 \n"
    assertEquals(expected, actual)

    println("Test case test_reduce_3d_matrix done!")


  }


  @Test
  def test_numpy_cross_prod(): Unit = {

    val path = s"$common_path/37.cross_prod"
    val file = "libcross_prod.cpp"

    val array = ArrayType(TupleType(Float, Float, Float) , N)


    val f = fun(
      array,
      array,
      (A,B) => MapSeq(  fun( y =>
        cross_calc.apply(
          Get(Get(y,0),0),
          Get(Get(y,0),1),
          Get(Get(y,0),2),
          Get(Get(y,1),0),
          Get(Get(y,1),1),
          Get(Get(y,1),2)
        ) )
      )  $ Zip(A,B)
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "-3 -7 11 -9 18 -9 \n"
    assertEquals(expected, actual)

    println("Test case test_reduce_3d_matrix done!")


  }


  @Test
  def test_numpy_trapz(): Unit = {

    val path = s"$common_path/38.trapz"
    val file = "libtrapz.cpp"

    val array = ArrayType(Float, N)


    val f = fun(
      array,
      array,
      (A,B) => MapSeq(
                  fun( (z) => trapz.apply(
                    Get( ArrayAccess(0) $ z, 0),
                    Get( ArrayAccess(1) $ z, 0),
                    Get( ArrayAccess(0) $ z, 1),
                    Get( ArrayAccess(1) $ z, 1) )
                  )
               ) o Slide(2,1) $ Zip(A,B)
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file))

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "4.5 5.5 \n"
    assertEquals(expected, actual)

    println("Test case test_reduce_3d_matrix done!")


  }

  val sin = UserFun("sin_uf", Array("x"),
    "{ return sin(x); }",
    Seq(Float), Float)

  val cos = UserFun("cos_uf", Array("x"),
    "{ return cos(x); }",
    Seq(Float), Float)

  val tan = UserFun("tan_uf", Array("x"),
    "{ return tan(x); }",
    Seq(Float), Float)

  val arcsin = UserFun("arcsin_uf", Array("x"),
    "{ return asin(x); }",
    Seq(Float), Float)

  val arccos = UserFun("arccos_uf", Array("x"),
    "{ return acos(x); }",
    Seq(Float), Float)

  val arctan = UserFun("arctan_uf", Array("x"),
    "{ return atan(x); }",
    Seq(Float), Float)

  val div = UserFun("div_uf", Array("x", "y"),
    "{ return (x)/(y); }",
    Seq(Float, Float), Float)

  val hypot = UserFun("hypot_uf", Array("x", "y"),
    "{ return sqrt((x*x)+(y*y)); }",
    Seq(Float, Float), Float)

  //Degree = radian * 180/π
  //radian = degree * π/180

  val degrees = UserFun("r2d_uf", Array("x"),
    "{ return x*180/M_PI; }",
    Seq(Float), Float)

  val radians = UserFun("d2r_uf", Array("x"),
    "{ return x*M_PI/180; }",
    Seq(Float), Float)


  val sinh = UserFun("sinh_uf", Array("x"),
    "{ return sinh(x); }",
    Seq(Float), Float)


  val cosh = UserFun("cosh_uf", Array("x"),
    "{ return cosh(x); }",
    Seq(Float), Float)

  val tanh = UserFun("tanh_uf", Array("x"),
    "{ return tanh(x); }",
    Seq(Float), Float)

  val arcsinh = UserFun("arcsinh_uf", Array("x"),
    "{ return asinh(x); }",
    Seq(Float), Float)

  val arccosh = UserFun("arccosh_uf", Array("x"),
    "{ return acosh(x); }",
    Seq(Float), Float)

  val arctanh = UserFun("arctanh_uf", Array("x"),
    "{ return atanh(x); }",
    Seq(Float), Float)

  val around = UserFun("round_uf", Array("x"),
    "return ( ((int) ceil(x)) % 2 == 0 ? ceil(x) : ceil(x) -1) ;",
    Seq(Float), Float)

  val rint = UserFun("rint_uf", Array("x"),
    "return round(x) ;",
    Seq(Float), Float)

  val fix = UserFun("fix_uf", Array("x"),
    "return trunc(x) ;",
    Seq(Float), Float)

  val floor = UserFun("floor_uf", Array("x"),
    "return floor(x);",
    Seq(Float), Float)

  val ceil = UserFun("ceil_uf", Array("x"),
    "return ceil(x);",
    Seq(Float), Float)

  val trunc = UserFun("trunc_uf", Array("x"),
    "return trunc(x);",
    Seq(Float), Float)

  val prod2 = UserFun("prod2_uf", Array("l", "r"),
    "{ return (l * r); }",
    Seq(Float, Float), Float)

  val gradient2 = UserFun("grad2_uf", Array("l", "r"),
    "{ return (l - r)/2.0f; }",
    Seq(Float, Float), Float)

  val exp = UserFun("exp_uf", Array("x"),
    "return exp(x) ;",
    Seq(Float), Float)

  val expm1 = UserFun("expm1_uf", Array("x"),
    "return exp(x) - 1 ;",
    Seq(Float), Float)

  val exp2 = UserFun("exp2_uf", Array("x"),
    "return pow(2,x) ;",
    Seq(Float), Float)

  val log = UserFun("log_uf", Array("x"),
    "return log(x) ;",
    Seq(Float), Float)

  val log10 = UserFun("log10_uf", Array("x"),
    "return log10(x) ;",
    Seq(Float), Float)

  val log2 = UserFun("log2_uf", Array("x"),
    "return log2(x) ;",
    Seq(Float), Float)

  val log1p = UserFun("log1p_uf", Array("x"),
    "return log(1+x) ;",
    Seq(Float), Float)

  val logaddexp = UserFun("logaddexp_uf", Array("x1", "x2"),
    "{ return log(exp(x1) + exp(x2)); }",
    Seq(Float, Float), Float)

  val logaddexp2 = UserFun("logaddexp2_uf", Array("x1", "x2"),
    "{ return log2(pow(2,x1) + pow(2,x2)); }",
    Seq(Float, Float), Float)

  val sinc = UserFun("sinc_uf", Array("x"),
    "return sin(M_PI*x)/(M_PI*x) ;",
    Seq(Float), Float)

  val signbit = UserFun("signbit_uf", Array("x"),
    "return x<0? 1:0 ;",
    Seq(Float), Float)

  val copysign = UserFun("copysign_uf", Array("x", "y"),
    "return y<0? x*(-1):x ;",
    Seq(Float, Float), Float)

  val frexp = UserFun("frexp_uf", Array("x"),
    "int exp; return {frexp(x,&exp), exp} ;",
    Seq(Float), TupleType(Float, Float) )

  val ldexp = UserFun("ldexp_uf", Array("x", "y"),
    "return x* pow(2,y) ;",
    Seq(Float, Float), Float)

  val nextafter = UserFun("nextafter_uf", Array("x", "y"),
    "return x<y? x+ std::numeric_limits<float>::epsilon() : (x>y? x - std::numeric_limits<float>::epsilon() : x)   ;",
    Seq(Float, Float), Float)

  val reciprocal = UserFun("reciprocal_uf", Array("x"),
    "return 1.0f/x",
    Seq(Float), Float)

  val negative = UserFun("negative_uf", Array("x"),
    "return (-1.0f)*x",
    Seq(Float), Float)

  val multiply = UserFun("multiply_uf", Array("x", "y"),
    "return x * y;",
    Seq(Float, Float), Float)

  val divide = UserFun("divide_uf", Array("x", "y"),
    "return x / y;",
    Seq(Float, Float), Float)

  val power = UserFun("power_uf", Array("x", "y"),
    "return pow(x, y);",
    Seq(Float, Float), Float)

  val subsract = UserFun("subtract_uf", Array("x", "y"),
    "return x - y;",
    Seq(Float, Float), Float)

  val floor_div = UserFun("floor_div_uf", Array("x", "y"),
    "return floor(x/y);",
    Seq(Float, Float), Float)

  val fmod = UserFun("fmod_uf", Array("x", "y"),
    "return ((int)x) % ((int)y);",
    Seq(Float, Float), Float)

  val modf = UserFun("modf_uf", Array("x"),
    "if(x>=0) return { x - floor(x), floor(x) }; else return  { x - round(x), round(x)} ;",
    Seq(Float), TupleType(Float, Float))

  //TODO: may need a closer look at this one, if x and y are not the same sign
  val remainder = UserFun("remainder_uf", Array("x", "y"),
    "if(x>=0) return x - floor(x/y)*y; else return x - round(x/y)*y",
    //"return int(x) % int(y)",
    Seq(Float, Float), Float)

  val divmod = UserFun("divmod_uf", Array("x", "y"),
    "return {int(x/y), x>=0? x - floor(x/y)*y : x - round(x/y)*y};",
    Seq(Float, Float), TupleType(Float, Float) )

  val angle_radian = UserFun("angle_randian_uf", Array("x", "y"),
    "{ return atan2(y,x); }",
    Seq(Float, Float), Float)

  val angle_degree = UserFun("angle_degree_uf", Array("x", "y"),
    "{ return atan2(y,x) * 180 / M_PI; }",
    Seq(Float, Float), Float)

  val real = UserFun("real_uf", Array("x", "y"),
    "{ return x; }",
    Seq(Float, Float), Float)

  val imag = UserFun("imag_uf", Array("x", "y"),
    "{ return y; }",
    Seq(Float, Float), Float)

  val conj = UserFun("conj_uf", Array("x", "y"),
    "{ return {x, (-1.0f)*y}; }",
    Seq(Float, Float), TupleType(Float, Float) )

  val sqrt = UserFun("sqrt_uf", Array("x"),
    "{ return sqrt(x); }",
    Seq(Float), Float)

  val cbrt = UserFun("cbrt_uf", Array("x"),
    "{ return cbrt(x); }",
    Seq(Float), Float)

  val square = UserFun("square_uf", Array("x"),
    "{ return x*x; }",
    Seq(Float), Float)

  val absolute = UserFun("absolute_uf", Array("x"),
    "{ return abs(x); }",
    Seq(Float), Float)

  val fabs = UserFun("fabs_uf", Array("x"),
    "{ return abs(x); }",
    Seq(Float), Float)

  val sign = UserFun("sign_uf", Array("x"),
    "{ return x==0? 0: ( x< 0 ? -1 : 1 ); }",
    Seq(Float), Float)

  //val heaviside

  val maximum = UserFun("maximum_uf", Array("x", "y"),
    "{ return max(x,y); }",
    Seq(Float, Float), Float)

  val minimum = UserFun("minimum_uf", Array("x", "y"),
    "{ return min(x,y); }",
    Seq(Float, Float), Float)

  val fmax = UserFun("fmax_uf", Array("x", "y"),
    "{ return max(x,y); }",
    Seq(Float, Float), Float)

  val fmin = UserFun("fmin_uf", Array("x", "y"),
    "{ return min(x,y); }",
    Seq(Float, Float), Float)

  //Not do it, because C++ don't have a NaN reprensentation
  //val nan_to_num

  //Not do it, because the output shape is run-time data dependent
  //real_if_close

  //reduce pattern
  //it is a bit hard to handle cases that is not within bounds,
  // because using map or reduce, it is hard to get access to the concept of the first element or the last element
  // if that is the case, maybe switch to numpy implementation instead.
  val interp = UserFun("interp_uf", Array("acc", "x", "x1", "y1", "x2", "y2"),
    "if (x >= x1 && x <= x2) {auto a = (y1-y2)/(x1-x2); auto b = y1 - a*x1; return acc + a*x + b;} else return acc + 0.0f;",
    Seq(Float, Float, Float, Float, Float, Float), Float)


  @Test
  def test_generate_all_numpy_functions(): Unit = {

    val path = s"$common_path/39.numpy/lift_numpy"

    val func_names = List(
      "lift_sin", "cos", "tan", "arcsin", "arccos", "arctan", "hypot", "arctan2", "degrees", "radians", "deg2rad", "rad2deg",
      "sinh", "cosh", "tanh", "arcsinh", "arccosh", "arctanh",
      "around", "round_", "rint", "fix", "lift_floor", "ceil", "trunc",
      "prod", "sum", "sum_axis_0", "sum_axis_1",  "nanprod", "nansum", "cumprod", "cumsum", "nancumprod", "nancumsum", "diff", "ediff1d", "gradient", "cross", "trapz",
      "lift_exp", "expm1", "exp2", "lift_log", "lift_log10", "lift_log2", "log1p", "logaddexp", "logaddexp2",
      "sinc",
      "signbit", "copysign", "lift_frexp", "ldexp", "nextafter",

      "add", "reciprocal", "positive", "negative", "multiply", "divide", "power", "subtract", "true_divide", "floor_divide", "float_power",
      "fmod", "mod", "modf", "lift_remainder", "divmod",

      "angle_radian", "angle_degree", "real", "imag", "conj",

      "convolve", //"clip"
      "sqrt", "cbrt", "square", "absolute", "fabs", "sign", "maximum", "minimum", "fmax", "fmin", "interp"
    )

    //val files = func_names.map("lib" + _ + ".cpp")

    val array = ArrayType(Float, N)
    val array_m = ArrayType(Float, M)
    val array_2d = ArrayType(ArrayType(Float, N), M)
    val array_t2 = ArrayType(TupleType(Float, Float) , N)
    val array_t3 = ArrayType(TupleType(Float, Float, Float) , N)

    val sin_f = fun( array, MapSeq( sin ) $ _ )
    val cos_f = fun( array, MapSeq( cos ) $ _ )
    val tan_f = fun( array, MapSeq( tan ) $ _ )
    val arcsin_f = fun( array, MapSeq( arcsin ) $ _ )
    val arccos_f = fun( array, MapSeq( arccos ) $ _ )
    val arctan_f = fun( array, MapSeq( arctan ) $ _ )
    val hypot_f = fun( array, array, MapSeq( fun( y => hypot.apply(Get(y,0), Get(y,1)) )  ) $ Zip(_,_) )
    val arctan2_f = fun( array, array, MapSeq( arctan ) o MapSeq( fun( y => div.apply(Get(y,0), Get(y,1)) )  ) $ Zip(_,_) )
    val degrees_f = fun( array, MapSeq( degrees ) $ _ )
    val radians_f = fun( array, MapSeq( radians ) $ _ )
    //val unwrap_f
    val deg2rad_f = radians_f
    val rad2deg_f = degrees_f

    val sinh_f = fun( array, MapSeq(sinh) $ _ )
    val cosh_f = fun( array, MapSeq(cosh) $ _ )
    val tanh_f = fun( array, MapSeq(tanh) $ _ )
    val arcsinh_f = fun( array, MapSeq(arcsinh) $ _ )
    val arccosh_f = fun( array, MapSeq(arccosh) $ _ )
    val arctanh_f = fun( array, MapSeq(arctanh) $ _ )

    val around_f = fun( array, MapSeq(around) $ _ )
    val round__f = around_f
    val rint_f = fun( array, MapSeq(rint) $ _ )
    val fix_f = fun( array, MapSeq(fix) $ _ )
    val floor_f = fun( array, MapSeq(floor) $ _ )
    val ceil_f = fun( array, MapSeq(ceil) $ _ )
    val trunc_f = fun( array, MapSeq(trunc) $ _ )

    val prod_f = fun( array, ReduceSeq(prod2, 1.0f) $ _ )
    val sum_f = fun( array, ReduceSeq(add2, 0.0f) $ _ )
    // tested on 2D array only
    val sum_axis_0_f = fun( array_2d, MapSeq(ReduceSeq(add2, 0.0f)) o Transpose() $ _ )
    // tested on 2D array only
    val sum_axis_1_f = fun( array_2d, MapSeq(ReduceSeq(add2, 0.0f)) $ _ )
    //can filter nan at python level, for prod, nan -> 1, for sum, nan -> 0
    val nanprod_f = prod_f
    val nansum_f = sum_f
    val cumprod_f = fun( array, ScanSeq(prod2, 1.0f) $ _ )
    val cumsum_f = fun( array, ScanSeq(add2, 0.0f) $ _ )
    val nancumprod_f = cumprod_f
    val nancumsum_f = cumsum_f
    val diff_f = fun( array, MapSeq( ReduceSeq(diff2, 0.0f) ) o Slide(2,1) $ _ )
    //the array concantenation can be done at python level
    val ediff1d_f = diff_f
    //the first element and the last element should be set manually
    //out[0] = in[0]
    //out[last] = in[last] - in[last - 1]
    val gradient_f = fun(array, MapSeq(  fun(arr => gradient2.apply(ArrayAccess(2) $ arr, ArrayAccess(0) $ arr ) ) ) o Slide(3,1) $ _)
    val cross_f = fun( array_t3, array_t3,
      (A,B) => MapSeq(  fun( y =>
        cross_calc.apply(
          Get(Get(y,0),0),
          Get(Get(y,0),1),
          Get(Get(y,0),2),
          Get(Get(y,1),0),
          Get(Get(y,1),1),
          Get(Get(y,1),2)
        ) )
      )  $ Zip(A,B)
    )
    val trapz_f = fun( array, array, (A,B) => ReduceSeq(add2, 0.0f) o MapSeq(
        fun( (z) => trapz.apply(
          Get( ArrayAccess(0) $ z, 0),
          Get( ArrayAccess(1) $ z, 0),
          Get( ArrayAccess(0) $ z, 1),
          Get( ArrayAccess(1) $ z, 1) )
        )
      ) o Slide(2,1) $ Zip(A,B)
    )

    val exp_f = fun( array, MapSeq(exp) $ _ )
    val expm1_f = fun( array, MapSeq(expm1) $ _ )
    val exp2_f = fun( array, MapSeq(exp2) $ _ )
    val log_f = fun( array, MapSeq(log) $ _ )
    val log10_f = fun( array, MapSeq(log10) $ _ )
    val log2_f = fun( array, MapSeq(log2) $ _ )
    val log1p_f = fun( array, MapSeq(log1p) $ _ )
    val logaddexp_f = fun( array, array, (A,B) => MapSeq( fun(y => logaddexp.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val logaddexp2_f = fun( array, array, (A,B) => MapSeq( fun(y => logaddexp2.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val nextafter_f = fun( array, array, (A,B) => MapSeq( fun(y => nextafter.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )

    //TODO: i0, can not find its math def
    val sinc_f = fun( array, MapSeq(sinc) $ _ )

    val signbit_f = fun( array, MapSeq(signbit) $ _ )
    val copysign_f = fun( array, array, (A,B) => MapSeq( fun(y => copysign.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val frexp_f = fun( array, MapSeq(frexp) $ _ )
    val ldexp_f = fun( array, array, (A,B) => MapSeq( fun(y => ldexp.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    //TODO: can not find its math def
    //val spacing_f =

    val add_f = fun( array, array, (A,B) => MapSeq( fun(y => add2.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val reciprocal_f = fun( array, MapSeq(reciprocal) $ _ )
    val positive_f = fun( array, MapSeq(id) $ _ )
    val negative_f = fun( array, MapSeq(negative) $ _ )
    //for different dimensionality, maybe handled at python level
    val multiply_f = fun( array, array, (A,B) => MapSeq( fun(y => multiply.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val divide_f = fun( array, array, (A,B) => MapSeq( fun(y => divide.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val power_f = fun( array, array, (A,B) => MapSeq( fun(y => power.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val subtract_f = fun( array, array, (A,B) => MapSeq( fun(y => subtract.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val true_divide = divide_f
    val floor_divide = fun( array, array, (A,B) => MapSeq( fun(y => floor_div.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val float_power_f = power_f
    val fmod_f = fun( array, array, (A,B) => MapSeq( fun(y => fmod.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val mod_f = fmod_f
    val modf_f = fun( array, MapSeq(modf) $ _ )
    val remainder_f = fun( array, array, (A,B) => MapSeq( fun(y => remainder.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val divmod_f = fun( array, array, (A,B) => MapSeq( fun(y => divmod.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )

    val angle_radian_f = fun(array_t2, MapSeq( fun(y => angle_radian.apply(Get(y,0), Get(y,1))) ) $ _ )
    val angle_degree_f = fun(array_t2, MapSeq( fun(y => angle_degree.apply(Get(y,0), Get(y,1))) ) $ _ )
    val real_f = fun(array_t2, MapSeq( fun(y => real.apply(Get(y,0), Get(y,1))) ) $ _ )
    val imag_f = fun(array_t2, MapSeq( fun(y => imag.apply(Get(y,0), Get(y,1))) ) $ _ )
    val conj_f = fun(array_t2, MapSeq( fun(y => conj.apply(Get(y,0), Get(y,1))) ) $ _ )

    // Can slide window be a run-time parameter? Maybe you can, as Slide can take a arith expr as param, a variable is a arith expr
    // the truth is: it must be in that way, as type check need to make sure two inputs to zip are the same
    // assume M is already reversed, later may be doable in Lift
    val convolve_f = fun(array_m, array, (A, M) =>
      MapSeq( fun( y =>  ReduceSeq( fun( (acc, y) => multAndSumUp(acc, Get(y,0), Get(y,1)) ), 0.0f) $ Zip(y, M) ) ) o Slide(N,1) $ A)
    //two things: do we have a filter pattern? can we pass a scalar to the top level lambda instead of arrays?
    //val clip_f = fun(array, O, K,   )
    val sqrt_f = fun( array, MapSeq(sqrt) $ _ )
    val cbrt_f = fun( array, MapSeq(cbrt) $ _ )
    val square_f = fun( array, MapSeq(square) $ _ )
    val absolute_f = fun( array, MapSeq(absolute) $ _ )
    val fabs_f = absolute_f
    val sign_f = fun( array, MapSeq(sign) $ _ )
    val maximum_f = fun( array, array, MapSeq(fun(y => maximum.apply(Get(y,0), Get(y,1)))) $ Zip(_,_) )
    val minimum_f = fun( array, array, MapSeq(fun(y => minimum.apply(Get(y,0), Get(y,1)))) $ Zip(_,_) )
    val fmax_f = fun( array, array, MapSeq(fun(y => fmax.apply(Get(y,0), Get(y,1)))) $ Zip(_,_) )
    val fmin_f = fun( array, array, MapSeq(fun(y => fmin.apply(Get(y,0), Get(y,1)))) $ Zip(_,_) )
    val interp_f = fun(array, array_m, array_m, (I, X, Y) =>
      MapSeq( fun(ix =>
         ReduceSeq( fun( (acc, y) =>
           interp.apply(
             acc,
             ix,
             Get(ArrayAccess(0) $ y, 0),
             Get(ArrayAccess(0) $ y, 1),
             Get(ArrayAccess(1) $ y, 0),
             Get(ArrayAccess(1) $ y, 1)
           )
         ), 0.0f) o Slide(2,1) $ Zip(X,Y)
      ) ) $ I
    )

    val all_funcs = List(
      sin_f, cos_f, tan_f, arcsin_f, arccos_f, arctan_f, hypot_f, arctan2_f, degrees_f, radians_f, deg2rad_f, rad2deg_f,
      sinh_f, cosh_f, tanh_f, arcsinh_f, arccos_f, arctanh_f,
      around_f, round__f, rint_f, fix_f, floor_f, ceil_f, trunc_f,
      prod_f, sum_f, sum_axis_0_f, sum_axis_1_f, nanprod_f, nansum_f, cumprod_f, cumsum_f, nancumprod_f, nancumsum_f, diff_f, ediff1d_f, gradient_f, cross_f, trapz_f,
      exp_f, expm1_f, exp2_f, log_f, log10_f, log2_f, log1p_f, logaddexp_f, logaddexp2_f,
      sinc_f,
      signbit_f, copysign_f, frexp_f, ldexp_f, nextafter_f,

      add_f, reciprocal_f, positive_f, negative_f, multiply_f, divide_f, power_f, subtract_f, true_divide, floor_divide, float_power_f,
      fmod_f, mod_f, modf_f, remainder_f, divmod_f,

      angle_radian_f, angle_degree_f, real_f, imag_f, conj_f,

      convolve_f, //clip_f,
      sqrt_f, cbrt_f, square_f, absolute_f, fabs_f, sign_f, maximum_f, minimum_f, fmax_f, fmin_f, interp_f
    )

    (s"mkdir -p $path") !

    (func_names zip all_funcs).foreach {
      case (func_name, func) => HostCompiler ! (func, path, List("lib" + func_name + ".cpp"), func_name)
    }

    //HostCompiler ! (sin_f, path, List(files(0)), func_names(0))

    println("Done")

  }

  @Test
  def test_split_on_variable(): Unit = {

    val path = s"$common_path/40.split_on_variable"
    val file = "libsplit_on_variable.cpp"

    val f = fun( ArrayType(Float, N),
      //in => MapSeq( incrementF ) $ in
      in => MapSeq( MapSeq( incrementF ) ) o Split(N/2) $ in
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }

  @Test
  def test_temp(): Unit = {

    val path = s"$common_path/40.split_on_variable"
    val file = "libsplit_on_variable.cpp"

    val f = fun( ArrayType(Float, N),
      //in => MapSeq( incrementF ) $ in
      in => MapSeq( Join() o MapSeq( MapSeq(incrementF) ) o Split(2) ) o Split(8) $ in
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }

  @Test
  def test_par_reduce(): Unit = {

    val path = s"$common_path/41.parallel_reduce"
    val file = "libpar_reduce.cpp"

    val f = fun( ArrayType(Float, N),
      //in => MapSeq( incrementF ) $ in
      in => ReduceSeq(add2, 0.0f)  o Join() o Join() o MapSeq(MapSeq( ReduceSeq(add2, 0.0f) ) ) o Split(4) o Split(N/8) $ in
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "32 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }

  @Test
  def test_slice(): Unit = {

    val path = s"$common_path/42.slice"
    val file = "libslice.cpp"

    val f = fun( ArrayType(Float, N),
      in => MapSeq( incrementF ) o Slice(3,N) $ in
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }

  @Test
  def test_concat(): Unit = {

    val path = s"$common_path/43.concat"
    val file = "libconcat.cpp"

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, M),
      //The args in Concat must contain at least one operator,
      //as Concat is an output view construct, thus there must be at least one operator to produce output
      (in1, in2) => Concat( MapSeq(id) $ in1, MapSeq(id) $ in2)
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 2 2 2 2 2 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }

  @Test
  def test_concat_more(): Unit = {

    val path = s"$common_path/43.concat"
    val file = "libconcat.cpp"

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, M),
      //The args in Concat must contain at least one operator,
      //as Concat is an output view construct, thus there must be at least one operator to produce output
      //TODO: if MapSeq(id) o MapSeq(id) $ in, the memory allocator will yield wrong final code
      (in1, in2) => MapSeq(incrementF) $ Concat( MapSeq(id) $ in1, MapSeq(id) $ in2)
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "2 2 2 3 3 3 3 3 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }

  @Test
  def test_concat_slice(): Unit = {

    val path = s"$common_path/44.concat_slice"
    val file = "libconcat_slice.cpp"

    val f = fun(
      ArrayType(Float, N),
      //The args in Concat must contain at least one operator,
      //as Concat is an output view construct, thus there must be at least one operator to produce output
      in => MapSeq(incrementF) $ Concat( MapSeq(incrementF) o Slice(0,3) $ in, MapSeq(incrementF) o Slice(3, N) $ in)
    )

    (s"mkdir -p $path") !

    HostCompiler ! (f, path, List(file) )

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "2 2 2 3 3 3 3 3 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }

}
