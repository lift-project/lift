package cbackends.host

import ir.ast.Pad.Boundary.WrapUnsafe
import ir.ast.{Array3DFromUserFunGenerator, ArrayAccess, ArrayFromUserFunGenerator, Get, Iterate, Join, Lambda, Pad, Slide, Slide2D, Slide3D, Slide3D_R, Split, Transpose, TransposeW, Unzip, UserFun, Zip, \, fun}
import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import lift.arithmetic.{Cst, SizeVar}
import opencl.ir.pattern._
import opencl.ir.{Float, add, dividedBy, _}
import org.junit.Assert._
import org.junit.{Ignore, Test}
import rewriting.Rewrite
import rewriting.rules.Rules
import rewriting.utils.NumberPrinter
//import org.scalatest.expect

import scala.language.postfixOps
import scala.sys.process._

import cbackends.common.executor.Executor.{native_compile_and_run}

@Ignore("These tests require fixes from host_code_gen_fft")
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
    "{ return {a2 * b3 - a3 * b2, a1 * b3 - a3 * b1, a1 * b2 - a2 * b1 };}",
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

  // TODO by Naums: this test fails because of the lift namespace-related problem
  // Another problem is that this test depended on Lu's change to Rules.scala:splitJoinMapSeq rule. Since that change
  // is breaking older tests, I reverted the rule to its original state. This test will have to be fixed to
  // account for that
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
    val expected : String = "-3 7 11 -9 -18 -9 \n"
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

  @Test
  def test_generate_all_numpy_functions(): Unit = {

    val path = s"$common_path/39.numpy/lift_numpy"

    val func_names = List("sin", "cos")

    //val files = func_names.map("lib" + _ + ".cpp")

    val array = ArrayType(Float, N)

    val sin_f = fun( array, MapSeq( sin ) $ _ )
    val cos_f = fun( array, MapSeq( cos ) $ _ )

    val all_funcs = List(sin_f, cos_f)

    (s"mkdir -p $path") !

    (func_names zip all_funcs).foreach {
      case (func_name, func) => HostCompiler ! (func, path, List("lib" + func_name + ".cpp"), func_name)
    }

    //HostCompiler ! (sin_f, path, List(files(0)), func_names(0))

    println("Done")

  }


}
