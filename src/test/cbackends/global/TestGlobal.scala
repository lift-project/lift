package cbackends.global

import cbackends.host.host_ir._
import cbackends.onnx.lift_nn_ir.host_ir.Pool3D
import ir.ast.Pad.Boundary.WrapUnsafe
import ir.ast.{Array3DFromUserFunGenerator, ArrayFromUserFunGenerator, Get, Iterate, Join, Lambda, Pad, Slide, Slide2D, Slide3D, Slide3D_R, Split, Transpose, TransposeW, Unzip, UserFun, Zip, \, fun}
import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import lift.arithmetic.{Cst, SizeVar}
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
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


class TestGlobal {

  val common_path = System.getProperty("user.dir") + "/src/test/cbackends/global"

  val N = SizeVar("N")
  val M = SizeVar("M")
  val O = SizeVar("O")
  val K = SizeVar("K")

  val incrementF = fun(Float, x => add(Float).apply(1f, x))
  val incrementF2 = fun(Float, x => add(Float).apply(2f, x))

  val add2 = UserFun("add", Array("l", "r"),
    "{ return (l + r); }",
    Seq(Float, Float), Float)

  @Test
  def test_cpu_func(): Unit = {

    val path = s"$common_path/01.cpufunc"
    val file = "libcpufunc.cpp"

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      in => CPUFunc( MapSeq(MapSeq(incrementF))  ) o CPUFunc( MapSeq(MapSeq(incrementF)) ) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "3 3 3 3 3 3 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Test
  def test_pool(): Unit = {

    val path = s"$common_path/02.pool"
    val file = "libpool.cpp"

    val pool_lambda = MapSeq( MapSeq ( dividedBy(2) )) o
        Join() o MapSeq( MapSeq( Join() o MapSeq(
        fun( y =>
          ReduceSeq(add, 0.0f) o
            Join() o Join() $ y )
      ) ) ) o Slide3D_R(3,1,3,1,3,1)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N) ,
      in => CPUFunc( pool_lambda  )  $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "27 27 27 27 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }


  @Test
  def test_pool_pool(): Unit = {

    val path = s"$common_path/03.pool_pool"
    val file = "libpool_pool.cpp"

    val pool_lambda1 = MapSeq( MapSeq ( MapSeq( dividedBy(2) ) )) o
      MapSeq( MapSeq( Join() o MapSeq(
      fun( y =>
        ReduceSeq(add, 0.0f) o
          Join() o Join() $ y )
    ) ) ) o Slide3D_R(3,1,3,1,3,1)

    //TODO: in the future you can find a way to reuse kernel
    //currently, since the funcName in Lambda is mutable,
    //thus one has to use multiple lambdas even though they are the same
    val pool_lambda2 = MapSeq( MapSeq ( MapSeq( dividedBy(2) ) )) o
      MapSeq( MapSeq( Join() o MapSeq(
        fun( y =>
          ReduceSeq(add, 0.0f) o
            Join() o Join() $ y )
      ) ) ) o Slide3D_R(3,1,3,1,3,1)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N) ,
      in => CPUFunc( pool_lambda1 ) o CPUFunc( pool_lambda2  )  $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "364.5 364.5 364.5 364.5 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Test
  def test_binary_cpu_func(): Unit = {

    val path = s"$common_path/04.binary_cpu_func"
    val file = "libbinary_cpu_func.cpp"

    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, M),
      (in1, in2) => CPUFunc( fun( (i1,i2) => MapSeq(incrementF) $ i1 ) ).apply( in1, in2)
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "3 3 3 3 3 3 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Test
  def test_conv(): Unit = {

    val path = s"$common_path/05.conv"
    val file = "libconv.cpp"

    val conv_lambda = fun(
      (in, weights) =>
        MapSeq(  MapSeq( Join() o MapSeq(

          fun(cube =>

            ReduceSeq(add, 0.0f) o
              MapSeq( fun(y => mult.apply(Get(y,0), Get(y,1))) )
              $ Zip( Join() o Join() $ cube, Join() o Join() $ weights)

          )

        ) ) ) o Slide3D_R(3,1,3,1,3,1) $ in

    )

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 8), 8) ,
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 3), 3), 3) ,
      (in, weights) =>
          CPUFunc( conv_lambda  ).apply(in, weights)
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 54 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Test
  def test_conv_pool(): Unit = {

    val path = s"$common_path/06.conv_pool"
    val file = "libconv_pool.cpp"

    val pool_lambda = MapSeq( MapSeq ( MapSeq( dividedBy(2) ) )) o
      MapSeq( MapSeq( Join() o MapSeq(
        fun( y =>
          ReduceSeq(add, 0.0f) o
            Join() o Join() $ y )
      ) ) ) o Slide3D_R(3,1,3,1,3,1)

    val conv_lambda = fun(
      (in, weights) =>
        MapSeq(  MapSeq( Join() o MapSeq(

          fun(cube =>

            ReduceSeq(add, 0.0f) o
              MapSeq( fun(y => mult.apply(Get(y,0), Get(y,1))) )
              $ Zip( Join() o Join() $ cube, Join() o Join() $ weights)

          )

        ) ) ) o Slide3D_R(3,1,3,1,3,1) $ in

    )

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 8), 8), 8) ,
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, 3), 3), 3) ,
      (in, weights) =>
        //CPUFunc2( pool_lambda ) o
        CPUFunc( pool_lambda ) o CPUFunc( conv_lambda  ) apply (in, weights)
      //conv_lambda.apply(in, weights)

    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 729 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }


  @Ignore("not ready yet")
  @Test
  def test_gpu_func(): Unit = {

    val path = s"$common_path/07.gpufunc"
    val file = "libgpufunc.cpp"

    val f = fun(
      ArrayTypeWSWC(Float, N),
        in => ToHost() o OclFunc( MapGlb( incrementF )  ) o ToGPU()  $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "2 2 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }


  @Ignore("not ready yet")
  @Test
  def test_gpu_func_multi(): Unit = {

    val path = s"$common_path/08.gpufunc_multi"
    val file = "libgpufunc_multi.cpp"

    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => ToHost() o OclFunc( MapGlb( incrementF )  ) o OclFunc( MapGlb( incrementF )  ) o ToGPU()  $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "3 3 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Ignore("not ready yet")
  @Test
  def test_cpu_gpu_func_multi(): Unit = {

    val path = s"$common_path/09.cpu_gpu_func_multi"
    val file = "libcpu_gpu_func_multi.cpp"

    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => CPUFunc( MapSeq(incrementF) ) o CPUFunc( MapSeq(incrementF) ) o
        ToHost() o OclFunc( MapGlb( incrementF )  )  o OclFunc( MapGlb( incrementF )  ) o ToGPU()  $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "5 5 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Ignore("not ready yet")
  @Test
  def test_cpu_gpu_func_multi_interleaved(): Unit = {

    val path = s"$common_path/10.cpu_gpu_func_multi_interleaved"
    val file = "libcpu_gpu_func_multi_interleaved.cpp"

    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => CPUFunc( MapSeq(incrementF) ) o ToHost() o OclFunc( MapGlb( incrementF )  ) o ToGPU() o
        CPUFunc( MapSeq(incrementF) ) o ToHost() o OclFunc( MapGlb( incrementF )  ) o ToGPU()  $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "5 5 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Ignore("not ready yet")
  @Test
  def test_gpu_func_enable_gpu_timer(): Unit = {

    val path = s"$common_path/11.gpufunc_enable_gpu_timer"
    val file = "libgpufunc_enable_gpu_timer.cpp"

    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => CPUFunc( MapSeq(incrementF), cpu_timer = true ) o
        ToHost(cpu_timer = true, gpu_timer = true) o
        OclFunc( MapGlb( incrementF ), cpu_timer = true,  gpu_timer = true  ) o
        ToGPU(cpu_timer = true, gpu_timer = true)  $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    //need to use a larger array to measure time,
    //the result is too large array, thus hard to enumerate
    //and test, ignore now.

    val actual : String = native_compile_and_run(path, file)
    //val expected : String = "3 3 \n"
    //assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }


  @Test
  def test_iterate(): Unit = {

    val path = s"$common_path/12.iterate"
    val file = "libiterate.cpp"

    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => Iterate(6)(  CPUFunc( MapSeq(incrementF) ) ) $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "6 6 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Ignore("not ready yet")
  @Test
  def test_iterate_gpu(): Unit = {

    val path = s"$common_path/13.iterate_gpu"
    val file = "libiterate.cpp"

    val f = fun(
      ArrayTypeWSWC(Float, N),
      in => ToHost() o Iterate(6)(  OclFunc( MapGlb(incrementF) ) ) o ToGPU() $ in
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "6 6 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }


  val tuple_in_tuple_out = UserFun("tuple_in_tuple_out", Array("l", "r"),
    "{ return {l+1, r+1}; }",
    Seq(Float, Float), TupleType(Float,Float)
  )


  @Ignore("not ready yet")
  @Test
  def test_iterate_zip_array_tuples(): Unit = {

    val path = s"$common_path/14.iterate_zip"
    val file = "libiterate_zip.cpp"

    val f = fun(
      ArrayType(TupleType(Float, Float), N),
      Iterate(6)( CPUFunc( MapSeq( fun(y => tuple_in_tuple_out.apply(Get(y,0), Get(y,1)) ) ) ) ) $ _
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "6 6 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Ignore("not ready yet")
  @Test
  def test_iterate_zip_two_arrays(): Unit = {

    val path = s"$common_path/14.iterate_zip"
    val file = "libiterate_zip.cpp"

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) => Iterate(6)( CPUFunc( Unzip() o MapSeq( fun(y => tuple_in_tuple_out.apply(Get(y,0), Get(y,1)) ) ) o Zip(2) )  o Unzip() ) $ Zip(left, right)
    )

    ("mkdir -p " + s"$path" ) !!

    GlobalCompiler ! (f, path, List(file))


    val actual : String = native_compile_and_run(path, file)
    val expected : String = "6 6 \n"
    assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }


  import arithmetic._
  import lift.arithmetic._
  import ir._
  import ir.ast._
  import opencl.ir._
  import opencl.ir.pattern._
  import opencl.ir.ast._
  @Test
  def test_conv_gpu(): Unit = {

    val path = s"$common_path/15.conv_gpu"
    val file = "libiterate_zip.cpp"

    /*
    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) => Iterate(6)( CPUFunc( Unzip() o MapSeq( fun(y => tuple_in_tuple_out.apply(Get(y,0), Get(y,1)) ) ) o Zip(2) )  o Unzip() ) $ Zip(left, right)
    )*/


    val v_inputChannels_0 = Var("inputChannels", RangeUnknown)
    val v_kernelWidthHeight_1 = Var("kernelWidthHeight", RangeUnknown)
    val v_kernelChannels_2 = Var("kernelChannels", RangeUnknown)
    val v_inputWidthHeight_3 = Var("inputWidthHeight", RangeUnknown)
    val v_nInputs_4 = Var("nInputs", RangeUnknown)
    val v_tileStride_5 = Var("tileStride", RangeUnknown)
    val v_kernelStride_6 = Var("kernelStride", RangeUnknown)
    val v_seqOpsPerThread_7 = Var("seqOpsPerThread", RangeUnknown)
    val v_nKernelsPerWrg_8 = Var("nKernelsPerWrg", RangeUnknown)

    val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
    val vectorisableMultAndSumUp = UserFun("vectorisableMultAndSumUp", Array("acc", "l", "r"), """|{ return acc + (l * r); }""".stripMargin, Seq(Float, Float, Float), Float)
    val f = fun(
      //kernel, 3*3*3*2
      ArrayType(ArrayType(ArrayType(ArrayType(Float, v_inputChannels_0), v_kernelWidthHeight_1), v_kernelWidthHeight_1), v_kernelChannels_2),
      //input, 1*2*8*8*2
      ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float, v_inputChannels_0), v_inputWidthHeight_3), v_inputWidthHeight_3), v_nInputs_4), 1),
      (p_0, p_1) => FunCall(
        MapWrg(2)(fun((p_2) => FunCall(MapWrg(1)(fun((p_3) => FunCall(MapWrg(0)(fun((p_4) => FunCall(MapLcl(2)(fun((p_5) => FunCall(MapLcl(1)(fun((p_6) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_7) => FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(id, p_9))), p_8))), FunCall(ReduceSeq(fun((p_10, p_11) => FunCall(vectorisableMultAndSumUp, p_10, FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), FunCall(toPrivate(fun((p_12) => FunCall(id, p_12))), Value("0.0f", Float)), FunCall(Zip(2), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))))), FunCall(Zip(2), p_6, p_5))))), p_3))), p_4))), FunCall(Split(v_nKernelsPerWrg_8), FunCall(Map(fun((p_13) => FunCall(Split(v_seqOpsPerThread_7), FunCall(Join(), FunCall(Map(fun((p_14) => FunCall(Join(), p_14))), p_13))))), p_0))))), FunCall(Map(fun((p_15) => FunCall(Map(fun((p_16) => FunCall(Split(v_seqOpsPerThread_7), p_16))), p_15))), FunCall(Map(fun((p_17) => FunCall(Map(fun((p_18) => FunCall(Join(), FunCall(Join(), p_18)))), p_17))), FunCall(Map(fun((p_19) => FunCall(Join(), p_19))), FunCall(Join(), FunCall(Map(fun((p_20) => FunCall(Join(), p_20))), FunCall(Join(), FunCall(Map(fun((p_21) => FunCall(Map(fun((p_22) => FunCall(Map(fun((p_23) => FunCall(Map(fun((p_24) => FunCall(Map(fun((p_25) => FunCall(Transpose(), p_25))), FunCall(Slide(v_kernelWidthHeight_1,v_kernelStride_6), FunCall(Map(fun((p_26) => FunCall(Slide(v_kernelWidthHeight_1,v_kernelStride_6), p_26))), p_24))))), p_23))), FunCall(Map(fun((p_27) => FunCall(Transpose(), p_27))), FunCall(Slide((v_kernelWidthHeight_1+v_tileStride_5+(-1*v_kernelStride_6)),v_tileStride_5), FunCall(Map(fun((p_28) => FunCall(Slide((v_kernelWidthHeight_1+v_tileStride_5+(-1*v_kernelStride_6)),v_tileStride_5), p_28))), p_22)))))), p_21))), p_2))))))))))
        , FunCall(Split(Cst(1)), p_1))
    )

    ("mkdir -p " + s"$path" ) !!

    import opencl.executor.Compile
    val opencl_string = Compile(f)
    //GlobalCompiler ! (f, path, List(file))

    import java.io.PrintWriter
    new PrintWriter("/home/lu/Documents/Research/research_original_data/YearlyData/2019/002.ONNX/3.c++_example/1.NaumsExample/3.v3/kernel.cl") { write(opencl_string); close }


    //val actual : String = native_compile_and_run(path, file)
   // val expected : String = "6 6 \n"
   // assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Test
  def test_conv_gpu_integrate(): Unit = {

    val path = s"$common_path/16.conv_gpu_integrate_first_half"
    val file = "libconv_gpu_integrate_first_half.cpp"

    /*
    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) => Iterate(6)( CPUFunc( Unzip() o MapSeq( fun(y => tuple_in_tuple_out.apply(Get(y,0), Get(y,1)) ) ) o Zip(2) )  o Unzip() ) $ Zip(left, right)
    )*/


    val v_inputChannels_0 = Var("inputChannels", RangeUnknown)
    val v_kernelWidthHeight_1 = Var("kernelWidthHeight", RangeUnknown)
    val v_kernelChannels_2 = Var("kernelChannels", RangeUnknown)
    val v_inputWidthHeight_3 = Var("inputWidthHeight", RangeUnknown)
    val v_nInputs_4 = Var("nInputs", RangeUnknown)
    val v_tileStride_5 = Var("tileStride", RangeUnknown)
    val v_kernelStride_6 = Var("kernelStride", RangeUnknown)
    val v_seqOpsPerThread_7 = Var("seqOpsPerThread", RangeUnknown)
    val v_nKernelsPerWrg_8 = Var("nKernelsPerWrg", RangeUnknown)

    val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
    val vectorisableMultAndSumUp = UserFun("vectorisableMultAndSumUp", Array("acc", "l", "r"), """|{ return acc + (l * r); }""".stripMargin, Seq(Float, Float, Float), Float)
    val gpu_fun = fun(
      //kernel, 3*3*3*2
      ArrayType(ArrayType(ArrayType(ArrayType(Float, v_inputChannels_0), v_kernelWidthHeight_1), v_kernelWidthHeight_1), v_kernelChannels_2),
      //input, 1*2*8*8*2
      ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float, v_inputChannels_0), v_inputWidthHeight_3), v_inputWidthHeight_3), v_nInputs_4), 1),
      (p_0, p_1) => FunCall(
        MapWrg(2)(fun((p_2) => FunCall(MapWrg(1)(fun((p_3) => FunCall(MapWrg(0)(fun((p_4) => FunCall(MapLcl(2)(fun((p_5) => FunCall(MapLcl(1)(fun((p_6) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_7) => FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(id, p_9))), p_8))), FunCall(ReduceSeq(fun((p_10, p_11) => FunCall(vectorisableMultAndSumUp, p_10, FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), FunCall(toPrivate(fun((p_12) => FunCall(id, p_12))), Value("0.0f", Float)), FunCall(Zip(2), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))))), FunCall(Zip(2), p_6, p_5))))), p_3))), p_4))), FunCall(Split(v_nKernelsPerWrg_8), FunCall(Map(fun((p_13) => FunCall(Split(v_seqOpsPerThread_7), FunCall(Join(), FunCall(Map(fun((p_14) => FunCall(Join(), p_14))), p_13))))), p_0))))), FunCall(Map(fun((p_15) => FunCall(Map(fun((p_16) => FunCall(Split(v_seqOpsPerThread_7), p_16))), p_15))), FunCall(Map(fun((p_17) => FunCall(Map(fun((p_18) => FunCall(Join(), FunCall(Join(), p_18)))), p_17))), FunCall(Map(fun((p_19) => FunCall(Join(), p_19))), FunCall(Join(), FunCall(Map(fun((p_20) => FunCall(Join(), p_20))), FunCall(Join(), FunCall(Map(fun((p_21) => FunCall(Map(fun((p_22) => FunCall(Map(fun((p_23) => FunCall(Map(fun((p_24) => FunCall(Map(fun((p_25) => FunCall(Transpose(), p_25))), FunCall(Slide(v_kernelWidthHeight_1,v_kernelStride_6), FunCall(Map(fun((p_26) => FunCall(Slide(v_kernelWidthHeight_1,v_kernelStride_6), p_26))), p_24))))), p_23))), FunCall(Map(fun((p_27) => FunCall(Transpose(), p_27))), FunCall(Slide((v_kernelWidthHeight_1+v_tileStride_5+(-1*v_kernelStride_6)),v_tileStride_5), FunCall(Map(fun((p_28) => FunCall(Slide((v_kernelWidthHeight_1+v_tileStride_5+(-1*v_kernelStride_6)),v_tileStride_5), p_28))), p_22)))))), p_21))), p_2))))))))))
        , FunCall(Split(Cst(1)), p_1))
    )


    val whole_fun = fun(
      //kernel, 3*3*3*2
      ArrayType(ArrayType(ArrayType(ArrayType(Float, v_inputChannels_0), v_kernelWidthHeight_1), v_kernelWidthHeight_1), v_kernelChannels_2),
      //input, 1*2*8*8*2
      ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float, v_inputChannels_0), v_inputWidthHeight_3), v_inputWidthHeight_3), v_nInputs_4), 1),
      (p_0, p_1) => ToHost() $ OclFunc(gpu_fun).apply(ToGPU() $ p_0, ToGPU() $ p_1)
    )

    ("mkdir -p " + s"$path" ) !!

    //import opencl.executor.Compile
    //val opencl_string = Compile(f)
    GlobalCompiler ! (whole_fun, path, List(file))

    //import java.io.PrintWriter
    //new PrintWriter("/home/lu/Documents/Research/research_original_data/YearlyData/2019/002.ONNX/3.c++_example/1.NaumsExample/3.v3/kernel.cl") { write(opencl_string); close }


    //val actual : String = native_compile_and_run(path, file)
    // val expected : String = "6 6 \n"
    // assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Test
  def test_conv_gpu_integrate_substited(): Unit = {

    val path = s"$common_path/16.conv_gpu_integrate_first_half"
    val file = "libconv_gpu_integrate_first_half.cpp"

    val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
    val vectorisableMultAndSumUp = UserFun("vectorisableMultAndSumUp", Array("acc", "l", "r"), """|{ return acc + (l * r); }""".stripMargin, Seq(Float, Float, Float), Float)
    val gpu_fun = fun(ArrayType(ArrayType(ArrayType(ArrayType(Float, Cst(2)), Cst(3)), Cst(3)), Cst(3)), ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float, Cst(2)), Cst(8)), Cst(8)), Cst(2)), Cst(1)),(p_0, p_1) => FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(MapWrg(2)(fun((p_2) => FunCall(MapWrg(1)(fun((p_3) => FunCall(MapWrg(0)(fun((p_4) => FunCall(MapLcl(2)(fun((p_5) => FunCall(MapLcl(1)(fun((p_6) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_7) => FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(id, p_9))), p_8))), FunCall(ReduceSeq(fun((p_10, p_11) => FunCall(vectorisableMultAndSumUp, p_10, FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), FunCall(toPrivate(fun((p_12) => FunCall(id, p_12))), Value("0.0f", Float)), FunCall(Zip(2), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))))), FunCall(Zip(2), p_6, p_5))))), p_3))), p_4))), FunCall(Split(Cst(1)), FunCall(Map(fun((p_13) => FunCall(Split(Cst(1)), FunCall(Join(), FunCall(Map(fun((p_14) => FunCall(Join(), p_14))), p_13))))), p_0))))), FunCall(Map(fun((p_15) => FunCall(Map(fun((p_16) => FunCall(Split(Cst(1)), p_16))), p_15))), FunCall(Map(fun((p_17) => FunCall(Map(fun((p_18) => FunCall(Join(), FunCall(Join(), p_18)))), p_17))), FunCall(Map(fun((p_19) => FunCall(Join(), p_19))), FunCall(Join(), FunCall(Map(fun((p_20) => FunCall(Join(), p_20))), FunCall(Join(), FunCall(Map(fun((p_21) => FunCall(Map(fun((p_22) => FunCall(Map(fun((p_23) => FunCall(Map(fun((p_24) => FunCall(Map(fun((p_25) => FunCall(Transpose(), p_25))), FunCall(Slide(Cst(3), Cst(1)), FunCall(Map(fun((p_26) => FunCall(Slide(Cst(3), Cst(1)), p_26))), p_24))))), p_23))), FunCall(Map(fun((p_27) => FunCall(Transpose(), p_27))), FunCall(Slide(Cst(8), Cst(6)), FunCall(Map(fun((p_28) => FunCall(Slide(Cst(8), Cst(6)), p_28))), p_22)))))), p_21))), p_2)))))))))), FunCall(Split(Cst(1)), p_1))))))))

    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    val gpu_fun2 = fun(ArrayType(Float, Cst(3)), ArrayType(Float, Cst(3888)),(p_0, p_1) => FunCall(Map(fun((p_2) => FunCall(Map(fun((p_3) => FunCall(Map(fun((p_4) => FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(Join(), p_6))), FunCall(TransposeW(), p_5)))), p_4)))), p_3))), p_2))), FunCall(Map(fun((p_7) => FunCall(Map(fun((p_8) => FunCall(TransposeW(), FunCall(Map(fun((p_9) => FunCall(TransposeW(), p_9))), p_8)))), p_7))), FunCall(Split(Cst(2)), FunCall(Split(Cst(1)), FunCall(Split(Cst(1)), FunCall(Map(fun((p_10) => FunCall(Map(fun((p_11) => FunCall(Split(Cst(6)), p_11))), p_10))), FunCall(MapWrg(1)(fun((p_12) => FunCall(Join(), FunCall(MapWrg(0)(fun((p_13) => FunCall(MapLcl(1)(fun((p_14) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_15) => FunCall(Map(fun((p_16) => p_16)), FunCall(MapSeq(fun((p_17) => FunCall(toGlobal(fun((p_18) => FunCall(id, p_18))), p_17))), FunCall(ReduceSeq(fun((p_19, p_20) => FunCall(add, p_19, p_20))), FunCall(toPrivate(fun((p_21) => FunCall(id, p_21))), FunCall(Get(1), p_14)), p_15))))), FunCall(Get(0), p_14))))), FunCall(Zip(2), FunCall(Get(0), p_13), FunCall(Get(1), p_13))))), FunCall(Zip(2), p_12, FunCall(Split(Cst(1)), p_0)))))), FunCall(Split(Cst(3)), FunCall(Split(Cst(1)), FunCall(Split(Cst(36)), FunCall(Split(Cst(18)), p_1))))))))))))

    val whole_fun = fun(
      //kernel, 3*3*3*2
      gpu_fun.params(0).t,
      gpu_fun2.params(0).t,
      gpu_fun.params(1).t,

      //ArrayType(ArrayType(ArrayType(ArrayType(Float, v_inputChannels_0), v_kernelWidthHeight_1), v_kernelWidthHeight_1), v_kernelChannels_2),
      //input, 1*2*8*8*2
      //ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float, v_inputChannels_0), v_inputWidthHeight_3), v_inputWidthHeight_3), v_nInputs_4), 1),
      (p_k, p_b, p_x) => ToHost() $ OclFunc(gpu_fun2).apply( ToGPU() $ p_b , OclFunc(gpu_fun).apply(ToGPU() $ p_k, ToGPU() $ p_x))
    )

    ("mkdir -p " + s"$path" ) !!

    //import opencl.executor.Compile
    //val opencl_string = Compile(gpu_fun2)
    GlobalCompiler ! (whole_fun, path, List(file))

    //import java.io.PrintWriter
    //new PrintWriter("/home/lu/Documents/Research/research_original_data/YearlyData/2019/002.ONNX/3.c++_example/1.NaumsExample/3.v3/kernel.cl") { write(opencl_string); close }


    //val actual : String = native_compile_and_run(path, file)
    // val expected : String = "6 6 \n"
    // assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }


  @Test
  def test_conv_gpu_integrate_substited_profiling(): Unit = {

    val path = s"$common_path/17.conv_gpu_integrate_substitued_profiling"
    val file = "libconv_gpu_integrate_substitued_profiling.cpp"

    val id = UserFun("id", Array("x"), """|{ return x; }""".stripMargin, Seq(Float), Float)
    val vectorisableMultAndSumUp = UserFun("vectorisableMultAndSumUp", Array("acc", "l", "r"), """|{ return acc + (l * r); }""".stripMargin, Seq(Float, Float, Float), Float)
    val gpu_fun = fun(ArrayType(ArrayType(ArrayType(ArrayType(Float, Cst(2)), Cst(3)), Cst(3)), Cst(3)), ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float, Cst(2)), Cst(8)), Cst(8)), Cst(2)), Cst(1)),(p_0, p_1) => FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(Join(), FunCall(MapWrg(2)(fun((p_2) => FunCall(MapWrg(1)(fun((p_3) => FunCall(MapWrg(0)(fun((p_4) => FunCall(MapLcl(2)(fun((p_5) => FunCall(MapLcl(1)(fun((p_6) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_7) => FunCall(toGlobal(fun((p_8) => FunCall(MapSeq(fun((p_9) => FunCall(id, p_9))), p_8))), FunCall(ReduceSeq(fun((p_10, p_11) => FunCall(vectorisableMultAndSumUp, p_10, FunCall(Get(0), p_11), FunCall(Get(1), p_11)))), FunCall(toPrivate(fun((p_12) => FunCall(id, p_12))), Value("0.0f", Float)), FunCall(Zip(2), FunCall(Get(0), p_7), FunCall(Get(1), p_7)))))), FunCall(Zip(2), p_6, p_5))))), p_3))), p_4))), FunCall(Split(Cst(1)), FunCall(Map(fun((p_13) => FunCall(Split(Cst(1)), FunCall(Join(), FunCall(Map(fun((p_14) => FunCall(Join(), p_14))), p_13))))), p_0))))), FunCall(Map(fun((p_15) => FunCall(Map(fun((p_16) => FunCall(Split(Cst(1)), p_16))), p_15))), FunCall(Map(fun((p_17) => FunCall(Map(fun((p_18) => FunCall(Join(), FunCall(Join(), p_18)))), p_17))), FunCall(Map(fun((p_19) => FunCall(Join(), p_19))), FunCall(Join(), FunCall(Map(fun((p_20) => FunCall(Join(), p_20))), FunCall(Join(), FunCall(Map(fun((p_21) => FunCall(Map(fun((p_22) => FunCall(Map(fun((p_23) => FunCall(Map(fun((p_24) => FunCall(Map(fun((p_25) => FunCall(Transpose(), p_25))), FunCall(Slide(Cst(3), Cst(1)), FunCall(Map(fun((p_26) => FunCall(Slide(Cst(3), Cst(1)), p_26))), p_24))))), p_23))), FunCall(Map(fun((p_27) => FunCall(Transpose(), p_27))), FunCall(Slide(Cst(8), Cst(6)), FunCall(Map(fun((p_28) => FunCall(Slide(Cst(8), Cst(6)), p_28))), p_22)))))), p_21))), p_2)))))))))), FunCall(Split(Cst(1)), p_1))))))))

    val add = UserFun("add", Array("x", "y"), """|{ return x+y; }""".stripMargin, Seq(Float, Float), Float)
    val gpu_fun2 = fun(ArrayType(Float, Cst(3)), ArrayType(Float, Cst(3888)),(p_0, p_1) => FunCall(Map(fun((p_2) => FunCall(Map(fun((p_3) => FunCall(Map(fun((p_4) => FunCall(Join(), FunCall(Map(fun((p_5) => FunCall(Map(fun((p_6) => FunCall(Join(), p_6))), FunCall(TransposeW(), p_5)))), p_4)))), p_3))), p_2))), FunCall(Map(fun((p_7) => FunCall(Map(fun((p_8) => FunCall(TransposeW(), FunCall(Map(fun((p_9) => FunCall(TransposeW(), p_9))), p_8)))), p_7))), FunCall(Split(Cst(2)), FunCall(Split(Cst(1)), FunCall(Split(Cst(1)), FunCall(Map(fun((p_10) => FunCall(Map(fun((p_11) => FunCall(Split(Cst(6)), p_11))), p_10))), FunCall(MapWrg(1)(fun((p_12) => FunCall(Join(), FunCall(MapWrg(0)(fun((p_13) => FunCall(MapLcl(1)(fun((p_14) => FunCall(Join(), FunCall(MapLcl(0)(fun((p_15) => FunCall(Map(fun((p_16) => p_16)), FunCall(MapSeq(fun((p_17) => FunCall(toGlobal(fun((p_18) => FunCall(id, p_18))), p_17))), FunCall(ReduceSeq(fun((p_19, p_20) => FunCall(add, p_19, p_20))), FunCall(toPrivate(fun((p_21) => FunCall(id, p_21))), FunCall(Get(1), p_14)), p_15))))), FunCall(Get(0), p_14))))), FunCall(Zip(2), FunCall(Get(0), p_13), FunCall(Get(1), p_13))))), FunCall(Zip(2), p_12, FunCall(Split(Cst(1)), p_0)))))), FunCall(Split(Cst(3)), FunCall(Split(Cst(1)), FunCall(Split(Cst(36)), FunCall(Split(Cst(18)), p_1))))))))))))

    val whole_fun = fun(
      //kernel, 3*3*3*2
      gpu_fun.params(0).t,
      gpu_fun2.params(0).t,
      gpu_fun.params(1).t,

      //ArrayType(ArrayType(ArrayType(ArrayType(Float, v_inputChannels_0), v_kernelWidthHeight_1), v_kernelWidthHeight_1), v_kernelChannels_2),
      //input, 1*2*8*8*2
      //ArrayType(ArrayType(ArrayType(ArrayType(ArrayType(Float, v_inputChannels_0), v_inputWidthHeight_3), v_inputWidthHeight_3), v_nInputs_4), 1),
      (p_k, p_b, p_x) => ToHost() $ OclFunc(gpu_fun2, cpu_timer = true, gpu_timer = true).apply( ToGPU() $ p_b , OclFunc(gpu_fun, cpu_timer = true, gpu_timer = true).apply(ToGPU() $ p_k, ToGPU() $ p_x))
    )

    ("mkdir -p " + s"$path" ) !!

    //import opencl.executor.Compile
    //val opencl_string = Compile(gpu_fun2)
    GlobalCompiler ! (whole_fun, path, List(file))

    //import java.io.PrintWriter
    //new PrintWriter("/home/lu/Documents/Research/research_original_data/YearlyData/2019/002.ONNX/3.c++_example/1.NaumsExample/3.v3/kernel.cl") { write(opencl_string); close }


    //val actual : String = native_compile_and_run(path, file)
    // val expected : String = "6 6 \n"
    // assertEquals(expected, actual)

    println("Test case test_slide_hello done!")
  }

  @Test
  def batch_code_generate_for_cases_paper(): Unit = {

    //val path = s"$common_path/99.cases_paper"
    val lambda_path = "/home/lu/Documents/Research/Experiments/Cases19/generated_files/"
    val generated_c_path = "/home/lu/Documents/Research/Experiments/Cases19/generated_c_files/"


    val common_file_name1 = lambda_path + "ConvStencil3DConcreteLambda_0_"
    val common_file_name2 = lambda_path  + "ConvStencil3DConcreteLambda2_0_"

    //val id = 0

    import opencl.executor.Eval
    import exploration.ParameterRewrite.readFromFile

    for {id <- 0 until 1000} {

      val file1 = common_file_name1 + id + ".scala"
      val file2 = common_file_name2 + id + ".scala"



      val gpu_fun: Lambda = Eval(readFromFile(file1))
      val gpu_fun2: Lambda = Eval(readFromFile(file2))


      val whole_fun = fun(
        gpu_fun.params(0).t,
        gpu_fun2.params(0).t,
        gpu_fun.params(1).t,

        //(p_k, p_b, p_x) => ToHost() $ OclFunc(gpu_fun2, cpu_timer = true, gpu_timer = true).apply( ToGPU() $ p_b ,
        //OclFunc(Join() o Join() o Join() o Join() o Join() o gpu_fun, cpu_timer = true, gpu_timer = true).apply(ToGPU() $ p_k, ToGPU() $ p_x))
        (p_k, p_b, p_x) => ToHost() $ OclFunc(gpu_fun2, gpu_timer = true).apply(ToGPU() $ p_b,
          OclFunc(/*Join() o Join() o Join() o Join() o */ Join() o gpu_fun, gpu_timer = true).apply(ToGPU() $ p_k, ToGPU() $ p_x))
      )


      //("mkdir -p " + s"$path" ) !!

      //("mkdir -p " + s"$generated_c_path" ) !!

      val path_with_config = generated_c_path + id
      ("mkdir -p " + s"$path_with_config") !!
      val file_with_config = "libhost.cpp"

      println("[Log]: compiling for "+path_with_config)

      GlobalCompiler ! (whole_fun, path_with_config, List(file_with_config))

    }


    println("Test done!")

  }



}
