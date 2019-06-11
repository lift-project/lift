package cbackends.sdh


//combine general IR and backend IR by multiple imports
import cbackends.sdh.sdh_ir._
import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import ir.ast.{ArrayAccess, Get, Join, Slide, Split, Transpose, UserFun, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.ir.pattern.{MapSeq, ReduceSeq, ScanSeq}
import opencl.ir.{Float, add, _}
import org.junit.Test

import scala.sys.process._

class TestSDH {

  val common_path = System.getProperty("user.dir") + "/src/test/cbackends/sdh"

  private def incrementF = fun(Float, x => add(Float).apply(1f, x))

  val N = SizeVar("N")
  val M = SizeVar("M")
  val O = SizeVar("O")
  val K = SizeVar("K")


  /*

  @Test
  def test_vec_scale(): Unit = {

    val path = System.getProperty("user.dir") + "/src/test/sdh/1.vector_scale"

    val f = fun(
      ArrayType(Float, N),
      in => ToLCP() o Join() o  MapTile( Join() o MapGPESync() o MapGPE( TMKernel( MapSeq(incrementF) ) ) o Split(2)  ) o Split(8) o  ToGPE()  $ in
    )
    CompileSDH(f, path)
    //test()
  }

  @Test
  def test_vec_add(): Unit = {

    val path = System.getProperty("user.dir") + "/src/test/sdh/2.vector_add"


    val add2 = UserFun("add", Array("l", "r"),
      "{ return (l + r); }",
      Seq(Float, Float), Float)


    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) => ToLCP() o Join() o  MapTile( Join() o MapGPESync() o
        MapGPE( TMKernel( MapSeq( fun(y => add2.apply(Get(y, 0), Get(y, 1))) ) ) ) o Split(2)  ) o Split(8) o  ToGPE()  $ Zip(left, right)
    )
    CompileSDH(f, path)
    //test()
  }
  */

  @Test
  def test_vec_add_multi_tile(): Unit = {

    val path = s"$common_path/01.vector_add_multi_tile"
    val sched_file = "lib_sched.cpp"
    val worker_file = "test_worker.cpp"


    val add2 = UserFun("add", Array("l", "r"),
      "{ return (l + r); }",
      Seq(Float, Float), Float)


    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),


      (left, right) => ToLCP() o Join() o MapTM( Join() o MapTile( Join() o MapGPESync() o
        MapGPE( TMKernel( MapSeq( fun(y => add2.apply(Get(y, 0), Get(y, 1))) ) ) ) o Split(2) ) o Split(8) ) o Split(16) o ToGPE() $ Zip(left, right)

    )

    SDHCompiler ! (f, path, List(sched_file, worker_file))

  }

  @Test
  def test_matrix_mul_multi_tile_naive(): Unit = {

    //Naive version: the matrix shape has to be 2 x C and C x 4,
    //               to match the 2 tiles and 4 GPEs in a tile

    val path = s"$common_path/02.matrix_mul_multi_tile_naive"
    val sched_file = "test_lift_matrixmul_sched_lib.hpp"
    val worker_file = "test_lift_matrixmul_kernel.cpp"

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    //only split on A, which is wrong, as it leads to one extra loop on B anyway, thus each thread produce more elements as expected
    /*val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) =>
        ToLCP() o Join() o
          MapTM(
            Join() o MapTile(
                      Join() o MapGPESync() o MapGPE( TMKernel(
                        MapSeq(
                          fun( Arow => Join() o
                               MapSeq( fun( Bcol => ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f) $ Zip(Arow, Bcol) ) ) $ B )
                          )
                                    ) ) o Split(2)
                 ) o Split(8)
               ) o Split(16) o ToGPE() $ A
      )*/

    //now the improved version:
    // * MapTM is only a preparation step for A
    // * MapTile map over A
    // * MapGPE map over B
    // * then only a reduce is needed in the kernel, which is executed by each GPE.
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) =>
        ToLCP() o Join() o MapTM(
          Join() o MapTile( fun(Arow =>
            MapGPESync() o MapGPE( TMKernel(
              fun(Bcol => ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(Arow, Bcol) )
            )) $ B )
          ) ) o Split(2) o ToGPE() $ A
    )

    SDHCompiler ! (f, path, List(sched_file, worker_file))

  }

  @Test
  def test_matrix_mul_multi_tile_multiples_of_4_for_B(): Unit = {

    val path = s"$common_path/03.matrix_mul_multi_tile_multiples_of_4_for_B"
    val sched_file = "test_lift_matrixmul_sched_lib.hpp"
    val worker_file = "test_lift_matrixmul_kernel.cpp"

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) =>
        ToLCP() o Join() o MapTM(
          Join() o MapTile( fun( Arow =>
              MapGPE( TMKernel(
              fun(Bcol => ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(Arow, Bcol) )
            )) $ B )
          ) ) o Split(2) o ToGPE() $ A
    )

    SDHCompiler ! (f, path, List(sched_file, worker_file))

  }

  @Test
  def test_matrix_mul_sdh_demo(): Unit = {

    val path = s"$common_path/04.matrix_mul_abitrary_size_for_A_B"
    val sched_file = "test_lift_matrixmul_sched_lib.hpp"
    val worker_file = "test_lift_matrixmul_kernel.cpp"

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) =>
        ToLCP() o MapTile( fun( Arow =>
            MapGPE( TMKernel(
              fun(Bcol => ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(Arow, Bcol) )
            )) $ B )
          )  o ToGPE() $ A
    )

    SDHCompiler ! (f, path, List(sched_file, worker_file), "matrixmul")

    (s"$path/sdh_demo.sh" ) !

    println("done")
  }

  val add2 = UserFun("add", Array("l", "r"),
    "{ return (l + r); }",
    Seq(Double, Double), Double)

  val diff2 = UserFun("diff2", Array("l", "r"),
    "{ return (r - l); }",
    Seq(Double, Double), Double)

  val cross_calc = UserFun("cross_calc", Array("a1","a2","a3","b1", "b2", "b3"),
    "{ return {a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1 };}",
    Seq(Double, Double, Double, Double, Double, Double), TupleType(Double,Double,Double) )

  val trapz = UserFun("trapz", Array("x1", "x2", "y1", "y2"),
    "{ return (x2-x1)*(y2+y1)/2.0f; }",
    Seq(Double, Double, Double, Double), Double
  )

  val sin = UserFun("sin_uf", Array("x"),
    "{ return sin(x); }",
    Seq(Double), Double)

  val cos = UserFun("cos_uf", Array("x"),
    "{ return cos(x); }",
    Seq(Double), Double)

  val tan = UserFun("tan_uf", Array("x"),
    "{ return tan(x); }",
    Seq(Double), Double)

  val arcsin = UserFun("arcsin_uf", Array("x"),
    "{ return asin(x); }",
    Seq(Double), Double)

  val arccos = UserFun("arccos_uf", Array("x"),
    "{ return acos(x); }",
    Seq(Double), Double)

  val arctan = UserFun("arctan_uf", Array("x"),
    "{ return atan(x); }",
    Seq(Double), Double)

  val div = UserFun("div_uf", Array("x", "y"),
    "{ return (x)/(y); }",
    Seq(Double, Double), Double)

  val hypot = UserFun("hypot_uf", Array("x", "y"),
    "{ return sqrt((x*x)+(y*y)); }",
    Seq(Double, Double), Double)

  //Degree = radian * 180/π
  //radian = degree * π/180

  val degrees = UserFun("r2d_uf", Array("x"),
    "{ return x*180/M_PI; }",
    Seq(Double), Double)

  val radians = UserFun("d2r_uf", Array("x"),
    "{ return x*M_PI/180; }",
    Seq(Double), Double)


  val sinh = UserFun("sinh_uf", Array("x"),
    "{ return sinh(x); }",
    Seq(Double), Double)


  val cosh = UserFun("cosh_uf", Array("x"),
    "{ return cosh(x); }",
    Seq(Double), Double)

  val tanh = UserFun("tanh_uf", Array("x"),
    "{ return tanh(x); }",
    Seq(Double), Double)

  val arcsinh = UserFun("arcsinh_uf", Array("x"),
    "{ return asinh(x); }",
    Seq(Double), Double)

  val arccosh = UserFun("arccosh_uf", Array("x"),
    "{ return acosh(x); }",
    Seq(Double), Double)

  val arctanh = UserFun("arctanh_uf", Array("x"),
    "{ return atanh(x); }",
    Seq(Double), Double)

  val around = UserFun("round_uf", Array("x"),
    "return ( ((int) ceil(x)) % 2 == 0 ? ceil(x) : ceil(x) -1) ;",
    Seq(Double), Double)

  val rint = UserFun("rint_uf", Array("x"),
    "return round(x) ;",
    Seq(Double), Double)

  val fix = UserFun("fix_uf", Array("x"),
    "return trunc(x) ;",
    Seq(Double), Double)

  val floor = UserFun("floor_uf", Array("x"),
    "return floor(x);",
    Seq(Double), Double)

  val ceil = UserFun("ceil_uf", Array("x"),
    "return ceil(x);",
    Seq(Double), Double)

  val trunc = UserFun("trunc_uf", Array("x"),
    "return trunc(x);",
    Seq(Double), Double)

  val prod2 = UserFun("prod2_uf", Array("l", "r"),
    "{ return (l * r); }",
    Seq(Double, Double), Double)

  val gradient2 = UserFun("grad2_uf", Array("l", "r"),
    "{ return (l - r)/2.0f; }",
    Seq(Double, Double), Double)

  val exp = UserFun("exp_uf", Array("x"),
    "return exp(x) ;",
    Seq(Double), Double)

  val expm1 = UserFun("expm1_uf", Array("x"),
    "return exp(x) - 1 ;",
    Seq(Double), Double)

  val exp2 = UserFun("exp2_uf", Array("x"),
    "return pow(2,x) ;",
    Seq(Double), Double)

  val log = UserFun("log_uf", Array("x"),
    "return log(x) ;",
    Seq(Double), Double)

  val log10 = UserFun("log10_uf", Array("x"),
    "return log10(x) ;",
    Seq(Double), Double)

  val log2 = UserFun("log2_uf", Array("x"),
    "return log2(x) ;",
    Seq(Double), Double)

  val log1p = UserFun("log1p_uf", Array("x"),
    "return log(1+x) ;",
    Seq(Double), Double)

  val logaddexp = UserFun("logaddexp_uf", Array("x1", "x2"),
    "{ return log(exp(x1) + exp(x2)); }",
    Seq(Double, Double), Double)

  val logaddexp2 = UserFun("logaddexp2_uf", Array("x1", "x2"),
    "{ return log2(pow(2,x1) + pow(2,x2)); }",
    Seq(Double, Double), Double)

  val sinc = UserFun("sinc_uf", Array("x"),
    "return sin(M_PI*x)/(M_PI*x) ;",
    Seq(Double), Double)

  val signbit = UserFun("signbit_uf", Array("x"),
    "return x<0? 1:0 ;",
    Seq(Double), Double)

  val copysign = UserFun("copysign_uf", Array("x", "y"),
    "return y<0? x*(-1):x ;",
    Seq(Double, Double), Double)

  val frexp = UserFun("frexp_uf", Array("x"),
    "int exp; return {frexp(x,&exp), exp} ;",
    Seq(Double), TupleType(Double, Double) )

  val ldexp = UserFun("ldexp_uf", Array("x", "y"),
    "return x* pow(2,y) ;",
    Seq(Double, Double), Double)

  val nextafter = UserFun("nextafter_uf", Array("x", "y"),
    "return x<y? x+ std::numeric_limits<float>::epsilon() : (x>y? x - std::numeric_limits<float>::epsilon() : x)   ;",
    Seq(Double, Double), Double)

  val reciprocal = UserFun("reciprocal_uf", Array("x"),
    "return 1.0f/x",
    Seq(Double), Double)

  val negative = UserFun("negative_uf", Array("x"),
    "return (-1.0f)*x",
    Seq(Double), Double)

  val multiply = UserFun("multiply_uf", Array("x", "y"),
    "return x * y;",
    Seq(Double, Double), Double)

  val divide = UserFun("divide_uf", Array("x", "y"),
    "return x / y;",
    Seq(Double, Double), Double)

  val power = UserFun("power_uf", Array("x", "y"),
    "return pow(x, y);",
    Seq(Double, Double), Double)

  val subsract = UserFun("subtract_uf", Array("x", "y"),
    "return x - y;",
    Seq(Double, Double), Double)

  val floor_div = UserFun("floor_div_uf", Array("x", "y"),
    "return floor(x/y);",
    Seq(Double, Double), Double)

  val fmod = UserFun("fmod_uf", Array("x", "y"),
    "return ((int)x) % ((int)y);",
    Seq(Double, Double), Double)

  val modf = UserFun("modf_uf", Array("x"),
    "if(x>=0) return { x - floor(x), floor(x) }; else return  { x - round(x), round(x)} ;",
    Seq(Double), TupleType(Double, Double))

  //TODO: may need a closer look at this one, if x and y are not the same sign
  val remainder = UserFun("remainder_uf", Array("x", "y"),
    "if(x>=0) return x - floor(x/y)*y; else return x - round(x/y)*y",
    //"return int(x) % int(y)",
    Seq(Double, Double), Double)

  val divmod = UserFun("divmod_uf", Array("x", "y"),
    "return {int(x/y), x>=0? x - floor(x/y)*y : x - round(x/y)*y};",
    Seq(Double, Double), TupleType(Double, Double) )

  val angle_radian = UserFun("angle_randian_uf", Array("x", "y"),
    "{ return atan2(y,x); }",
    Seq(Double, Double), Double)

  val angle_degree = UserFun("angle_degree_uf", Array("x", "y"),
    "{ return atan2(y,x) * 180 / M_PI; }",
    Seq(Double, Double), Double)

  val real = UserFun("real_uf", Array("x", "y"),
    "{ return x; }",
    Seq(Double, Double), Double)

  val imag = UserFun("imag_uf", Array("x", "y"),
    "{ return y; }",
    Seq(Double, Double), Double)

  val conj = UserFun("conj_uf", Array("x", "y"),
    "{ return {x, (-1.0f)*y}; }",
    Seq(Double, Double), TupleType(Double, Double) )

  val sqrt = UserFun("sqrt_uf", Array("x"),
    "{ return sqrt(x); }",
    Seq(Double), Double)

  val cbrt = UserFun("cbrt_uf", Array("x"),
    "{ return cbrt(x); }",
    Seq(Double), Double)

  val square = UserFun("square_uf", Array("x"),
    "{ return x*x; }",
    Seq(Double), Double)

  val absolute = UserFun("absolute_uf", Array("x"),
    "{ return x>=0? x : x * (-1.0); }",
    Seq(Double), Double)

  val fabs = UserFun("fabs_uf", Array("x"),
    "{ return x>=0? x : x * (-1.0); }",
    Seq(Double), Double)

  val sign = UserFun("sign_uf", Array("x"),
    "{ return x==0? 0: ( x< 0 ? -1 : 1 ); }",
    Seq(Double), Double)

  //val heaviside

  val maximum = UserFun("maximum_uf", Array("x", "y"),
    "{ return max(x,y); }",
    Seq(Double, Double), Double)

  val minimum = UserFun("minimum_uf", Array("x", "y"),
    "{ return min(x,y); }",
    Seq(Double, Double), Double)

  val fmax = UserFun("fmax_uf", Array("x", "y"),
    "{ return max(x,y); }",
    Seq(Double, Double), Double)

  val fmin = UserFun("fmin_uf", Array("x", "y"),
    "{ return min(x,y); }",
    Seq(Double, Double), Double)

  val id_double = UserFun("id_double_uf", Array("x"),
    "{ return x; }",
    Seq(Double), Double)

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
    Seq(Double, Double, Double, Double, Double, Double), Double)



  @Test
  def test_generate_all_numpy_functions(): Unit = {

    val path = s"$common_path/05.lift_numpy"

    val func_names = List(
      "lift_sin" , "cos", "tan", "arcsin", "arccos", "arctan" /*, "hypot", "arctan2"*/, "degrees", "radians", "deg2rad", "rad2deg",
      "sinh", "cosh", "tanh", "arcsinh", "arccosh", "arctanh",
      "around", "round_", "rint", "fix", "lift_floor", "ceil", "trunc",/*
      "prod", "sum", "sum_axis_0", "sum_axis_1",  "nanprod", "nansum", "cumprod", "cumsum", "nancumprod", "nancumsum", "diff", "ediff1d", "gradient", "cross", "trapz",
      */"lift_exp", "expm1", "exp2", "lift_log", "lift_log10", "lift_log2", "log1p",/* "logaddexp", "logaddexp2",*/
      "sinc",
      "signbit"/*, "copysign", "lift_frexp", "ldexp", "nextafter",

      "add"*/, "reciprocal", "positive", "negative"/*, "multiply", "divide", "power", "subtract", "true_divide", "floor_divide", "float_power",
      "fmod", "mod", "modf", "lift_remainder", "divmod",

      "angle_radian", "angle_degree", "real", "imag", "conj",

      "convolve"*/, //"clip"
      "sqrt", "cbrt", "square", "absolute", "fabs", "sign"/*, "maximum", "minimum", "fmax", "fmin", "interp" */
    )

    //val files = func_names.map("lib" + _ + ".cpp")

    val array = ArrayType(Double, N)
    val array_m = ArrayType(Double, M)
    val array_2d = ArrayType(ArrayType(Double, N), M)
    val array_t2 = ArrayType(TupleType(Double, Double) , N)
    val array_t3 = ArrayType(TupleType(Double, Double, Double) , N)

    //val sin_f = fun( array, MapSeq( sin ) $ _ )
    val sin_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(sin)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val cos_f = fun( array, MapSeq( cos ) $ _ )
    val cos_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(cos)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val tan_f = fun( array, MapSeq( tan ) $ _ )
    val tan_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(tan)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val arcsin_f = fun( array, MapSeq( arcsin ) $ _ )
    val arcsin_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(arcsin)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val arccos_f = fun( array, MapSeq( arccos ) $ _ )
    val arccos_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(arccos)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val arctan_f = fun( array, MapSeq( arctan ) $ _ )
    val arctan_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(arctan)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    val hypot_f = fun( array, array, MapSeq( fun( y => hypot.apply(Get(y,0), Get(y,1)) )  ) $ Zip(_,_) )
    val arctan2_f = fun( array, array, MapSeq( arctan ) o MapSeq( fun( y => div.apply(Get(y,0), Get(y,1)) )  ) $ Zip(_,_) )
    //val degrees_f = fun( array, MapSeq( degrees ) $ _ )
    val degrees_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(degrees)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val radians_f = fun( array, MapSeq( radians ) $ _ )
    val radians_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(radians)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val unwrap_f
    val deg2rad_f = radians_f
    val rad2deg_f = degrees_f

    //val sinh_f = fun( array, MapSeq(sinh) $ _ )
    val sinh_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(sinh)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val cosh_f = fun( array, MapSeq(cosh) $ _ )
    val cosh_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(cosh)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val tanh_f = fun( array, MapSeq(tanh) $ _ )
    val tanh_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(tanh)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val arcsinh_f = fun( array, MapSeq(arcsinh) $ _ )
    val arcsinh_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(arcsinh)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val arccosh_f = fun( array, MapSeq(arccosh) $ _ )
    val arccosh_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(arccosh)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val arctanh_f = fun( array, MapSeq(arctanh) $ _ )
    val arctanh_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(arctanh)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )

    //val around_f = fun( array, MapSeq(around) $ _ )
    val around_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(around)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    val round__f = around_f
    //val rint_f = fun( array, MapSeq(rint) $ _ )
    val rint_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(rint)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val fix_f = fun( array, MapSeq(fix) $ _ )
    val fix_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(fix)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val floor_f = fun( array, MapSeq(floor) $ _ )
    val floor_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(floor)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val ceil_f = fun( array, MapSeq(ceil) $ _ )
    val ceil_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(ceil)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val trunc_f = fun( array, MapSeq(trunc) $ _ )
    val trunc_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(trunc)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )

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

    //val exp_f = fun( array, MapSeq(exp) $ _ )
    val exp_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(exp)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val expm1_f = fun( array, MapSeq(expm1) $ _ )
    val expm1_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(expm1)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val exp2_f = fun( array, MapSeq(exp2) $ _ )
    val exp2_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(exp2)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val log_f = fun( array, MapSeq(log) $ _ )
    val log_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(log)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val log10_f = fun( array, MapSeq(log10) $ _ )
    val log10_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(log10)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val log2_f = fun( array, MapSeq(log2) $ _ )
    val log2_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(log2)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val log1p_f = fun( array, MapSeq(log1p) $ _ )
    val log1p_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(log1p)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    val logaddexp_f = fun( array, array, (A,B) => MapSeq( fun(y => logaddexp.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val logaddexp2_f = fun( array, array, (A,B) => MapSeq( fun(y => logaddexp2.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    val nextafter_f = fun( array, array, (A,B) => MapSeq( fun(y => nextafter.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )

    //TODO: i0, can not find its math def
    //val sinc_f = fun( array, MapSeq(sinc) $ _ )
    val sinc_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(sinc)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )

    //val signbit_f = fun( array, MapSeq(signbit) $ _ )
    val signbit_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(signbit)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    val copysign_f = fun( array, array, (A,B) => MapSeq( fun(y => copysign.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    //uf return tuple, not simple map
    val frexp_f = fun( array, MapSeq(frexp) $ _ )
    //val frexp_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(frexp)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    val ldexp_f = fun( array, array, (A,B) => MapSeq( fun(y => ldexp.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    //TODO: can not find its math def
    //val spacing_f =

    val add_f = fun( array, array, (A,B) => MapSeq( fun(y => add2.apply(Get(y, 0), Get(y,1))) ) $ Zip(A,B) )
    //val reciprocal_f = fun( array, MapSeq(reciprocal) $ _ )
    val reciprocal_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(reciprocal)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val positive_f = fun( array, MapSeq(id) $ _ )
    val positive_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(id_double)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val negative_f = fun( array, MapSeq(negative) $ _ )
    val negative_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(negative)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
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
    //return tuple, not simple map
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
    //val sqrt_f = fun( array, MapSeq(sqrt) $ _ )
    val sqrt_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(sqrt)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val cbrt_f = fun( array, MapSeq(cbrt) $ _ )
    val cbrt_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(cbrt)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val square_f = fun( array, MapSeq(square) $ _ )
    val square_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(square)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    //val absolute_f = fun( array, MapSeq(absolute) $ _ )
    val absolute_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(absolute)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
    val fabs_f = absolute_f
    //val sign_f = fun( array, MapSeq(sign) $ _ )
    val sign_f = fun( array, ToLCP() o MapTile( Join() o MapGPE( TMKernel(MapSeq(sign)) ) o Split(2) ) o Split(8) o ToGPE() $ _ )
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
      sin_f, cos_f, tan_f , arcsin_f, arccos_f, arctan_f/*, hypot_f, arctan2_f*/, degrees_f, radians_f, deg2rad_f, rad2deg_f,
      sinh_f, cosh_f, tanh_f, arcsinh_f, arccosh_f, arctanh_f,
      around_f, round__f, rint_f, fix_f, floor_f, ceil_f, trunc_f,/*
      prod_f, sum_f, sum_axis_0_f, sum_axis_1_f, nanprod_f, nansum_f, cumprod_f, cumsum_f, nancumprod_f, nancumsum_f, diff_f, ediff1d_f, gradient_f, cross_f, trapz_f,
      */exp_f, expm1_f, exp2_f, log_f, log10_f, log2_f, log1p_f,/* logaddexp_f, logaddexp2_f, */
      sinc_f,
      signbit_f,/* copysign_f, frexp_f, ldexp_f, nextafter_f,

      add_f,*/ reciprocal_f, positive_f, negative_f/*, multiply_f, divide_f, power_f, subtract_f, true_divide, floor_divide, float_power_f,
      fmod_f, mod_f, modf_f, remainder_f, divmod_f,

      angle_radian_f, angle_degree_f, real_f, imag_f, conj_f,

      convolve_f, //clip_f*/,
      sqrt_f, cbrt_f, square_f, absolute_f, fabs_f, sign_f/*, maximum_f, minimum_f, fmax_f, fmin_f, interp_f */
    )

    (s"mkdir -p $path") !

    (func_names zip all_funcs).foreach {
      case (func_name, func) => SDHCompiler ! (func, path,
        List( "test_lift_" + func_name + "_sched_lib.hpp" ,
              "test_lift_" + func_name + "_kernel.cpp"), func_name)
    }


    println("Done")

  }




  //( "gedit " + s"$path/$sched_file #&"  ) !
  //val process = Process( "gedit " + s"$path/$sched_file #&" ).lines

  //( "gedit " + s"$path/$sched_file #&; " +  s" gedit $path/$worker_file"  ) !!
  //( s" gedit $path/$worker_file"  ) !!

  /*
  val thread = new Thread {
      override def run {
          // your custom behavior here
        ( "gedit " + s"$path/$sched_file"  ) !!
      }
  }
  thread.start


  Thread.sleep(50)
  */

  @Test
  def test_numpy_sum(): Unit = {

    val path = s"$common_path/05.numpy_sum"
    val sched_file = "test_lift_numpy_sum_sched_lib.hpp"
    val worker_file = "test_lift_numpy_sum_kernel.cpp"

    val N = SizeVar("N")

    /*
    val f = fun(
      ArrayTypeWSWC(Float, N),
      A =>
        ToLCP() o MapTile( fun( Arow =>
          MapGPE( TMKernel(
            fun(Bcol => ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(Arow, Bcol) )
          )) $ B )
        )  o ToGPE() $ A
    )*/

    //SDHCompiler ! (f, path, List(sched_file, worker_file))

    println("done")

  }

  @Test
  def test_paralell_reduce(): Unit = {

    val path = s"$common_path/06.parallel_reduce"
    val sched_file = "test_lift_par_reduce_sched_lib.hpp"
    val worker_file = "test_lift_par_reduce_kernel.cpp"

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Double, N),
      LCPSingle(ReduceSeq(add2, 0.0)) o ToLCP() o Join() o Join() o
        MapTile( MapGPE(TMKernel(fun( ReduceSeq(add2, 0.0) $ _ ) ) ) )  o
        Split(4) o Split(N/8) o ToGPE()  $ _
    )

    (s"mkdir -p $path") !

    SDHCompiler ! (f, path, List(sched_file, worker_file), "matrixmul")

    println("done")
  }



}
