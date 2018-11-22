package host

import host.ir_host.MapHSeq
import ir.{ArrayType, ArrayTypeWSWC}
import ir.ast.{Array3DFromUserFunGenerator, Get, Join, Pad, Split, Transpose, TransposeW, UserFun, Zip, fun}
import ir.ast.Pad.Boundary.WrapUnsafe
import lift.arithmetic.SizeVar
import org.junit.Test
import org.junit.Assert._
import opencl.ir.{Float, add, _}

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

    assert(status_code2 == 0, "Delete generated lib file error!")

    result


  }

  @Test
  def test_map(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/01.maphost"
    val file = "libmap.cpp"

    val f = fun( ArrayType(Float, N),
      in => MapHSeq( incrementF ) $ in
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_map done!")

  }

  @Test
  def test_zip(): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/02.zip"
    val file = "libzip.cpp"

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      (left, right) => MapHSeq( fun(y => add2.apply(Get(y,0), Get(y,1)) ) ) $ Zip(left, right)
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
      in => Join() o MapHSeq( MapHSeq(incrementF)  )  o Split(8) $ in
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
      (left, right) => Join() o MapHSeq( MapHSeq( fun(y => add2.apply(Get(y,0), Get(y,1)) ) )  )  o Split(8) $ Zip(left, right)
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
      in => Join() o TransposeW() o MapHSeq( MapHSeq(incrementF)  ) o Transpose() o Split(8) $ in
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
      in =>  MapHSeq(incrementF) o Pad(1, 1, WrapUnsafe)  $ in
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
  def test_array3dfromuserfungenerator (): Unit = {

    val path = "/home/lu/Documents/Research/lift/src/test/host/07.array3dfromuserfungenerator"
    val file = "libarray3dfromuserfungenerator.cpp"

    val type3d = ArrayTypeWSWC( ArrayTypeWSWC( ArrayTypeWSWC(Float, 1), 2), 3)
    val userfun = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ return i+j+k; }", Seq(Int, Int, Int, Int, Int, Int), Int)
    val data = Array3DFromUserFunGenerator( userfun , type3d)

    val f = fun( ArrayTypeWSWC(Float, 60),
      in => MapHSeq(MapHSeq(MapHSeq(incrementF) ) )  $ Zip( Split(3) o Split(2) $ in, data)
    )

    CompileHost(f, path, file)

    val actual : String = native_compile_and_run(path, file)
    val expected : String = "1 1 1 1 1 1 \n"
    assertEquals(expected, actual)

    println("Test case test_array3dfromuserfungenerator done!")

  }

}
