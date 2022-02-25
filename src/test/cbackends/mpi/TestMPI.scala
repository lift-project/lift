package cbackends.mpi

import cbackends.mpi.mpi_ir.BcastMPI
import cbackends.mpi.MPICompiler
import ir.ArrayType
import ir.ast.{Lambda, fun}
import lift.arithmetic.SizeVar
import opencl.ir.pattern.MapSeq
import org.junit.{Ignore, Test}
import org.junit.Assert._
import opencl.ir.{Float, add, _}

class TestMPI {

  //TODO: Quick & dirty workaround to the problem of different folders for different developers. A better way is needed.
  val common_path = {
    if (System.getProperty("user.name").equals("eduardo")) {
      "/home/eduardo/lift_bueno/lift/src/test/cbackends/mpi" }
    else {
      System.getProperty("user.dir") + "/src/test/cbackends/mpi" }
  }
  private val N = SizeVar("N")

  val incrementF = fun(Float, x => add(Float).apply(1f, x))

  /*
  private def native_compile_and_run(f: Lambda, path: String, file: String, processes: Option[Int] = None)  : String = {

    var outputName: String = s"a.out"

    //compile_native(path, "main.cpp")
    BuildHost(f, path, "main.cpp", s"$path/$outputName")

    //val status_code = (s"rm $path" + "/" + s"$file") !
    val status_code = 0

    assert(status_code == 0, "Delete generated lib file error!")

    val result = RunHost(f, path, outputName, processes)

    val status_code2 = (s"rm $path" + s"/$outputName") !

    assert(status_code2 == 0, "Delete generated lib file error!")

    result

  }*/


  @Ignore
  @Test
  def test_mpi_bcast(): Unit = {

    val path = s"$common_path/01.Broadcast"
    val file = "broadcast.cpp"

    val basicMPIBcast = fun(
      ArrayType(Float, N),
      (input) => {BcastMPI() $ input}
    )


    MPICompiler ! (basicMPIBcast, path, List(file) )

    //val actual : String = native_compile_and_run(basicMPIBcast, path, file, Some(2))
    //val expected : String = "0.000000 1.000000 2.000000 3.000000 4.000000 5.000000 6.000000 7.000000 8.000000 9.000000 10.000000 11.000000 12.000000 13.000000 14.000000 15.000000 \n0.000000 1.000000 2.000000 3.000000 4.000000 5.000000 6.000000 7.000000 8.000000 9.000000 10.000000 11.000000 12.000000 13.000000 14.000000 15.000000 \n"
    //assertEquals(expected, actual)

    println("Test case MPI_BCAST done!")
  }

}
