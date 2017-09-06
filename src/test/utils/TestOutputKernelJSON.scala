package utils

import ir.ArrayTypeWSWC
import ir.ast.{Get, Join, Slide3D, Zip, \, fun, _}
import lift.arithmetic.SizeVar
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import org.junit.Assert._
import org.junit._

/**
  *
  * Tests the "OutputKernelJSON function to output parameters of created openCL kernels in a JSON format
  *
 */


object TestOutputKernelJSON
{
    def sanitiseData(str: String): Array[String] =
    {
      val seps = Array[Char](':',',')
      val regExDel = "_[0-9]+".r
      val whitespace = "\\s+"
      val filterValues = "{}\\\"".toSet
      val kernelParamStr = OutputKernelJSON.kernelParamNameString

      val jsonarr = str.split(seps)
      val j2 = jsonarr.flatMap(_.split(whitespace)).map(_.filterNot(filterValues))
      val j3 = j2.filter( x => !x.contains(kernelParamStr) && !x.isEmpty )
      j3.map( x => regExDel.replaceAllIn(x,""))

    }
}
class TestOutputKernelJSON {

  @Test
  def testLambdaWithAllTypesOfParameters(): Unit =
  {

    val compareJson = "{\"parameters\" : {\"float* v__1006\" : \" (4*v_M_1*v_N_0*v_O_2)\", \"float* v__1007\" : \" (4*v_M_1*v_N_0*v_O_2)\", \"float* v__1008\" : \" 108\", \"float* v__1009\" : \" 108\", \"float v__1010\" : \" 4\"}, \"outputs\" : {\"float* v__1055\" : \" (-32+(-8*v_M_1*v_N_0)+(-8*v_M_1*v_O_2)+(-8*v_N_0*v_O_2)+(4*v_M_1*v_N_0*v_O_2)+(16*v_M_1)+(16*v_N_0)+(16*v_O_2))\"}, \"temporary buffers\" : {\"float* v__1029\" : \" (-864+(-216*v_M_1*v_N_0)+(108*v_M_1*v_N_0*v_O_2)+(-216*v_M_1*v_O_2)+(-216*v_N_0*v_O_2)+(432*v_M_1)+(432*v_N_0)+(432*v_O_2))\", \"float* v__1043\" : \" (-864+(-216*v_M_1*v_N_0)+(108*v_M_1*v_N_0*v_O_2)+(-216*v_M_1*v_O_2)+(-216*v_N_0*v_O_2)+(432*v_M_1)+(432*v_N_0)+(432*v_O_2))\", \"float* v__1017\" : \" (-864+(-216*v_M_1*v_N_0)+(108*v_M_1*v_N_0*v_O_2)+(-216*v_M_1*v_O_2)+(-216*v_N_0*v_O_2)+(432*v_M_1)+(432*v_N_0)+(432*v_O_2))\"}, \"sizes\" : {\"int v_M_1\" : \"4\", \"int v_N_0\" : \"4\", \"int v_O_2\" : \"4\"}}"

    val n = SizeVar("N")
    val m = SizeVar("M")
    val o = SizeVar("O")

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)

    val lambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      Float,
      (mat1, mat2, weights, weightsMiddle, c4) => {
        MapGlb(fun((m) =>
          toGlobal(MapSeq(fun(x => mult(x, c4)))) o
            MapSeq(addTuple) $
            Zip(MapSeq(addTuple) $
              Zip(MapSeq(fun(x => mult(x, constantOriginal(2)))) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $
                Zip(Join() $ Get(m, 0), Join() $ weightsMiddle),
                MapSeq(fun(x => mult(x, constantOriginal(0)))) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $
                  Zip(Join() $ Get(m, 1), Join() $ weights)),
              MapSeq(fun(x => mult(x, constantOriginal(1)))) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $
                Zip(Join() $ Get(m, 1), Join() $ weightsMiddle))
        )) $ Zip(Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1), Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2))
      })

      // remove random numbers in parameter names
      val json = TestOutputKernelJSON.sanitiseData(OutputKernelJSON.getJsonString(lambda))
      val sanCompareJson = TestOutputKernelJSON.sanitiseData(compareJson)

      assertEquals(sanCompareJson.mkString, json.mkString)

  }

}


