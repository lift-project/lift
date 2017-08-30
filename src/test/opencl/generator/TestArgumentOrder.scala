package opencl.generator

import ir.ArrayTypeWSWC
import ir.ast.{Split, UserFun, fun}
import lift.arithmetic.SizeVar
import opencl.ir.Int
import opencl.ir.pattern.{MapAtomWrg, MapLcl}
import org.junit.Assert.assertEquals
import org.junit.Test
import utils.{OutputKernelJSON, TestOutputKernelJSON}

class TestArgumentOrder {

  /**
    * This test will fail if the order of arguments changes.
    * It uses the kernel -> JSON mechanism from OutputKernelJSON to dump the generated kernel and compare it to a
    * gold JSON string.
    */
  @Test def ARGUMENT_ORDER_MAP_ATOMIC_WG(): Unit = {
    // Taken from opencl.generator.TestDynMap.ATOM_WRG_MAP
    val splitSize = Math.pow(2, 5).toInt
    val idIterate = UserFun("idIterate", "x", """
                                              int y = x;
                                              for(int i = 0;i<x;i++){
                                                y = y + 1;
                                              }
                                              return y;
                                              """.stripMargin, Int, Int)

    val lambda = fun(ArrayTypeWSWC(Int, SizeVar("N")),
      (array) => MapAtomWrg(MapLcl(idIterate)) o Split(splitSize) $ array
    )

    val json = OutputKernelJSON.getJsonString(lambda)
    val goldJSON = "{\"parameters\" : {\"int* v__8\" : \"(4*v_N_0)\"}, \"outputs\" : {\"int* v__10\" : \"(4*v_N_0)\"}, \"temporary buffers\" : {\"int* v__9\" : \"4\"}, \"sizes\" : {\"int v_N_0\" : \"4\"}}"

    assertEquals(
      TestOutputKernelJSON.sanitiseData(goldJSON).mkString,
      TestOutputKernelJSON.sanitiseData(json).mkString)
  }

}
