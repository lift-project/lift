package exploration.utils

import java.io.FileWriter

import lift.arithmetic.{ArithExpr, Var}
import exploration.SplitSlideRewrite
import opencl.executor.Eval
import rewriting.utils.{DumpToFile, Utils}

import scala.io.Source

object RenameLowerLambdas {

  def main(args: Array[String]) {

    val topFolder = if (args.nonEmpty) args(0)
    else "/home/s1042579/Documents/cgo-2016-kernels/mmTransposeANewHash"

    val top = topFolder.split("/").init.mkString("", "/", "/")

    val kernels = Source.fromFile(topFolder + "/index").getLines()

    val newTopFolder = topFolder + "NewHash"

    kernels.foreach(highLevel => {

      val hash = highLevel.split("/").last

      val location = s"/home/s1042579/Documents/cgo-2016-kernels/mmTransposeALowerTemp/$hash/index"

//      println(Files.exists(Paths.get(location)))

      val lowLevelKernels = Source.fromFile(location).getLines()

      lowLevelKernels.foreach(lowLevel => {

        val location = "/home/s1042579/Documents/cgo-2016-kernels/" + lowLevel.replace("mmTransposeALower", "mmTransposeALowerTemp")

        try {

          val factory = Eval.getMethod(SplitSlideRewrite.readFromFile(location).replace("id,", "idfloat,"))

          val lambda = factory(Array[ArithExpr](Var("M"), Var("K"), Var("N"), Var(), Var(), Var(), Var(), Var(), Var(), Var(), Var()))

          val stringRep = DumpToFile.dumpLambdaToMethod(lambda)
          val sha256 = DumpToFile.Sha256Hash(stringRep)

          val folder = "/home/s1042579/Documents/cgo-2016-kernels/" +
              "mmTransposeALowerNewHash/" + hash + "/" + sha256.charAt(0) + "/" +
              sha256.charAt(1)

          if (DumpToFile.dumpToFile(stringRep, sha256, folder)) {
            val idxFile = new FileWriter("/home/s1042579/Documents/cgo-2016-kernels/" +
              "mmTransposeALowerNewHash/" + hash+ "/index", true)
            idxFile.write("mmTransposeALower/" + hash + "/" + sha256.charAt(0) +
              "/" + sha256.charAt(1) + "/" + sha256 + "\n")
            idxFile.close()
            println(lowLevel.split("/").last + "," + sha256)
          }


        } catch {
          case _: Throwable =>
        }

      })


    })



  }

}
