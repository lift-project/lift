package exploration.utils

import java.io.FileWriter
import java.nio.file.{Files, Paths}

import exploration.{HighLevelRewrite, ParameterRewrite}
import rewriting.utils.Utils

import scala.io.Source

/**
  * Created by Toomas Remmelg on 09/05/16.
  */
object RenameLambdas {

  def main(args: Array[String]) {

    val topFolder = if (args.nonEmpty) args(0)
    else "/home/s1042579/Documents/cgo-2016-kernels/mmTransposeA"

    val top = topFolder.split("/").init.mkString("", "/", "/")

    val kernels = Source.fromFile(topFolder + "/index")

    val newTopFolder = topFolder + "NewHash"

    kernels.getLines().foreach(location => {

      try {

        val fullFilename = top + location

        if (Files.exists(Paths.get(fullFilename))) {
          val lambda = HighLevelRewrite.finishRewriting(ParameterRewrite.readLambdaFromFile(fullFilename))

          val stringRep = Utils.dumpLambdaToString(lambda)
          val sha256 = Utils.Sha256Hash(stringRep)

          println(location.split("/").last + ", " + sha256)

          if (Utils.dumpToFile(stringRep, sha256, newTopFolder)) {
            // Add to index if it was unique
            synchronized {
              val idxFile = new FileWriter(newTopFolder + "/index", true)
              idxFile.write(topFolder + "NewHash/" + sha256 + "\n")
              idxFile.close()
            }
          }


        }
      } catch {
        case _: Throwable =>
      }

    })

  }

}
